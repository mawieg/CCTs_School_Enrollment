# This function produces vectors of treatment effects by applying double machine learning, 
# using outcome, treatment status, indicators for missing outcomes, covariates, and ML methods as inputs.
# Author: Martin Wiegand; based on code provided by V.Chernozhukov, D. Chetverikov, 
# M. Demirer, E. Duflo, C. Hansen, W. Newey, J. Robins for their paper "Double/debiased machine 
# learning for treatment and structural parameters", Econometrics Journal (2018).
# Last changed: 23.09.2020

source("ML_Functions.R") 

DML_mis <- function(data, y, d, r, rt, rc, groupvar, xx, xo, xL, methods, nfold, arguments, arguments.ens, outcomename, silent=FALSE){

  K         <- nfold # number of folds
  M         <- length(methods)
  binary.d  <- as.numeric(checkBinary(data[,d]))  # 1 if treatment variable binary 0/1 variable, 0 otherwise 
                                                  # (checkBinary defined in ML_Functions.R)
  
  TE <- TET <- TEN <- matrix(0, dim(data)[1], (M+1))
  MSE1 <- MSE2 <- MSE3 <- MSE4 <- MSE5 <- MSE6 <- MSE7 <- rep(0,M+1)
  cond.comp <- matrix(list(),M+1,K) # #methods*#folds matrix of lists with predicted nuisance functions 
  
  if(outcomename=="pcexp" | outcomename=="lnpcexp") {
    binary.y <- 0
  } else {
    binary.y <- 1
  }
  
  # Create nfold-fold split such that there is no overlap between groups and splits
  data <- as.data.frame(fold(data, k = nfold, cat_col = d, id_col = groupvar, method = 'n_dist'))
  cvgroup <- data$.folds <- as.integer(data$.folds)
  
  for(k in 1:M){   
    
    if(silent==FALSE){
      cat(methods[k],'\n')
    }
    
    if (any(c("RLasso", "PostRLasso", "Ridge", "PostLasso", "Lasso", "Elnet")==methods[k])){
      xvars <- xL  # use large variable set including polynomials and interactions for above methods
    } 
    if (methods[k]=="Boosting"){
      xvars <- xo # boosting can handle missing values
    } 
    if (any(methods[k]==c("Forest", "Nnet", "SVM"))){
      xvars <- xx # forest and nnet can't handle missing values
    }
    
    for(j in 1:K){ 
        
      if(silent==FALSE){
        cat('  fold',j,'\n')
      }
      
      ii  <- cvgroup == j # ii index 1 for leftout group
      nii <- cvgroup != j # nii index 1 for all other groups
        
      datause <- as.data.frame(data[nii,]) # data split used to estimate nuisance parameters (functions m and g)
      dataout <- as.data.frame(data[ii,])  # data split used to estimate treatment parameter (theta)

      # ignore variables with variance 0 in that fold
      d.is.1 <- which(datause[,d]==1)
      Xuse.d0 <- dplyr::select(datause[-d.is.1,], one_of(xvars))
      Xuse.d1 <- dplyr::select(datause[d.is.1,], one_of(xvars))
      d.is.1.r.is.1 <- which(datause[,d]==1 & datause[,r]==1)
      d.is.0.r.is.1 <- which(datause[,d]==0 & datause[,r]==1)
      Xuse.d0.r1 <- dplyr::select(datause[d.is.0.r.is.1,], one_of(xvars))
      Xuse.d1.r1 <- dplyr::select(datause[d.is.1.r.is.1,], one_of(xvars))
        
      var.not.0.d0 <- !unlist(lapply(Xuse.d0.r1, function(x) length(unique(x)) <2)) 
      var.not.0.d1 <- !unlist(lapply(Xuse.d1.r1, function(x) length(unique(x)) <2))
      var.not.0 <- var.not.0.d0 * var.not.0.d1 # indices of variables with non-zero variance for both d=0 and d=1
      form_x <- paste(names(var.not.0[var.not.0==1]), collapse='+') # var1 + var2 + ...
      x <- names(var.not.0[var.not.0==1])
        
      remove(d.is.1, Xuse.d0, Xuse.d1, d.is.1.r.is.1, d.is.0.r.is.1, Xuse.d0.r1, Xuse.d1.r1, var.not.0.d0, var.not.0.d1, var.not.0)
      
        
      cond.comp[[k,j]] <- cond_comp_mis(datause=datause, dataout=dataout, y, d, r, rt, rc, form_x, methods[k], plinear=0, binary.d, binary.y, arguments=arguments)
      MSE1[k] <- MSE1[k] + cond.comp[[k,j]]$err.yz0^2 * length(which(dataout[,d]==0 & dataout[,r]==1)) / length(which(data[,d]==0 & data[,r]==1))  # MSE of g(x,0)
      MSE2[k] <- MSE2[k] + cond.comp[[k,j]]$err.yz1^2 * length(which(dataout[,d]==1 & dataout[,r]==1)) / length(which(data[,d]==1 & data[,r]==1))  # MSE of g(x,1)
      MSE3[k] <- MSE3[k] + cond.comp[[k,j]]$err.z^2 * dim(dataout)[1] / dim(data)[1] # MSE of m(x) (prop. score)
      MSE4[k] <- MSE4[k] + cond.comp[[k,j]]$err.rt^2 * dim(dataout)[1] / dim(data)[1] # MSE of m(x)*r(1,x)
      MSE5[k] <- MSE5[k] + cond.comp[[k,j]]$err.rc^2 * dim(dataout)[1] / dim(data)[1] # MSE of m(x)*r(0,x)
      MSE6[k] <- MSE6[k] + cond.comp[[k,j]]$err.r_t^2 * length(which(dataout[,d]==1)) / length(which(data[,d]==1))  # MSE of r(1,x)  
      MSE7[k] <- MSE7[k] + cond.comp[[k,j]]$err.r_c^2 * length(which(dataout[,d]==0)) / length(which(data[,d]==0))  # MSE of r(0,x)     
        
      cond.comp[[k,j]]$dout <- dataout[,d]  # think I don't need this
      cond.comp[[k,j]]$yout <- dataout[,y]
      cond.comp[[k,j]]$rout <- dataout[,r]
      remove(datause, dataout)
    }
      
    groupvector            <- c()
    mz_x               <- c()    
    my_z1x             <- c()
    my_z0x             <- c()
    m_rt               <- c()    
    m_rc               <- c()    
    m_r_t               <- c()    
    m_r_c               <- c()    
    yout                   <- c()
    dout                   <- c()
    rout                   <- c()
    for (j in 1:K) {
      groupvector <- c(groupvector, rep(j,length(cond.comp[[k,j]]$mz_x)))
      mz_x <- c(mz_x, cond.comp[[k,j]]$mz_x)
      my_z1x <- c(my_z1x, cond.comp[[k,j]]$my_z1x)
      my_z0x <- c(my_z0x, cond.comp[[k,j]]$my_z0x)
      m_rt <- c(m_rt, cond.comp[[k,j]]$m_rt)
      m_rc <- c(m_rc, cond.comp[[k,j]]$m_rc)
      m_r_t <- c(m_r_t, cond.comp[[k,j]]$m_r_t)
      m_r_c <- c(m_r_c, cond.comp[[k,j]]$m_r_c)
      yout <- c(yout, cond.comp[[k,j]]$yout)
      dout <- c(dout, cond.comp[[k,j]]$dout)
      rout <- c(rout, cond.comp[[k,j]]$rout)
    }
    rtout <- dout * rout
    rcout <- (1-dout) * rout
    
  }
  
  # Nuisance parameters for best method
  min1 <- which.min(MSE1[1:M])   # the model with minimal MSE for g(x,0)
  min2 <- which.min(MSE2[1:M])   # the model with minimal MSE for g(x,1)
  min3 <- which.min(MSE3[1:M])   # the model with minimal MSE for m(x)
  min4 <- which.min(MSE4[1:M])   # the model with minimal MSE for m(x)*r(1,x)
  min5 <- which.min(MSE5[1:M])   # the model with minimal MSE for m(x)*r(0,x)
  min6 <- which.min(MSE6[1:M])   # the model with minimal MSE for MSE of r(1,x)
  min7 <- which.min(MSE7[1:M])   # the model with minimal MSE for MSE of r(0,x)

  for (j in 1:K) {
    ii  <- cvgroup == j # ii index 1 for leftout group
    nii <- cvgroup != j # nii index 1 for all other groups
    datause <- as.data.frame(data[nii,]) # data split used to estimate nuisance parameters (functions m and g)
    dataout <- as.data.frame(data[ii,])  # data split used to estimate treatment parameter (theta)
    
    cond.comp[[M+1,j]]$my_z0x <- cond.comp[[min1,j]]$my_z0x
    cond.comp[[M+1,j]]$my_z1x <- cond.comp[[min2,j]]$my_z1x
    cond.comp[[M+1,j]]$mz_x <- cond.comp[[min3,j]]$mz_x
    cond.comp[[M+1,j]]$m_rt <- cond.comp[[min4,j]]$m_rt
    cond.comp[[M+1,j]]$m_rc <- cond.comp[[min5,j]]$m_rc
    cond.comp[[M+1,j]]$m_r_t <- cond.comp[[min6,j]]$m_r_t
    cond.comp[[M+1,j]]$m_r_c <- cond.comp[[min7,j]]$m_r_c
    
    cond.comp[[M+1,j]]$err.yz0 <- cond.comp[[min1,j]]$err.yz0
    cond.comp[[M+1,j]]$err.yz1 <- cond.comp[[min2,j]]$err.yz1
    cond.comp[[M+1,j]]$err.z <- cond.comp[[min3,j]]$err.z
    cond.comp[[M+1,j]]$err.rt <- cond.comp[[min4,j]]$err.rt
    cond.comp[[M+1,j]]$err.rc <- cond.comp[[min5,j]]$err.rc
    cond.comp[[M+1,j]]$err.r_t <- cond.comp[[min6,j]]$err.r_t
    cond.comp[[M+1,j]]$err.r_c <- cond.comp[[min7,j]]$err.r_c
    
    MSE1[M+1] <- MSE1[M+1] + cond.comp[[M+1,j]]$err.yz0^2 * length(which(dataout[,d]==0 & dataout[,r]==1)) / length(which(data[,d]==0 & data[,r]==1))  # MSE of g(x,0)
    MSE2[M+1] <- MSE2[M+1] + cond.comp[[M+1,j]]$err.yz1^2 * length(which(dataout[,d]==1 & dataout[,r]==1)) / length(which(data[,d]==1 & data[,r]==1))  # MSE of g(x,1)
    MSE3[M+1] <- MSE3[M+1] + cond.comp[[M+1,j]]$err.z^2 * dim(dataout)[1] / dim(data)[1] # MSE of m(x) (prop. score)
    MSE4[M+1] <- MSE4[M+1] + cond.comp[[M+1,j]]$err.rt^2 * dim(dataout)[1] / dim(data)[1] # MSE of m(x)*r(1,x)
    MSE5[M+1] <- MSE5[M+1] + cond.comp[[M+1,j]]$err.rc^2 * dim(dataout)[1] / dim(data)[1] # MSE of m(x)*r(0,x)
    MSE6[M+1] <- MSE6[M+1] + cond.comp[[M+1,j]]$err.r_t^2 * length(which(dataout[,d]==1)) / length(which(data[,d]==1))  # MSE of r(1,x)  
    MSE7[M+1] <- MSE7[M+1] + cond.comp[[M+1,j]]$err.r_c^2 * length(which(dataout[,d]==0)) / length(which(data[,d]==0))  # MSE of r(0,x)     
  }
  
  R.squared.y <- R.squared.d <- R.squared.r <- keepvec <- trimvec <- rep(0,(M+1))
  
  for (k in (1:(M+1))) {
    # do optimal trimming
    propscore.m_rt <- c()
    propscore.m_rc <- c()
    for(j in 1:K) {
      propscore.m_rt <- c(propscore.m_rt, cond.comp[[k,j]]$m_rt)
      propscore.m_rc <- c(propscore.m_rc, cond.comp[[k,j]]$m_rc)
    }
    # only consider observations with lower < propensity score < upper
    trim <- optimal.trim_mis(propscore.m_rt, propscore.m_rc)[1]
    
    for(t in 1:3){ # t = ATE, ATET, ATEN
      
      mz_x                   <- c()    
      my_z1x                 <- c()
      my_z0x                 <- c()
      m_rt                   <- c()
      m_rc                   <- c()
      m_r_t                  <- c()
      m_r_c                  <- c()
      yout                   <- c()
      dout                   <- c()
      rout                   <- c()
      groupout               <- c()
      keepout                <- c()
      
      for(j in 1:K) {
        ii  <- cvgroup == j                  # ii index 1 for leftout group
        dataout <- as.data.frame(data[ii,])  # data split used to estimate treatment parameter (theta)
        
        if (t==1) {keep <- c(NA,1)[(cond.comp[[k,j]]$m_rt>trim & cond.comp[[k,j]]$m_rc>trim) + 1]}
        if (t==2) {keep <- c(NA,1)[(cond.comp[[k,j]]$m_r_t>trim & cond.comp[[k,j]]$m_rc>trim) + 1]}
        if (t==3) {keep <- c(NA,1)[(cond.comp[[k,j]]$m_rt>trim & cond.comp[[k,j]]$m_r_c>trim) + 1]}
        
        mz_x <- c(mz_x, cond.comp[[k,j]]$mz_x * keep)         # append propscore for fold j
        my_z1x <- c(my_z1x, cond.comp[[k,j]]$my_z1x * keep)
        my_z0x <- c(my_z0x, cond.comp[[k,j]]$my_z0x * keep)
        m_rt <- c(m_rt, cond.comp[[k,j]]$m_rt * keep)
        m_rc <- c(m_rc, cond.comp[[k,j]]$m_rc * keep)
        m_r_t <- c(m_r_t, cond.comp[[k,j]]$m_r_t * keep)
        m_r_c <- c(m_r_c, cond.comp[[k,j]]$m_r_c * keep)
        yout <- c(yout, dataout[,y]) # some of these are NA already, meaning these outcomes are unobserved
        dout <- c(dout, dataout[,d] * keep)
        rout <- c(rout, dataout[,r] * keep)
        groupout <- c(groupout, dataout[,groupvar])
        keepout <- c(keepout, keep)
      }
      yout[is.na(yout)]      <- 0 # set unobserved values to 0
      pout <- mean(dout, na.rm = TRUE)
      
      if (t == 1) {
        TE[,k]    <- (rout * dout * (yout - my_z1x) / m_rt) -  (rout * (1 - dout) * (yout - my_z0x) / m_rc) + my_z1x - my_z0x;
      }
      if (t == 2) {
        TET[,k]   <- (rout * dout * (yout - my_z1x) / (pout * m_r_t) ) - (rout * (1 - dout) * (yout - my_z0x) * mz_x) / (pout * m_rc) + (dout/pout) * (my_z1x - my_z0x);
      }
      if (t == 3) {
        TEN[,k]   <- (rout * dout * (yout - my_z1x) * (1 - mz_x) / ((1 - pout) * m_rt) ) - (rout * (1 - dout) * (yout - my_z0x)) / ((1 - pout) * m_r_c) + ((1 - dout) / (1 - pout)) * (my_z1x - my_z0x);
      }
      
      # Compute R^2 for outcome
      ybar <- mean(yout[rout==1], na.rm=TRUE)
      total.sum.of.squares.y <- sum((yout[rout==1] - ybar)^2, na.rm = TRUE)
      yhat <- my_z1x * dout + my_z0x * (1 - dout)
      res.sum.of.squares.y <- sum((yout[rout==1] - yhat[rout==1])^2, na.rm = TRUE)
      R.squared.y[k] <- 1 - res.sum.of.squares.y/total.sum.of.squares.y
      
      # Compute R^2 for treatment
      dbar <- mean(dout, na.rm=TRUE)
      total.sum.of.squares.d <- sum((dout - dbar)^2, na.rm = TRUE)
      res.sum.of.squares.d <- sum((dout - mz_x)^2, na.rm = TRUE)
      R.squared.d[k] <- 1 - res.sum.of.squares.d/total.sum.of.squares.d
      
      # Compute R^2 for non-attrition
      rbar <- mean(rout, na.rm=TRUE)
      total.sum.of.squares.r <- sum((rout - dbar)^2, na.rm = TRUE)
      rhat <- m_r_t * dout + m_r_c * (1 - dout)
      res.sum.of.squares.r <- sum((rout - rhat)^2, na.rm = TRUE)
      R.squared.r[k] <- 1 - res.sum.of.squares.r/total.sum.of.squares.r
      
      # Count how many observations were trimmed
      keepvec[k] <- sum(is.na(keepout))
      trimvec[k] <- trim[1]
    }
  }
  
  RMSE <- rbind(sqrt(MSE1), sqrt(MSE2), sqrt(MSE3), sqrt(MSE4), sqrt(MSE5), sqrt(MSE6), sqrt(MSE7))
  rownames(RMSE) <- c("RMSE[Y|X, D=0]", "RMSE[Y|X, D=1]", "RMSE[D|X]", "RMSE[RD|X]", "RMSE[R(1-D)|X]", "RMSE[R|X, D=1]", "RMSE[R|X, D=0]")
  
  result.list <- list(TE, TET, TEN, groupout, RMSE, R.squared.y, R.squared.d, R.squared.r, keepvec, trimvec)
  names(result.list) <- c("TE", "TET", "TEN", "groupID", "RMSE", "R.squared.y", "R.squared.d", "R.squared.r", "keepvec", "trimvec")
  return(result.list)
}  
