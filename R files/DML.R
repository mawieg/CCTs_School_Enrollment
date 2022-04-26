# This function produces vectors of treatment effects by applying double machine 
# learning, using outcome, treatment status, covariates, and ML methods as 
# inputs.
# Author: Martin Wiegand 
# Partially based on code provided by V.Chernozhukov, D. Chetverikov, 
# M. Demirer, E. Duflo, C. Hansen, W. Newey, J. Robins for their paper 
# "Double/debiased machine learning for treatment and structural parameters", 
# Econometrics Journal (2018).
# Last changed: 26.04.2022

DML <- function(data, y, d, groupvar, xx, xo, xL, methods, nfold, arguments, 
                arguments.ens, outcomename, silent=FALSE){

  K         <- nfold # number of folds
  M         <- length(methods)
  # binary.d: 1 if treatment variable binary 0/1 variable, 0 otherwise 
  # (checkBinary defined in ML_Functions.R)
  binary.d  <- as.numeric(checkBinary(data[,d]))  
  TE <- TET <- TEN <- matrix(0, dim(data)[1], (M+1))
  MSE1 <- MSE2 <- MSE3 <- rep(0,M+1)
  # cond.comp: #methods*#folds matrix of lists with predicted nuisance functions 
  cond.comp <- matrix(list(),M+1,K) 
  
  if(outcomename=="pcexp" | outcomename=="lnpcexp") {
    binary.y <- 0
  } else {
    binary.y <- 1
  }
  
  # Create nfold-fold split such that there is no overlap between groups and 
  # splits
  data <- as.data.frame(fold(data, k = nfold, cat_col = d, id_col = groupvar, 
                             method = 'n_dist'))
  # cvgroup is a random assignment of the observations into K groups (values are 
  # indices 1,...,K)
  cvgroup <- data$.folds <- as.integer(data$.folds) 
  
  for(k in 1:M){   # loop over methods
    
    if(silent==FALSE){
      cat(methods[k],'\n')
    }
    
    if (any(c("RLasso", "PostRLasso", "Ridge", "PostLasso", "Lasso", 
              "Elnet")==methods[k])){
      # use large variable set including polynomials and interactions for
      # regularized regression
      xvars <- xL  
    } 
    if (methods[k]=="Boosting"){
      xvars <- xo # boosting can handle missing values
    } 
    if (any(methods[k]==c("Forest", "Nnet", "SVM"))){
      xvars <- xx # forest, SVM, and nnet can't handle missing values
    }
    
    for(j in 1:K){ # loop over folds
        
      if(silent==FALSE){
        cat('  fold',j,'\n')
      }
        
      ii  <- cvgroup == j # ii index 1 for leftout group
      nii <- cvgroup != j # nii index 1 for all other groups
      
      # data split used to estimate nuisance parameters (functions m and g)
      datause <- as.data.frame(data[nii,]) 
      # data split used to estimate treatment parameter (theta)
      dataout <- as.data.frame(data[ii,]) 

      # ignore variables with variance 0 in that fold
      d.is.1 <- which(datause[,d]==1)
      # predictors from training set
      Xuse.d0 <- dplyr::select(datause[-d.is.1,], one_of(xvars)) 
      # predictors from the left out fold
      Xuse.d1 <- dplyr::select(datause[d.is.1,], one_of(xvars)) 
        
      var.not.0.d0 <- !unlist(lapply(Xuse.d0, function(x) length(unique(x)) <2)) 
      var.not.0.d1 <- !unlist(lapply(Xuse.d1, function(x) length(unique(x)) <2))
      # indices of variables with non-zero variance for both d=0 and d=1
      var.not.0 <- var.not.0.d0 * var.not.0.d1 
      # var1 + var2 + ...
      form_x <- paste(names(var.not.0[var.not.0==1]), collapse='+') 
      x <- names(var.not.0[var.not.0==1])
        
      remove(d.is.1, Xuse.d0, Xuse.d1, var.not.0.d0, var.not.0.d1, var.not.0)
        
      cond.comp[[k,j]] <- cond_comp(datause=datause, dataout=dataout, y, d, 
                                    form_x, methods[k], plinear=0, binary.d, 
                                    binary.y, arguments=arguments)
      # MSE of g(x,0)
      MSE1[k] <- MSE1[k] + 
        cond.comp[[k,j]]$err.yz0^2 * length(which(dataout[,d]==0)) / 
        length(which(data[,d]==0))
      # MSE of g(x,1)
      MSE2[k] <- MSE2[k] + 
        cond.comp[[k,j]]$err.yz1^2 * length(which(dataout[,d]==1)) / 
        length(which(data[,d]==1))  
      # MSE of m(x) (prop. score)
      MSE3[k] <- MSE3[k] + 
        cond.comp[[k,j]]$err.z^2 * dim(dataout)[1] / dim(data)[1] 
        
      cond.comp[[k,j]]$dout <- dataout[,d]  
      cond.comp[[k,j]]$yout <- dataout[,y]
      cond.comp[[k,j]]$groupidout <- dataout[,groupvar]
      remove(datause, dataout)
    }
      
    groupvector <- c()
    mz_x        <- c()    
    my_z1x      <- c()
    my_z0x      <- c()
    yout        <- c()
    dout        <- c()
    groupidout  <- c()
    for (j in 1:K) {
      groupvector <- c(groupvector, rep(j,length(cond.comp[[k,j]]$mz_x)))
      mz_x <- c(mz_x, cond.comp[[k,j]]$mz_x)
      my_z1x <- c(my_z1x, cond.comp[[k,j]]$my_z1x)
      my_z0x <- c(my_z0x, cond.comp[[k,j]]$my_z0x)
      yout <- c(yout, cond.comp[[k,j]]$yout)
      dout <- c(dout, cond.comp[[k,j]]$dout)
      groupidout <- c(groupidout, cond.comp[[k,j]]$groupidout)
    }
  }
  
  # Nuisance parameters for best method
  min1 <- which.min(MSE1[1:M])   # the model with minimal MSE for g(x,0)
  min2 <- which.min(MSE2[1:M])   # the model with minimal MSE for g(x,1)
  min3 <- which.min(MSE3[1:M])   # the model with minimal MSE for m(x)
  
  for (j in 1:K) {
    ii  <- cvgroup == j # ii index 1 for leftout group
    nii <- cvgroup != j # nii index 1 for all other groups
    # data split used to estimate nuisance parameters (functions m and g)
    datause <- as.data.frame(data[nii,]) 
    # data split used to estimate treatment parameter (theta)
    dataout <- as.data.frame(data[ii,])  
    
    cond.comp[[M+1,j]]$my_z0x <- cond.comp[[min1,j]]$my_z0x
    cond.comp[[M+1,j]]$my_z1x <- cond.comp[[min2,j]]$my_z1x
    cond.comp[[M+1,j]]$mz_x <- cond.comp[[min3,j]]$mz_x
    cond.comp[[M+1,j]]$err.yz0 <- cond.comp[[min1,j]]$err.yz0
    cond.comp[[M+1,j]]$err.yz1 <- cond.comp[[min2,j]]$err.yz1
    cond.comp[[M+1,j]]$err.z <- cond.comp[[min3,j]]$err.z
    
    # MSE of g(x,0)
    MSE1[M+1] <- MSE1[M+1] + 
      cond.comp[[M+1,j]]$err.yz0^2 * length(which(dataout[,d]==0)) / 
      length(which(data[,d]==0)) 
    # MSE of g(x,1)
    MSE2[M+1] <- MSE2[M+1] + 
      cond.comp[[M+1,j]]$err.yz1^2 * length(which(dataout[,d]==1)) / 
      length(which(data[,d]==1))
    # MSE of m(x) (prop. score)
    MSE3[M+1] <- MSE3[M+1] + 
      cond.comp[[M+1,j]]$err.z^2 * dim(dataout)[1] / dim(data)[1] 
  }
  
  R.squared.y <- R.squared.d <- keepvec <- trimvec <- rep(0,(M+1))

  for (k in (1:(M+1))) {
    # do optimal trimming
    propscore <- c()
    for(j in 1:K) {
      propscore <- c(propscore, cond.comp[[k,j]]$mz_x)
    }
    # only consider observations with lower < propensity score < upper
    trim <- optimal.trim_mis(propscore, 1-propscore)
    
    mz_x     <- c()    
    my_z1x   <- c()
    my_z0x   <- c()
    yout     <- c()
    dout     <- c()
    groupout <- c()
    keepout  <- c()
    
    for(j in 1:K) {
      ii  <- cvgroup == j                  # ii index 1 for leftout group
      # data split used to estimate treatment parameter (theta)
      dataout <- as.data.frame(data[ii,])  
      keep <- c(NA,1)[(cond.comp[[k,j]]$mz_x>trim[1] & 
                         cond.comp[[k,j]]$mz_x<trim[2]) + 1]
      # append propscore for fold j
      mz_x <- c(mz_x, cond.comp[[k,j]]$mz_x * keep) 
      my_z1x <- c(my_z1x, cond.comp[[k,j]]$my_z1x * keep)
      my_z0x <- c(my_z0x, cond.comp[[k,j]]$my_z0x * keep)
      yout <- c(yout, dataout[,y] * keep)
      dout <- c(dout, dataout[,d] * keep)
      groupout <- c(groupout, dataout[,groupvar])
      keepout <- c(keepout, keep)
    }
    pout <- mean(dout, na.rm = TRUE)
    
    TE[,k] <- (dout * (yout - my_z1x) / mz_x) - 
      ((1 - dout) * (yout - my_z0x) / (1 - mz_x)) + my_z1x - my_z0x
    TET[,k] <- (dout * (yout - my_z0x) / pout) - 
      ((mz_x / (1 - mz_x)) * (1 - dout) * (yout - my_z0x) / pout)
    TEN[,k] <- -(((1 - dout) * (yout - my_z1x) / (1 - pout)) - 
                   (((1 - mz_x) / mz_x) * dout * (yout - my_z1x) / (1 - pout)))

    # Compute R^2 for outcome
    ybar <- mean(yout, na.rm=TRUE)
    total.sum.of.squares.y <- sum((yout - ybar)^2, na.rm = TRUE)
    yhat <- my_z1x * dout + my_z0x * (1 - dout)
    res.sum.of.squares.y <- sum((yout - yhat)^2, na.rm = TRUE)
    R.squared.y[k] <- 1 - res.sum.of.squares.y/total.sum.of.squares.y
    
    # Compute R^2 for treatment
    dbar <- mean(dout, na.rm=TRUE)
    total.sum.of.squares.d <- sum((dout - dbar)^2, na.rm = TRUE)
    res.sum.of.squares.d <- sum((dout - mz_x)^2, na.rm = TRUE)
    R.squared.d[k] <- 1 - res.sum.of.squares.d/total.sum.of.squares.d
    
    # Count how many observations were trimmed
    keepvec[k] <- sum(is.na(keepout))
    trimvec[k] <- trim[1]
  }
  
  RMSE <- rbind(sqrt(MSE1), sqrt(MSE2), sqrt(MSE3))
  rownames(RMSE) <- c("RMSE[Y|X, D=0]", "RMSE[Y|X, D=1]", "RMSE[D|X]")
  
  result.list <- list(TE, TET, TEN, groupout, RMSE, R.squared.y, R.squared.d, 
                      keepvec, trimvec)
  names(result.list) <- c("TE", "TET", "TEN", "groupID", "RMSE", "R.squared.y", 
                          "R.squared.d", "keepvec", "trimvec")
  return(result.list)
}  
