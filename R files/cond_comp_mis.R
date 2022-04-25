# This function produces vectors of DML nuisance parameters, using a variety of machine
# learning methods. This version allows for missing outcomes. 
# Author: Martin Wiegand; based on code provided by V.Chernozhukov, D. Chetverikov, 
# M. Demirer, E. Duflo, C. Hansen, W. Newey, J. Robins for their paper "Double/debiased machine 
# learning for treatment and structural parameters", Econometrics Journal (2018).
# Last changed: 23.09.2020

cond_comp_mis <- function(datause, dataout, y, d, r, rt, rc, x, method, plinear, binary.d, binary.y, arguments){
  
  form_y   <- y
  form_d   <- d
  form_x   <- x
  ind_u    <- which(datause[,d]==1)
  ind_o    <- which(dataout[,d]==1)
  ind_u_rt <- which(datause[,d]==1 & datause[,r]==1)
  ind_o_rt <- which(dataout[,d]==1 & dataout[,r]==1)
  ind_u_rc <- which(datause[,d]==0 & datause[,r]==1)
  ind_o_rc <- which(dataout[,d]==0 & dataout[,r]==1)
  err.yz1  <- NULL
  err.yz0  <- NULL
  my_z1x   <- NULL
  my_z0x   <- NULL
  fit.yz1  <- NULL
  fit.yz0  <- NULL
  
  ########################## Boosted  Trees ###################################################;
  
  if(method=="Boosting")
  {
    
    option <- arguments[[method]]
    arg    <- option
    arg[which(names(arg) %in% c("clas_dist","reg_dist"))] <-  NULL
    
    if(plinear==0){
      
      fit.yz1        <- xg.boost(datause=datause[ind_u_rt,], dataout=dataout, form_x=form_x, form_y=form_y, nround=arg$nrounds.d0, eta=arg$eta.d0, max.depth=arg$max_depth.d0, subsample=arg$subsample.d0, colsample_bytree=arg$colsample_bytree.d0, min_child_weight=arg$min_child_weight.d0, binary=binary.y)
      err.yz1        <- error(fit.yz1$yhatout[ind_o_rt], dataout[ind_o_rt,y])$err
      my_z1x         <- fit.yz1$yhatout

      fit.yz0        <- xg.boost(datause=datause[ind_u_rc,], dataout=dataout, form_x=form_x, form_y=form_y, nround=arg$nrounds.d1, eta=arg$eta.d1, max.depth=arg$max_depth.d1, subsample=arg$subsample.d1, colsample_bytree=arg$colsample_bytree.d1, min_child_weight=arg$min_child_weight.d1, binary=binary.y)
      err.yz0        <- error(fit.yz0$yhatout[ind_o_rc], dataout[ind_o_rc,y])$err
      my_z0x         <- fit.yz0$yhatout
    }
    
    if(binary.d==1){
      fit.z          <- xg.boost(datause=datause, dataout=dataout, form_x=form_x, form_y=form_d, nround=arg$nrounds.d, eta=arg$eta.d, max.depth=arg$max_depth.d, subsample=arg$subsample.d, colsample_bytree=arg$colsample_bytree.d, min_child_weight=arg$min_child_weight.d, binary=binary.d)
      mis.z          <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){
      fit.z          <- boost(datause=datause, dataout=dataout, form_x=form_x, form_y=form_d, distribution=option[['reg_dist']], nround=arg$nrounds.d, eta=arg$eta.d, max.depth=arg$max_depth.d, subsample=arg$subsample.d, colsample_bytree=arg$colsample_bytree.d, min_child_weight=arg$min_child_weight.d)
      mis.z          <- NA
    }
    
    fit.rt        <- xg.boost(datause=datause, dataout=dataout, form_x=form_x, form_y=rt, nround=arg$nrounds.rt, eta=arg$eta.rt, max.depth=arg$max_depth.rt, subsample=arg$subsample.rt, colsample_bytree=arg$colsample_bytree.rt, min_child_weight=arg$min_child_weight.rt, binary=binary.d)
    fit.rc        <- xg.boost(datause=datause, dataout=dataout, form_x=form_x, form_y=rc, nround=arg$nrounds.rc, eta=arg$eta.rc, max.depth=arg$max_depth.rc, subsample=arg$subsample.rc, colsample_bytree=arg$colsample_bytree.rc, min_child_weight=arg$min_child_weight.rc, binary=binary.d)
    
    fit.r_t       <- xg.boost(datause=datause[ind_u,], dataout=dataout, form_x=form_x, form_y=rt, nround=arg$nrounds.r_t, eta=arg$eta.r_t, max.depth=arg$max_depth.r_t, subsample=arg$subsample.r_t, colsample_bytree=arg$colsample_bytree.r_t, min_child_weight=arg$min_child_weight.r_t, binary=binary.d)
    fit.r_c       <- xg.boost(datause=datause[-ind_u,], dataout=dataout, form_x=form_x, form_y=rc, nround=arg$nrounds.r_c, eta=arg$eta.r_c, max.depth=arg$max_depth.r_c, subsample=arg$subsample.r_c, colsample_bytree=arg$colsample_bytree.r_c, min_child_weight=arg$min_child_weight.r_c, binary=binary.d)
    
    fit.y            <- 0
    
  }  
  
  
  ########################## Neural Network(Nnet Package) ###################################################;   
  
  
  if(method=="Nnet"){
    
    option <- arguments[[method]]
    arg    <- option
    
    if(plinear==0){
      
      fit.yz1        <- nnetF(datause=datause[ind_u,], dataout=dataout, form_x=form_x,  form_y=form_y, clas=TRUE, size=arg$size.d1, decay=arg$decay.d1)
      err.yz1        <- error(fit.yz1$yhatout[ind_o,], dataout[ind_o,y])$err
      my_z1x         <- fit.yz1$yhatout
      
      fit.yz0        <- nnetF(datause=datause[-ind_u,], dataout=dataout,  form_x=form_x, form_y=form_y, clas=TRUE, size=arg$size.d0, decay=arg$decay.d0)
      err.yz0        <- error(fit.yz0$yhatout[-ind_o,], dataout[-ind_o,y])$err
      my_z0x         <- fit.yz0$yhatout
    }
    
    if(binary.d==1){
      fit.z          <- nnetF(datause=datause, dataout=dataout, form_x=form_x, form_y=form_d, clas=TRUE, size=arg$size.d, decay=arg$decay.d)
      mis.z          <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){
      fit.z          <- nnetF(datause=datause, dataout=dataout, form_x=form_x, form_y=form_d, clas=FALSE, size=arg$size.d, decay=arg$decay.d)
      mis.z          <- NA
    }
    
    fit.y          <- 0
    
  } 
  
  ########################## Lasso and Post Lasso(Hdm Package) ###################################################;    
  
  if(method=="RLasso" || method=="PostRLasso"){
    
    post = FALSE
    if(method=="PostRLasso"){ post=TRUE }
    
    option    <- arguments[[method]]
    arg       <- option
    
    if(plinear==0){
      
      if(binary.y==1){
        logit.y <- TRUE
      } else {
        logit.y <- FALSE
      }
      
      fit.yz1        <- rlassoF(datause=datause[ind_u,], dataout=dataout,  form_x, form_y, post, logit=logit.y, arg=arg)[c("yhatout", "model")]
      err.yz1        <- error(fit.yz1$yhatout[ind_o,], dataout[ind_o,y])$err
      my_z1x         <- fit.yz1$yhatout
      
      fit.yz0        <- rlassoF(datause=datause[-ind_u,], dataout=dataout, form_x, form_y, post, logit=logit.y, arg=arg)[c("yhatout", "model")]
      err.yz0        <- error(fit.yz0$yhatout[-ind_o,], dataout[-ind_o,y])$err
      my_z0x         <- fit.yz0$yhatout
      
    }
    
    if(binary.d==1){
      fit.z          <- rlassoF(datause=datause, dataout=dataout,  form_x, form_d, post, logit=TRUE, arg=arg)[c("yhatout", "resout")]
      mis.z          <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){
      fit.z          <- rlassoF(datause=datause, dataout=dataout,  form_x, form_d, post, logit=FALSE, arg=arg)[c("yhatout", "resout")]
      mis.z          <- NA
    }   
    
    fit.y <- 0
  }    
  
  
  ########################## Lasso and Post Lasso(Glmnet) Package) ###################################################;    
  
  if(method=="Ridge" || method=="Lasso" || method=="Elnet" || method=="PostLasso"){
    
    if(method=="Lasso" | method=="PostLasso") {option <- arguments[["Lasso"]]}
    if(method=="Ridge") {option <- arguments[["Ridge"]]}
    if(method=="Elnet") {option <- arguments[["Elnet"]]}
    arg       <- option
    
    if(method=="PostLasso"){post <- TRUE}
    else{post <- FALSE}
    
    if(plinear==0){
      
      if(binary.y==1){
        logit.y <- TRUE
      } else {
        logit.y <- FALSE
      }
      
      lambda <- arg$lambda.yd1
      alp <- arg$alpha.yd1
      fit.yz1        <- RegRegF(datause=datause[ind_u_rt,], dataout=dataout,  form_x, form_y, logit=logit.y, lambda=lambda, alp=alp, post=post)
      err.yz1        <- error(fit.yz1$yhatout[ind_o_rt], dataout[ind_o_rt,y])$err
      my_z1x         <- fit.yz1$yhatout
      
      lambda <- arg$lambda.yd0
      alp <- arg$alpha.yd0
      fit.yz0        <- RegRegF(datause=datause[ind_u_rc,], dataout=dataout, form_x, form_y, logit=logit.y, lambda=lambda, alp=alp, post=post)
      err.yz0        <- error(fit.yz0$yhatout[ind_o_rc], dataout[ind_o_rc,y])$err
      my_z0x         <- fit.yz0$yhatout
      
    }
    
    lambda <- arg$lambda.d
    alp <- arg$alpha.d
    
    if(binary.d==1){
      fit.z          <- RegRegF(datause=datause, dataout=dataout,  form_x, form_d, logit=TRUE, lambda=lambda, alp=alp, post=post)
      mis.z          <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){
      fit.z          <- RegRegF(datause=datause, dataout=dataout,  form_x, form_d, logit=FALSE, lambda=lambda, alp=alp, post=post)
      mis.z          <- NA
    }   
    
    fit.rt        <- RegRegF(datause=datause, dataout=dataout, form_x, rt, logit=TRUE, lambda=arg$lambda.rt, alp=arg$alpha.rt, post=post)
    fit.rc        <- RegRegF(datause=datause, dataout=dataout, form_x, rc, logit=TRUE, lambda=arg$lambda.rc, alp=arg$alpha.rc, post=post)
    
    fit.r_t       <- RegRegF(datause=datause[ind_u,], dataout=dataout, form_x, rt, logit=TRUE, lambda=arg$lambda.r_t, alp=arg$alpha.r_t, post=post)
    fit.r_c       <- RegRegF(datause=datause[-ind_u,], dataout=dataout, form_x, rc, logit=TRUE, lambda=arg$lambda.r_c, alp=arg$alpha.r_c, post=post)
    
    fit.y            <- 0
    
  }    
  
  ########################## Lasso for combining results ###################################################;    
  
  if(method=="CLasso"){
    
    option <- arguments[["Lasso"]]
    arg       <- option
    
    if(binary.y==1){
      logit.y <- TRUE
    } else {
      logit.y <- FALSE
    }
    if(binary.d==1){
      logit.d <- TRUE
    } else {
      logit.d <- FALSE
    }
    
    form_x.yz1 <- paste(form_x[startsWith(form_x, 'my_z1x')], collapse='+')
    form_x.yz0 <- paste(form_x[startsWith(form_x, 'my_z0x')], collapse='+')
    form_x.z <- paste(form_x[startsWith(form_x, 'mz_x')], collapse='+')
    form_x.rt <- paste(form_x[startsWith(form_x, 'm_rt')], collapse='+')
    form_x.rc <- paste(form_x[startsWith(form_x, 'm_rc')], collapse='+')
    form_x.r_t <- paste(form_x[startsWith(form_x, 'm_r_t')], collapse='+')
    form_x.r_c <- paste(form_x[startsWith(form_x, 'm_r_c')], collapse='+')
    
    lambda <- arg$lambda.yd1
    alpha <- arg$alpha.yd1
    fit.yz1        <- RegRegF(datause=datause[ind_u_rt,], dataout=dataout,  form_x.yz1, form_y, logit=logit.y, lambda=lambda, alp=1, post=FALSE)
    err.yz1        <- error(fit.yz1$yhatout[ind_o_rt], dataout[ind_o_rt,y])$err
    my_z1x         <- fit.yz1$yhatout
    
    lambda <- arg$lambda.yd0
    alpha <- arg$alpha.yd0
    fit.yz0        <- RegRegF(datause=datause[ind_u_rc,], dataout=dataout, form_x.yz0, form_y, logit=logit.y, lambda=lambda, alp=1, post=FALSE)
    err.yz0        <- error(fit.yz0$yhatout[ind_o_rc], dataout[ind_o_rc,y])$err
    my_z0x         <- fit.yz0$yhatout
    
    lambda <- arg$lambda.d
    alpha <- arg$alpha.d
    fit.z          <- RegRegF(datause=datause, dataout=dataout,  form_x.z, form_d, logit=logit.d, lambda=lambda, alp=1, post=FALSE)
    mis.z          <- NA
    
    lambda <- arg$lambda.rt
    alpha <- arg$alpha.rt
    fit.rt          <- RegRegF(datause=datause, dataout=dataout,  form_x.rt, rt, logit=logit.d, lambda=lambda, alp=1, post=FALSE)
    mis.rt          <- NA
    
    lambda <- arg$lambda.rc
    alpha <- arg$alpha.rc
    fit.rc          <- RegRegF(datause=datause, dataout=dataout,  form_x.rc, rc, logit=logit.d, lambda=lambda, alp=1, post=FALSE)
    mis.rc          <- NA
    
    lambda <- arg$lambda.r_t
    alpha <- arg$alpha.r_t
    fit.r_t          <- RegRegF(datause=datause[ind_u,], dataout=dataout,  form_x.r_t, rt, logit=TRUE, lambda=lambda, alp=1, post=FALSE)
    mis.r_t          <- NA
    
    lambda <- arg$lambda.r_c
    alpha <- arg$alpha.r_c
    fit.r_c          <- RegRegF(datause=datause[-ind_u,], dataout=dataout,  form_x.r_c, rc, logit=TRUE, lambda=lambda, alp=1, post=FALSE)
    mis.r_c          <- NA
  
    fit.y <- 0
  }   
  
  ############# Random Forest ###################################################;
  
  if(method=="Forest" | method=="TForest"){
    
    tune = FALSE
    if(method=="TForest"){tune=TRUE}
    
    option    <- arguments[[method]]
    
    arg       <- option
    arg[which(names(arg) %in% c("clas_nodesize","reg_nodesize"))] <-  NULL
    
    if(plinear==0){
      
      if(binary.y==1){
        nodesize.y <- option[["clas_nodesize"]]
        reg.y <- FALSE
      } else {
        nodesize.y <- option[["reg_nodesize"]]
        reg.y <- TRUE
      }
      
        # The RF function (defined in ML_Functions) has output consisting of yhatuse, resuse, yhatout, resout, model
      fit.yz1        <- RF(datause=datause[ind_u_rt,], dataout=dataout, form_x=form_x,  form_y=form_y, nodesize=nodesize.y, arg=arg, reg=reg.y, tune=tune)
        # The error function (defined in ML_Functions) has output err (RMSE) and mis (misclassification rate)
      err.yz1        <- error(fit.yz1$yhatout[ind_o_rt], dataout[ind_o_rt,y])$err      # RMSE of prediction of g(x,1)
      my_z1x         <- fit.yz1$yhatout  # g_hat(x,1)
      
      fit.yz0        <- RF(datause=datause[ind_u_rc,], dataout=dataout,  form_x=form_x, form_y=form_y, nodesize=nodesize.y, arg=arg, reg=reg.y, tune=tune)
      err.yz0        <- error(fit.yz0$yhatout[ind_o_rc], dataout[ind_o_rc,y])$err     # RMSE of prediction of g(x,0)
      my_z0x         <- fit.yz0$yhatout  # g_hat(x,0)
      
    }
    
    if(binary.d==1){
      fit.z          <- RF(datause=datause, dataout=dataout,  form_x=form_x, form_y=form_d, nodesize=option[["clas_nodesize"]], arg=arg, reg=FALSE, tune=tune)
      mis.z          <- error(as.numeric(fit.z$yhatout), dataout[,y])$mis # misclassification rate of m(x)
    }
    
    if(binary.d==0){
      fit.z          <- RF(datause=datause, dataout=dataout,  form_x=form_x, form_y=form_d,nodesize=option[["clas_nodesize"]], arg=arg, reg=TRUE, tune=tune)
      mis.z          <- NA
    }   
    
    fit.rt        <- RF(datause=datause, dataout=dataout, form_x=form_x,  form_y=rt, nodesize=nodesize.y, arg=arg, reg=FALSE, tune=tune)
    fit.rc        <- RF(datause=datause, dataout=dataout, form_x=form_x,  form_y=rc, nodesize=nodesize.y, arg=arg, reg=FALSE, tune=tune)
    
    fit.r_t       <- RF(datause=datause[ind_u,], dataout=dataout, form_x=form_x,  form_y=rt, nodesize=nodesize.y, arg=arg, reg=FALSE, tune=tune)
    fit.r_c       <- RF(datause=datause[-ind_u,], dataout=dataout, form_x=form_x,  form_y=rc, nodesize=nodesize.y, arg=arg, reg=FALSE, tune=tune)
    
    fit.y          <- 0
    
  }
  
  ########################## Support Vector Machines ###################################################;   
  
  
  if(method=="SVM"){
    
    option <- arguments[[method]]
    arg    <- option
    
    if(plinear==0){
      
      Xout           <- as.matrix(dplyr::select(dataout, one_of(all.vars(form_x))))
      
      fit.yz1        <- svmF(datause[ind_u_rt,], dataout, form_x, form_y, cost=arg$cost_hsd1, gamma=arg$gamma_hsd1, binary=binary.y)
      err.yz1        <- error(fit.yz1$yhatout[ind_o_rt], dataout[ind_o_rt,y])$err
      my_z1x         <- fit.yz1$yhatout

      fit.yz0        <- svmF(datause[ind_u_rc,], dataout, form_x, form_y, cost=arg$cost_hsd0, gamma=arg$gamma_hsd0, binary=binary.y)
      err.yz0        <- error(fit.yz0$yhatout[ind_o_rc], dataout[ind_o_rc,y])$err
      my_z0x         <- fit.yz0$yhatout
    }
    
    if(binary.d==1){
      fit.z          <- svmF(datause, dataout, form_x=form_x, form_y=form_d, cost=arg$cost_treat, arg$gamma_treat, binary=1)
      mis.z          <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){
      fit.z          <- NA
      mis.z          <- NA
    }
    
    fit.rt        <- svmF(datause, dataout, form_x, rt, cost=arg$cost_rt, arg$gamma_rt, binary=1)
    fit.rc        <- svmF(datause, dataout, form_x, rc, cost=arg$cost_rc, arg$gamma_rc, binary=1)
    
    fit.r_t       <- svmF(datause[ind_u,], dataout, form_x, rt, cost=arg$cost_rt, arg$gamma_rt, binary=1)
    fit.r_c       <- svmF(datause[-ind_u,], dataout, form_x, rc, cost=arg$cost_rc, arg$gamma_rc, binary=1)
    
    fit.y          <- 0
    
  } 
  
  ########################## Regression Trees ###################################################;     
  
  if(method=="Trees"){
    
    option    <- arguments[[method]]
    arg       <- option
    arg[which(names(arg) %in% c("reg_method","clas_method"))] <-  NULL
    
    if(plinear==0){
      
      fit.yz1        <- tree(datause=datause[ind_u,], dataout=dataout[ind_o,], form_x=form_x,  form_y=form_y, method=option[["reg_method"]], arg=arg)
      err.yz1        <- error(fit.yz1$yhatout, dataout[ind_o,y])$err
      my_z1x         <- predict(fit.yz1$model, dataout) 
      
      fit.yz0        <- tree(datause=datause[-ind_u,], dataout=dataout[-ind_o,],  form_x=form_x, form_y=form_y, method=option[["reg_method"]], arg=arg)
      err.yz0        <- error(fit.yz0$yhatout, dataout[-ind_o,y])$err
      my_z0x         <- predict(fit.yz0$model,dataout)   
      
    }
    
    if(binary.d==1){
      
      fit.z          <- tree(datause=datause, dataout=dataout,  form_x=form_x, form_y=form_d, method=option[["clas_method"]], arg=arg)
      mis.z          <- error(as.numeric(fit.z$yhatout), dataout[,y])$mis
    }
    
    if(binary.d==0){
      
      fit.z          <- tree(datause=datause, dataout=dataout,  form_x=form_x, form_y=form_d, method=option[["reg_method"]], arg=arg)
      mis.z          <- NA
    }        
    
    fit.y          <- tree(datause=datause, dataout=dataout,  form_x=form_x, form_y=form_y, method=option[["cont_method"]], arg=arg)
    
  }
  
  # All of the following are predictions in the left out fold
  err.z          <- error(fit.z$yhatout, dataout[,d])$err   # RMSE of m(x)
  mz_x           <- fit.z$yhatout                           # m_hat(x)
  rz             <- fit.z$resout                            # residuals of m(x) (as.numeric(fit.p$y)) - as.numeric(yhatout)
  err.z          <- error(fit.z$yhatout, dataout[,d])$err   # same as above (by mistake?)
  
  err.rt        <- error(fit.rt$yhatout, dataout[,rt])$err
  m_rt         <- fit.rt$yhatout
  err.rc        <- error(fit.rc$yhatout, dataout[,rc])$err
  m_rc         <- fit.rc$yhatout
  err.r_t        <- error(fit.r_t$yhatout[ind_o], dataout[ind_o,rt])$err
  m_r_t         <- fit.r_t$yhatout
  err.r_c        <- error(fit.r_c$yhatout[-ind_o], dataout[-ind_o,rc])$err
  m_r_c         <- fit.r_c$yhatout
  
  return(list(my_z1x=my_z1x, mz_x= mz_x, my_z0x=my_z0x, err.z = err.z,  err.yz0= err.yz0,  err.yz1=err.yz1, mis.z=mis.z, rz=rz, fit.yz1out= fit.yz1$yhatout[ind_o_rt],  fit.yz0out= fit.yz0$yhatout[ind_o_rc], err.rt=err.rt, m_rt=m_rt, err.rc=err.rc, m_rc=m_rc, err.r_t=err.r_t, m_r_t=m_r_t, err.r_c=err.r_c, m_r_c=m_r_c, fit.r_tout= fit.r_t$yhatout[ind_o],  fit.r_cout= fit.r_c$yhatout[-ind_o]));
  
}  