# This function produces vectors of DML nuisance parameters, using a variety of 
# machine learning methods. This version assumes non-missing outcomes. 
# Author: Martin Wiegand 
# Partially based on code provided by V.Chernozhukov, D. Chetverikov, 
# M. Demirer, E. Duflo, C. Hansen, W. Newey, J. Robins for their paper 
# "Double/debiased machine learning for treatment and structural parameters", 
# Econometrics Journal (2018).
# Last changed: 26.04.2022
#
# In:
# - datause, dataout: data frames (the tuning and prediction sets, respectively)
# - y, d: character
# - x: character (factor variables as formula)
# - method: character (ML method to be used)
# - plinear: double (deprecated; always 0)
# - binary.d: double (indicator for binary treatment; always 1)
# - binary.y: double (indicator for binary outcome)
# - arguments: list (of hyperparameters for each ML method)
#  
# Out: 
# - my_z1x, my_z0x: lists (outcome scores in predicion set)
# - mz_x: list (propensity score in predicion set)
# - err.yz0, err.yz1, err.z: double (of RMSE)
# - mis.z: double (misclassification rate of m(x))
# - rz: list (residuals of m(x))
# - fit.yz1out, fit.yz0out: lists (outcome scores in prediction set separate by 
#     treatment status)


cond_comp <- function(datause, dataout, y, d, x, method, plinear, binary.d, 
                      binary.y, arguments){
  
  form_y   <- y
  form_d   <- d
  form_x   <- x
  ind_u    <- which(datause[,d]==1)
  ind_o    <- which(dataout[,d]==1)
  err.yz1  <- NULL
  err.yz0  <- NULL
  my_z1x   <- NULL
  my_z0x   <- NULL
  fit.yz1  <- NULL
  fit.yz0  <- NULL
  
  ########################## Boosted Trees #####################################
  
  if(method=="Boosting")  {
    
    option <- arguments[[method]]
    arg    <- option
    arg[which(names(arg) %in% c("clas_dist","reg_dist"))] <-  NULL
    
    if(plinear==0){
      fit.yz1 <- xg.boost(datause=datause[ind_u,], dataout=dataout, 
                          form_x=form_x, form_y=form_y, nround=arg$nrounds.d0, 
                          eta=arg$eta.d0, max.depth=arg$max_depth.d0, 
                          subsample=arg$subsample.d0, 
                          colsample_bytree=arg$colsample_bytree.d0, 
                          min_child_weight=arg$min_child_weight.d0, 
                          binary=binary.y, double=FALSE, startmodel=0)
      err.yz1 <- error(fit.yz1$yhatout[ind_o], dataout[ind_o,y])$err
      my_z1x  <- fit.yz1$yhatout
      
      fit.yz0 <- xg.boost(datause=datause[-ind_u,], dataout=dataout, 
                          form_x=form_x, form_y=form_y, nround=arg$nrounds.d1, 
                          eta=arg$eta.d1, max.depth=arg$max_depth.d1, 
                          subsample=arg$subsample.d1, 
                          colsample_bytree=arg$colsample_bytree.d1, 
                          min_child_weight=arg$min_child_weight.d1, 
                          binary=binary.y, double=FALSE, startmodel=0)
      err.yz0 <- error(fit.yz0$yhatout[-ind_o], dataout[-ind_o,y])$err
      my_z0x  <- fit.yz0$yhatout
    }
    
    if(binary.d==1){
      fit.z   <- xg.boost(datause=datause, dataout=dataout, form_x=form_x, 
                          form_y=form_d, nround=arg$nrounds.d, eta=arg$eta.d, 
                          max.depth=arg$max_depth.d, subsample=arg$subsample.d, 
                          colsample_bytree=arg$colsample_bytree.d, 
                          min_child_weight=arg$min_child_weight.d, 
                          binary=binary.d, double=FALSE, startmodel=0)
      mis.z   <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){ 
      # not specified, d is always binary
      stop("The case binary.d = 0 is not specified.")
    }
    
    fit.y     <- 0
    
  }  
  
  
  ########################## Neural Network(Nnet Package) ######################  
  
  
  if(method=="Nnet"){
    
    option <- arguments[[method]]
    arg    <- option
    
    if(plinear==0){
      
      fit.yz1 <- nnetF(datause=datause[ind_u,], dataout=dataout, form_x=form_x, 
                       form_y=form_y, clas=TRUE, size=arg$size.d1, 
                       decay=arg$decay.d1)
      err.yz1 <- error(fit.yz1$yhatout[ind_o,], dataout[ind_o,y])$err
      my_z1x  <- fit.yz1$yhatout
      
      fit.yz0 <- nnetF(datause=datause[-ind_u,], dataout=dataout, form_x=form_x, 
                       form_y=form_y, clas=TRUE, size=arg$size.d0, 
                       decay=arg$decay.d0)
      err.yz0 <- error(fit.yz0$yhatout[-ind_o,], dataout[-ind_o,y])$err
      my_z0x  <- fit.yz0$yhatout
    }
    
    if(binary.d==1){
      fit.z   <- nnetF(datause=datause, dataout=dataout, form_x=form_x, 
                       form_y=form_d, clas=TRUE, size=arg$size.d, 
                       decay=arg$decay.d)
      mis.z   <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){
      # not specified, d is always binary
      stop("The case binary.d = 0 is not specified.")
    }
    
    fit.y     <- 0
    
  } 
  
  ########################## Lasso and Post Lasso(Hdm Package) #################   
  
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
      
      form_x.yz1 <- paste(form_x[startsWith(form_x, 'my_z1x')], collapse='+')
      form_x.yz0 <- paste(form_x[startsWith(form_x, 'my_z0x')], collapse='+')
      form_x.z   <- paste(form_x[startsWith(form_x, 'mz_x')], collapse='+')
      
      fit.yz1    <- rlassoF(datause=datause[ind_u,], dataout=dataout, 
                            form_x.yz1, form_y, post, logit=logit.y, 
                            arg=arg)[c("yhatout", "model")]
      err.yz1    <- error(fit.yz1$yhatout[ind_o,], dataout[ind_o,y])$err
      my_z1x     <- fit.yz1$yhatout
       
      fit.yz0    <- rlassoF(datause=datause[-ind_u,], dataout=dataout, 
                            form_x.yz0, form_y, post, logit=logit.y, 
                            arg=arg)[c("yhatout", "model")]
      err.yz0    <- error(fit.yz0$yhatout[-ind_o,], dataout[-ind_o,y])$err
      my_z0x     <- fit.yz0$yhatout
      
    }
    
    if(binary.d==1){
      fit.z      <- rlassoF(datause=datause, dataout=dataout, form_x.z, form_d, 
                            post, logit=TRUE, arg=arg)[c("yhatout", "resout")]
      mis.z      <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){
      # not specified, d is always binary
      stop("The case binary.d = 0 is not specified.")
    }   
    
    fit.y        <- 0
  }    
  
  
  ########################## Lasso and Post Lasso(Glmnet) Package) #############
  
  if(method=="Ridge" || method=="Lasso" || method=="Elnet" || 
     method=="PostLasso"){
    
    if(method=="Lasso" | method=="PostLasso") {option <- arguments[["Lasso"]]}
    if(method=="Ridge") {option <- arguments[["Ridge"]]}
    if(method=="Elnet") {option <- arguments[["Elnet"]]}
    arg <- option
    
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
      fit.yz1 <- RegRegF(datause=datause[ind_u,], dataout=dataout, form_x, 
                         form_y, logit=logit.y, lambda=lambda, alp=alp, 
                         post=post)
      err.yz1 <- error(fit.yz1$yhatout[ind_o], dataout[ind_o,y])$err
      my_z1x  <- fit.yz1$yhatout
      
      lambda <- arg$lambda.yd0
      alp <- arg$alpha.yd0
      fit.yz0 <- RegRegF(datause=datause[-ind_u,], dataout=dataout, form_x, 
                         form_y, logit=logit.y, lambda=lambda, alp=alp, 
                         post=post)
      err.yz0 <- error(fit.yz0$yhatout[-ind_o], dataout[-ind_o,y])$err
      my_z0x  <- fit.yz0$yhatout
      
    }
    
    lambda <- arg$lambda.d
    alp <- arg$alpha.d
    
    if(binary.d==1){
      fit.z   <- RegRegF(datause=datause, dataout=dataout,form_x, form_d, 
                         logit=TRUE, lambda=lambda, alp=alp, post=post)
      mis.z   <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){
      # not specified, d is always binary
      stop("The case binary.d = 0 is not specified.")
    }   
    
    fit.y     <- 0
    
  }    
  
  ########################## Lasso for combining results #######################
  
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
    
    lambda <- arg$lambda.yd1
    alpha <- arg$alpha.yd1
    fit.yz1 <- RegRegF(datause=datause[ind_u,], dataout=dataout, form_x.yz1, 
                       form_y, logit=logit.y, lambda=lambda, alp=1, post=FALSE)
    err.yz1 <- error(fit.yz1$yhatout[ind_o], dataout[ind_o,y])$err
    my_z1x  <- fit.yz1$yhatout
    
    lambda <- arg$lambda.yd0
    alpha <- arg$alpha.yd0
    fit.yz0 <- RegRegF(datause=datause[-ind_u,], dataout=dataout, form_x.yz0, 
                       form_y, logit=logit.y, lambda=lambda, alp=1, post=FALSE)
    err.yz0 <- error(fit.yz0$yhatout[-ind_o], dataout[-ind_o,y])$err
    my_z0x  <- fit.yz0$yhatout
    
    lambda <- arg$lambda.d
    alpha <- arg$alpha.d
    fit.z   <- RegRegF(datause=datause, dataout=dataout,  form_x.z, form_d, 
                       logit=logit.d, lambda=lambda, alp=1, post=FALSE)
    mis.z   <- NA
    
    fit.y <- 0
  }   
  
  ############# Random Forest ##################################################
  
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
      
      # The RF function (defined in ML_Functions) has output consisting of 
      # yhatuse, resuse, yhatout, resout, model
      fit.yz1 <- RF(datause=datause[ind_u,], dataout=dataout, form_x=form_x, 
                    form_y=form_y, nodesize=nodesize.y, arg=arg, reg=reg.y, 
                    tune=tune)
      # The error function (defined in ML_Functions) has output err (RMSE) and 
      # mis (misclassification rate).
      # RMSE of prediction of g(x,1)
      err.yz1 <- error(fit.yz1$yhatout[ind_o], dataout[ind_o,y])$err
      # g_hat(x,1)
      my_z1x  <- fit.yz1$yhatout  
      
      fit.yz0 <- RF(datause=datause[-ind_u,], dataout=dataout, form_x=form_x, 
                    form_y=form_y, nodesize=nodesize.y, arg=arg, reg=reg.y, 
                    tune=tune)
      # RMSE of prediction of g(x,0)
      err.yz0 <- error(fit.yz0$yhatout[-ind_o], dataout[-ind_o,y])$err   
      # g_hat(x,0)
      my_z0x  <- fit.yz0$yhatout 
      
    }
    
    if(binary.d==1){
      fit.z   <- RF(datause=datause, dataout=dataout,  form_x=form_x, 
                    form_y=form_d, nodesize=option[["clas_nodesize"]], arg=arg, 
                    reg=FALSE, tune=tune)
      # misclassification rate of m(x)
      mis.z   <- error(as.numeric(fit.z$yhatout), dataout[,y])$mis 
    }
    
    if(binary.d==0){
      # not specified, d is always binary
      stop("The case binary.d = 0 is not specified.")
    }   
    
    fit.y          <- 0

  }
  
  ########################## Support Vector Machines ###########################
  
  
  if(method=="SVM"){
    
    option <- arguments[[method]]
    arg    <- option
    
    if(plinear==0){
      
      Xout    <- as.matrix(dplyr::select(dataout, one_of(all.vars(form_x))))
      
      fit.yz1 <- svmF(datause[ind_u,], dataout, form_x, form_y, 
                      cost=arg$cost_hsd1, gamma=arg$gamma_hsd1, binary=binary.y)
      err.yz1 <- error(fit.yz1$yhatout[ind_o], dataout[ind_o,y])$err
      my_z1x  <- fit.yz1$yhatout

      fit.yz0 <- svmF(datause[-ind_u,], dataout, form_x, form_y, 
                      cost=arg$cost_hsd0, arg$gamma_hsd0, binary=binary.y)
      err.yz0 <- error(fit.yz0$yhatout[-ind_o], dataout[-ind_o,y])$err
      my_z0x  <- fit.yz0$yhatout
    }
    
    if(binary.d==1){
      fit.z   <- svmF(datause, dataout, form_x=form_x, form_y=form_d, 
                      cost=arg$cost_treat, arg$gamma_treat, binary=1)
      mis.z   <- error(fit.z$yhatout, dataout[,d])$mis
    }
    
    if(binary.d==0){
      # not specified, d is always binary
      stop("The case binary.d = 0 is not specified.")
    }
    
    fit.y          <- 0
    
  } 

  
  # All of the following are predictions in the left out fold
  # RMSE of m(x)
  err.z <- error(fit.z$yhatout, dataout[,d])$err   
  # m_hat(x)
  mz_x  <- fit.z$yhatout                           
  # residuals of m(x) (as.numeric(fit.p$y)) - as.numeric(yhatout)
  rz    <- fit.z$resout 

  return(list(my_z1x = my_z1x, mz_x = mz_x, my_z0x = my_z0x, err.z = err.z, 
              err.yz0 = err.yz0, err.yz1 = err.yz1, mis.z = mis.z, rz = rz, 
              fit.yz1out = fit.yz1$yhatout[ind_o], 
              fit.yz0out = fit.yz0$yhatout[-ind_o]));
  
}  