# This function produces performs repeated cross-validation for regularized regression, SVM, 
# and xgboost. The output is a list of values for the hyperparameters of the respective method
# for the model without attrition and continuous outcomes. 
# Author: Martin Wiegand
# Last changed: 23.09.2020

Best_Parameters_Reg <- function(method, data, y, d, groupvar, xvars, folds, reps, seed = 1){
  
  set.seed(seed = seed)
  
  # transform dataset 
  foldslist.d0 <- list()
  indexlist.d0 <- list()
  indexoutlist.d0 <- list()
  foldslist.d1 <- list()
  indexlist.d1 <- list()
  indexoutlist.d1 <- list()
  foldslist.d <- list()
  indexlist.d <- list()
  indexoutlist.d <- list()
  counter <- 1
  data <- as.data.frame(fold(data, k = folds, cat_col = d, id_col = groupvar, method = 'n_dist')) # this is just to get the order right
  data$.folds <- c()
  d.is.1 <- which(data[,d]==1)
  d.is.0 <- which(data[,d]==0)
  for (r in 1:reps) {
    foldslist.d[[r]] <- as.integer(fold(data, k = folds, cat_col = d, id_col = groupvar, method = 'n_dist')$.folds)
    foldslist.d1[[r]] <- as.integer(foldslist.d[[r]][d.is.1])
    foldslist.d0[[r]] <- as.integer(foldslist.d[[r]][-d.is.1])
    for (i in 1:folds) {
      indexlist.d[[counter]] <- which(foldslist.d[[r]]!=i)
      indexlist.d1[[counter]] <- intersect(which(foldslist.d[[r]]!=i),d.is.1)
      indexlist.d0[[counter]] <- intersect(which(foldslist.d[[r]]!=i),d.is.0)
      indexoutlist.d[[counter]] <- (1:nrow(data))[-indexlist.d[[counter]]]
      indexoutlist.d1[[counter]] <- intersect((1:nrow(data))[-indexlist.d[[counter]]],d.is.1)
      indexoutlist.d0[[counter]] <- intersect((1:nrow(data))[-indexlist.d[[counter]]],d.is.0)
      counter <- counter + 1
    }
  }
  
  
  # ignore variables with variance 0
  X.d0 <- dplyr::select(data[-d.is.1,], one_of(xvars))
  X.d1 <- dplyr::select(data[d.is.1,], one_of(xvars))
  
  var.not.0.d0 <- !unlist(lapply(X.d0, function(x) length(unique(x)) <2)) 
  var.not.0.d1 <- !unlist(lapply(X.d1, function(x) length(unique(x)) <2))
  var.not.0 <- var.not.0.d0 * var.not.0.d1 # indices of variables with non-zero variance for both d=0 and d=1
  form_x <- paste(names(var.not.0[var.not.0==1]), collapse='+') # var1 + var2 + ...
  x <- names(var.not.0[var.not.0==1])
  
  
  
  if(method == "Lasso" | method == "PostLasso" | method == "Ridge") {
    
    if(method == "Lasso" | method == "PostLasso") {
      alp <- 1
    } else {
      alp <- 0
    }
    
    data.d0 <- as.matrix(dplyr::select(data[-d.is.1,], one_of(x)))
    y.d0 <- as.matrix(dplyr::select(data[-d.is.1,], one_of(y)))
    
    data.d1 <- as.matrix(dplyr::select(data[d.is.1,], one_of(x)))
    y.d1 <- as.matrix(dplyr::select(data[d.is.1,], one_of(y)))
    
    data.d <- as.matrix(dplyr::select(data, one_of(x)))
    y.d <- as.matrix(dplyr::select(data, one_of(d)))
    
    cv.error.yd0 <- 0 # going to be error vectors of length of all the lambda values tried out
    cv.error.yd1 <- 0
    cv.error.d <- 0
    lambda.seq.yd0 <- NULL
    lambda.seq.yd1 <- NULL
    lambda.seq.d <- NULL
    for(r in 1:reps) {
      lasso.yd0 <- cv.glmnet(x=data.d0, y=y.d0, family="gaussian", lambda=lambda.seq.yd0, alpha=alp, foldid = foldslist.d0[[r]], parallel = TRUE) 
      if (r==1) {
        while (lasso.yd0$lambda.min >= lasso.yd0$lambda[5] & lasso.yd0$cvm[1] != lasso.yd0$cvm[length(lasso.yd0$cvm)] & lasso.yd0$lambda[5]<100000) {
          lambda.seq.yd0 <- lasso.yd0$lambda*10
          lasso.yd0 <- cv.glmnet(x=data.d0, y=y.d0, family="gaussian", lambda=lambda.seq.yd0, alpha=alp, foldid = foldslist.d0[[r]], parallel = TRUE)
        }
        lambda.seq.yd0 <- lasso.yd0$lambda
      }
      lasso.yd1 <- cv.glmnet(x=data.d1, y=y.d1, family="gaussian", lambda=lambda.seq.yd1, alpha=alp, foldid = foldslist.d1[[r]], parallel = TRUE)
      if (r==1) {
        while (lasso.yd1$lambda.min >= lasso.yd1$lambda[5] & lasso.yd1$cvm[1] != lasso.yd1$cvm[length(lasso.yd1$cvm)] & lasso.yd1$lambda[5]<100000) {
          lambda.seq.yd1 <- lasso.yd1$lambda*10
          lasso.yd1 <- cv.glmnet(x=data.d1, y=y.d1, family="gaussian", lambda=lambda.seq.yd1, alpha=alp, foldid = foldslist.d1[[r]], parallel = TRUE)
        }
        lambda.seq.yd1 <- lasso.yd1$lambda
      }
      lasso.d <- cv.glmnet(x=data.d, y=y.d, family="binomial", lambda=lambda.seq.d, alpha=alp, foldid = foldslist.d[[r]], parallel = TRUE)
      if (r==1) {
        while (lasso.d$lambda.min >= lasso.d$lambda[5] & lasso.d$cvm[1] != lasso.d$cvm[length(lasso.d$cvm)] & lasso.d$lambda[5]<100000) {
          lambda.seq.d <- lasso.d$lambda*10
          lasso.d <- cv.glmnet(x=data.d, y=y.d, family="binomial", lambda=lambda.seq.d, alpha=alp, foldid = foldslist.d[[r]], parallel = TRUE)
        }
        lambda.seq.d <- lasso.d$lambda
      }
      cv.error.yd0 <- cv.error.yd0 + lasso.yd0$cvm
      cv.error.yd1 <- cv.error.yd1 + lasso.yd1$cvm
      cv.error.d <- cv.error.d + lasso.d$cvm
      print(r)
    }
    lambda.yd0 <- lasso.yd0$lambda[which.min(cv.error.yd0)]
    lambda.yd1 <- lasso.yd1$lambda[which.min(cv.error.yd1)]
    lambda.d <- lasso.d$lambda[which.min(cv.error.d)]
    
    para <- list(lambda.yd0=lambda.yd0, lambda.yd1=lambda.yd1, lambda.d=lambda.d)
    
  }
  
  
  if(method == "Elnet") {
    
    alphas <- seq(0.1, 0.9, by=0.1)
    
    data.d0 <- as.matrix(dplyr::select(data[-d.is.1,], one_of(x)))
    y.d0 <- as.matrix(dplyr::select(data[-d.is.1,], one_of(y)))
    
    data.d1 <- as.matrix(dplyr::select(data[d.is.1,], one_of(x)))
    y.d1 <- as.matrix(dplyr::select(data[d.is.1,], one_of(y)))
    
    data.d <- as.matrix(dplyr::select(data, one_of(x)))
    y.d <- as.matrix(dplyr::select(data, one_of(d)))
    
    lambdamat <- matrix(0, nrow = 3, ncol = length(alphas))
    errormat <- matrix(0, nrow = 3, ncol = length(alphas))
    for(alp in alphas) {
      cv.error.yd0 <- 0
      cv.error.yd1 <- 0
      cv.error.d <- 0
      lambda.seq.yd0 <- NULL
      lambda.seq.yd1 <- NULL
      lambda.seq.d <- NULL
      for(r in 1:reps) {
        lasso.yd0 <- cv.glmnet(x=data.d0, y=y.d0, family="gaussian", lambda=lambda.seq.yd0, alpha=alp, foldid = foldslist.d0[[r]], parallel = TRUE) 
        if (r==1) {
          while (lasso.yd0$lambda.min >= lasso.yd0$lambda[5] & lasso.yd0$cvm[1] != lasso.yd0$cvm[length(lasso.yd0$cvm)] & lasso.yd0$lambda[5]<100000) {
            lambda.seq.yd0 <- lasso.yd0$lambda*10
            lasso.yd0 <- cv.glmnet(x=data.d0, y=y.d0, family="gaussian", lambda=lambda.seq.yd0, alpha=alp, foldid = foldslist.d0[[r]], parallel = TRUE)
          }
          lambda.seq.yd0 <- lasso.yd0$lambda
        }
        lasso.yd1 <- cv.glmnet(x=data.d1, y=y.d1, family="gaussian", lambda=lambda.seq.yd1, alpha=alp, foldid = foldslist.d1[[r]], parallel = TRUE)
        if (r==1) {
          while (lasso.yd1$lambda.min >= lasso.yd1$lambda[5] & lasso.yd1$cvm[1] != lasso.yd1$cvm[length(lasso.yd1$cvm)] & lasso.yd1$lambda[5]<100000) {
            lambda.seq.yd1 <- lasso.yd1$lambda*10
            lasso.yd1 <- cv.glmnet(x=data.d1, y=y.d1, family="gaussian", lambda=lambda.seq.yd1, alpha=alp, foldid = foldslist.d1[[r]], parallel = TRUE)
          }
          lambda.seq.yd1 <- lasso.yd1$lambda
        }
        lasso.d <- cv.glmnet(x=data.d, y=y.d, family="binomial", lambda=lambda.seq.d, alpha=alp, foldid = foldslist.d[[r]], parallel = TRUE)
        if (r==1) {
          while (lasso.d$lambda.min >= lasso.d$lambda[5] & lasso.d$cvm[1] != lasso.d$cvm[length(lasso.d$cvm)] & lasso.d$lambda[5]<100000) {
            lambda.seq.d <- lasso.d$lambda*10
            lasso.d <- cv.glmnet(x=data.d, y=y.d, family="binomial", lambda=lambda.seq.d, alpha=alp, foldid = foldslist.d[[r]], parallel = TRUE)
          }
          lambda.seq.d <- lasso.d$lambda
        }
        cv.error.yd0 <- cv.error.yd0 + lasso.yd0$cvm
        cv.error.yd1 <- cv.error.yd1 + lasso.yd1$cvm
        cv.error.d <- cv.error.d + lasso.d$cvm
        print(r)
      }
      lambdamat[1,which(alphas == alp)] <- lasso.yd0$lambda[which.min(cv.error.yd0)]
      lambdamat[2,which(alphas == alp)] <- lasso.yd1$lambda[which.min(cv.error.yd1)]
      lambdamat[3,which(alphas == alp)] <- lasso.d$lambda[which.min(cv.error.d)]
      errormat[1,which(alphas == alp)] <- min(cv.error.yd0)
      errormat[2,which(alphas == alp)] <- min(cv.error.yd1)
      errormat[3,which(alphas == alp)] <- min(cv.error.d)
    }
    
    alpha.yd0 <- alphas[which(errormat == rowMins(errormat, rows = 1), arr.ind = TRUE)[2]]
    lambda.yd0 <- lambdamat[1, which(errormat == rowMins(errormat, rows = 1), arr.ind = TRUE)[2]]
    alpha.yd1 <- alphas[which(errormat == rowMins(errormat, rows = 2), arr.ind = TRUE)[2]]
    lambda.yd1 <- lambdamat[2, which(errormat == rowMins(errormat, rows = 2), arr.ind = TRUE)[2]]
    alpha.d <- alphas[which(errormat == rowMins(errormat, rows = 3), arr.ind = TRUE)[2]]
    lambda.d <- lambdamat[3, which(errormat == rowMins(errormat, rows = 3), arr.ind = TRUE)[2]]
    
    para <- list(lambda.yd0=lambda.yd0, lambda.yd1=lambda.yd1, lambda.d=lambda.d, 
                 alpha.yd0=alpha.yd0, alpha.yd1=alpha.yd1, alpha.d=alpha.d)
    
  }
  
  
  if(method == "SVM") {
    
    costvector <- c(0.1, 0.25, 0.5, 0.75, 1, 2, 3, 5)
    gammavector <- c(0.001, 0.003, 0.006, 0.01, 0.02, 0.04, 0.1)
    paragrid.class <- expand.grid(C = costvector, sigma = gammavector) # the names vary across functions...
    
    costvector2 <- 2^(-3:7)
    paragrid.reg <- expand.grid(C = costvector2) 
    
    form <- as.formula(paste("ynew", "~", form_x))
    
    data.y <- dplyr::select(data, one_of(x)) %>%
      mutate(ynew = data[,y])
    
    svm_trcontrol.d0 <- trainControl(index = indexlist.d0, indexOut = indexoutlist.d0, indexFinal = d.is.0, 
                                     verboseIter = TRUE, returnData = FALSE, returnResamp = "all", 
                                     allowParallel = TRUE)
    
    svm.tune.d0 <- train(form = form, data = data.y, method = "svmLinear", metric = "RMSE", tuneGrid=paragrid.reg, 
                         scale = FALSE, trControl=svm_trcontrol.d0, nthread=1)$bestTune
    
    svm_trcontrol.d1 <- trainControl(index = indexlist.d1, indexOut = indexoutlist.d1, indexFinal = d.is.1, 
                                     verboseIter = TRUE, returnData = FALSE, returnResamp = "all", 
                                     allowParallel = TRUE)
    
    svm.tune.d1 <- train(form = form, data = data.y, method = "svmLinear", metric = "RMSE", tuneGrid=paragrid.reg, 
                         scale = FALSE, trControl=svm_trcontrol.d1, nthread=1)$bestTune
    
    data.d <- dplyr::select(data, one_of(x)) %>%
      mutate(ynew = factor(data[,d]*2 - 1, labels=c("X0", "X1")))
    
    svm_trcontrol.d <- trainControl(index = indexlist.d, indexOut = indexoutlist.d, 
                                    verboseIter = TRUE, returnData = FALSE, summaryFunction=twoClassSummary,	
                                    classProbs=TRUE, returnResamp = "all", allowParallel = TRUE)
    
    svm.tune.d <- train(form = form, data = data.d, method = "svmRadial", metric = "ROC", tuneGrid=paragrid.class, 
                        scale = FALSE, trControl=svm_trcontrol.d, nthread=1)$bestTune
    
    para <- list(cost_hsd0 = svm.tune.d0$C, 
                 gamma_hsd0 = 1, 
                 cost_hsd1 = svm.tune.d1$C, 
                 gamma_hsd1 = 1,
                 cost_treat = svm.tune.d$C, 
                 gamma_treat = 1)
    
    
  }
  
  
  if(method == "Boosting") {
    
    data.x <- as.matrix(dplyr::select(data, one_of(x)))
    
    y.y <- data[,y]
    
    y.d <- as.factor(data[,d])
    levels(y.d) <- c("X0", "X1")
    
    
    # set up the cross-validated hyper-parameter search

    # First, tune only eta and nrounds
    xgb_grid.1 <- expand.grid(nrounds = seq(from=1, to=500), eta = 10**(0:-3), 
                              #max_depth = c(2,3,4,5,6,8,10,12), gamma = 0, subsample = 1,
                              max_depth = c(1,2,4,7,10,15), gamma = 0, subsample = 1,
                              colsample_bytree = 1, min_child_weight = 1)
    
    xgb_trcontrol.d <- trainControl(index = indexlist.d, indexOut = indexoutlist.d, 
                                    verboseIter = TRUE, returnData = FALSE, returnResamp = "all", 
                                    classProbs = TRUE, allowParallel = TRUE)
    
    xgb_trcontrol.d0 <- trainControl(index = indexlist.d0, indexOut = indexoutlist.d0, indexFinal = d.is.0, 
                                     verboseIter = TRUE, returnData = FALSE, returnResamp = "all", 
                                     allowParallel = TRUE)
    
    xgb_trcontrol.d1 <- trainControl(index = indexlist.d1, indexOut = indexoutlist.d1, indexFinal = d.is.1, 
                                     verboseIter = TRUE, returnData = FALSE, returnResamp = "all", 
                                     allowParallel = TRUE)
    
    xgb_train_d0.1 <- train(x = data.x, y = y.y, trControl = xgb_trcontrol.d0, 
                            tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.d0     <- xgb_train_d0.1$nrounds
    eta.d0         <- xgb_train_d0.1$eta
    max_depth.d0   <- xgb_train_d0.1$max_depth

    xgb_train_d1.1 <- train(x = data.x, y = y.y, trControl = xgb_trcontrol.d1, 
                            tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.d1     <- xgb_train_d1.1$nrounds
    eta.d1         <- xgb_train_d1.1$eta
    max_depth.d1   <- xgb_train_d1.1$max_depth
    
    xgb_train_d.1 <- train(x = data.x, y = y.d, trControl = xgb_trcontrol.d, 
                           tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.d     <- xgb_train_d.1$nrounds
    eta.d         <- xgb_train_d.1$eta
    max_depth.d   <- xgb_train_d.1$max_depth
    
    # Second, fix the optimal values from the first step and tune max_depth, gamma, subsample, colsample_bytree
    
    xgb_grid.2.d0 <- expand.grid(nrounds = nrounds.d0, eta = eta.d0, 
                                 max_depth = max_depth.d0, gamma = 0, subsample = c(0.4,0.7,1),
                                 colsample_bytree = c(0.5,0.7,1), min_child_weight = c(0.5,1,2,4))
    
    xgb_grid.2.d1 <- expand.grid(nrounds = nrounds.d1, eta = eta.d1, 
                                 max_depth = max_depth.d1, gamma = 0, subsample = c(0.4,0.7,1),
                                 colsample_bytree = c(0.5,0.7,1), min_child_weight = c(0.5,1,2,4))
    
    xgb_grid.2.d <- expand.grid(nrounds = nrounds.d, eta = eta.d, 
                                max_depth = max_depth.d, gamma = 0, subsample = c(0.4,0.7,1),
                                colsample_bytree = c(0.5,0.7,1), min_child_weight = c(0.5,1,2,4))

    
    # train the model for each parameter combination in the grid, using CV to evaluate
    
    xgb_train_d0.2 <- train(x = data.x, y = y.y, trControl = xgb_trcontrol.d0, 
                            tuneGrid = xgb_grid.2.d0, method = "xgbTree", nthread=1)$bestTune
    
    xgb_train_d1.2 <- train(x = data.x, y = y.y, trControl = xgb_trcontrol.d1, 
                            tuneGrid = xgb_grid.2.d1, method = "xgbTree", nthread=1)$bestTune
    
    xgb_train_d.2 <- train(x = data.x, y = y.d, trControl = xgb_trcontrol.d, 
                           tuneGrid = xgb_grid.2.d, method = "xgbTree", nthread=1)$bestTune
    
    para <- list(nrounds.d0 = xgb_train_d0.2$nrounds, 
                 eta.d0 = xgb_train_d0.2$eta, 
                 max_depth.d0 = xgb_train_d0.2$max_depth, 
                 subsample.d0 = xgb_train_d0.2$subsample,
                 colsample_bytree.d0 = xgb_train_d0.2$colsample_bytree, 
                 min_child_weight.d0 = xgb_train_d0.2$min_child_weight, 
                 nrounds.d1 = xgb_train_d1.2$nrounds, 
                 eta.d1 = xgb_train_d1.2$eta, 
                 max_depth.d1 = xgb_train_d1.2$max_depth, 
                 subsample.d1 = xgb_train_d1.2$subsample,
                 colsample_bytree.d1 = xgb_train_d1.2$colsample_bytree, 
                 min_child_weight.d1 = xgb_train_d1.2$min_child_weight, 
                 nrounds.d = xgb_train_d.2$nrounds, 
                 eta.d = xgb_train_d.2$eta, 
                 max_depth.d = xgb_train_d.2$max_depth, 
                 subsample.d = xgb_train_d.2$subsample,
                 colsample_bytree.d = xgb_train_d.2$colsample_bytree, 
                 min_child_weight.d = xgb_train_d.2$min_child_weight)
    
  }
  
  return(para)

}
