# This function performs repeated cross-validation for regularized regression, 
# SVM, and xgboost. The output is a list of values for the hyperparameters of 
# the respective method for the model with attrition. 
# Author: Martin Wiegand
# Last changed: 26.04.2022
#
# In:
# - method: character (ML method to be used)
# - data: data frame
# - y, d, r, rt, rc, groupvar: character
# - xvars: list of characters (denoting factor variable names)
# - folds: double (number of folds for cross validation)
# - reps: double (number of repetitions for cross validation)
# - seed: double
# 
# Out: 
# - para: list of hyperparameters (which ones depends on method)


Best_Parameters_mis <- function(method, data, y, d, r, rt, rc, groupvar, xvars, 
                                folds, reps, seed = 1){
  
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
  foldslist.rt <- list()
  indexlist.rt <- list()
  indexoutlist.rt <- list()
  foldslist.rc <- list()
  indexlist.rc <- list()
  indexoutlist.rc <- list()
  foldslist.r_t <- list()
  indexlist.r_t <- list()
  indexoutlist.r_t <- list()
  foldslist.r_c <- list()
  indexlist.r_c <- list()
  indexoutlist.r_c <- list()
  counter <- 1
  data <- as.data.frame(fold(data, k = folds, cat_col = d, id_col = groupvar, 
                             method = 'n_dist'))
  data$.folds <- c()
  d.is.1 <- which(data[,d]==1)
  d.is.0 <- which(data[,d]==0)
  d.is.1.r.is.1 <- which(data[,d]==1 & data[,r]==1)
  d.is.0.r.is.1 <- which(data[,d]==0 & data[,r]==1)
  for (r in 1:reps) {
    foldslist.d[[r]] <- as.integer(fold(data, k = folds, cat_col = d, id_col = 
                                          groupvar, method = 'n_dist')$.folds)
    foldslist.d1[[r]] <- as.integer(foldslist.d[[r]][d.is.1.r.is.1])
    foldslist.d0[[r]] <- as.integer(foldslist.d[[r]][d.is.0.r.is.1])
    foldslist.rt[[r]] <- as.integer(foldslist.d[[r]])
    foldslist.rc[[r]] <- as.integer(foldslist.d[[r]])
    foldslist.r_t[[r]] <- as.integer(foldslist.d[[r]][d.is.1])
    foldslist.r_c[[r]] <- as.integer(foldslist.d[[r]][-d.is.1])
    for (i in 1:folds) {
      indexlist.d[[counter]] <- which(foldslist.d[[r]]!=i)
      indexlist.d1[[counter]] <- 
        intersect(which(foldslist.d[[r]]!=i),d.is.1.r.is.1)
      indexlist.d0[[counter]] <- 
        intersect(which(foldslist.d[[r]]!=i),d.is.0.r.is.1)
      indexlist.rt[[counter]] <- which(foldslist.d[[r]]!=i)
      indexlist.rc[[counter]] <- which(foldslist.d[[r]]!=i)
      indexlist.r_t[[counter]] <- intersect(which(foldslist.d[[r]]!=i),d.is.1)
      indexlist.r_c[[counter]] <- intersect(which(foldslist.d[[r]]!=i),d.is.0)
      indexoutlist.d[[counter]] <- (1:nrow(data))[-indexlist.d[[counter]]]
      indexoutlist.d1[[counter]] <- 
        intersect((1:nrow(data))[-indexlist.d[[counter]]],d.is.1.r.is.1)
      indexoutlist.d0[[counter]] <- 
        intersect((1:nrow(data))[-indexlist.d[[counter]]],d.is.0.r.is.1)
      indexoutlist.rt[[counter]] <- (1:nrow(data))[-indexlist.d[[counter]]]
      indexoutlist.rc[[counter]] <- (1:nrow(data))[-indexlist.d[[counter]]]
      indexoutlist.r_t[[counter]] <- 
        intersect((1:nrow(data))[-indexlist.d[[counter]]],d.is.1)
      indexoutlist.r_c[[counter]] <- 
        intersect((1:nrow(data))[-indexlist.d[[counter]]],d.is.0)
      counter <- counter + 1
    }
  }
  
  # ignore variables with variance 0
  X.d0.r1 <- dplyr::select(data[d.is.0.r.is.1,], one_of(xvars))
  X.d1.r1 <- dplyr::select(data[d.is.1.r.is.1,], one_of(xvars))
  var.not.0.d0.r1 <- !unlist(lapply(X.d0.r1, function(x) length(unique(x)) <2)) 
  var.not.0.d1.r1 <- !unlist(lapply(X.d1.r1, function(x) length(unique(x)) <2))
  # indices of variables with non-zero variance for both d=0 and d=1
  var.not.0 <- var.not.0.d0.r1 * var.not.0.d1.r1 
  # var1 + var2 + ...
  form_x <- paste(names(var.not.0[var.not.0==1]), collapse='+') 
  x <- names(var.not.0[var.not.0==1])
  
  rm(X.d0.r1, X.d1.r1)
  
  
  
  if(method == "Lasso" | method == "PostLasso" | method == "Ridge") {
    
    if(method == "Lasso" | method == "PostLasso") {
      alp <- 1
    } else {
      alp <- 0
    }
    
    data.d0 <- as.matrix(dplyr::select(data[d.is.0.r.is.1,], one_of(x)))
    y.d0 <- as.matrix(dplyr::select(data[d.is.0.r.is.1,], one_of(y)))
    
    data.d1 <- as.matrix(dplyr::select(data[d.is.1.r.is.1,], one_of(x)))
    y.d1 <- as.matrix(dplyr::select(data[d.is.1.r.is.1,], one_of(y)))
    
    data.d <- as.matrix(dplyr::select(data, one_of(x)))
    y.d <- as.matrix(dplyr::select(data, one_of(d)))
    
    data.rt <- data.d
    data.rc <- data.d
    y.rt <- as.matrix(dplyr::select(data, one_of(rt)))
    y.rc <- as.matrix(dplyr::select(data, one_of(rc)))
    
    data.r_t <- as.matrix(dplyr::select(data[d.is.1,], one_of(x)))
    y.r_t <- as.matrix(dplyr::select(data[d.is.1,], one_of(rt)))
    data.r_c <- as.matrix(dplyr::select(data[-d.is.1,], one_of(x)))
    y.r_c <- as.matrix(dplyr::select(data[-d.is.1,], one_of(rc)))
    
    cv.error.yd0 <- 0 # error vectors of length of all tried out lambda values
    cv.error.yd1 <- 0
    cv.error.d <- 0
    cv.error.rt <- 0
    cv.error.rc <- 0
    cv.error.r_t <- 0
    cv.error.r_c <- 0
    lambda.seq.yd0 <- NULL
    lambda.seq.yd1 <- NULL
    lambda.seq.d <- NULL
    lambda.seq.rt <- NULL
    lambda.seq.rc <- NULL
    lambda.seq.r_t <- NULL
    lambda.seq.r_c <- NULL
    for(r in 1:reps) {
      lasso.yd0 <- cv.glmnet(x=data.d0, y=y.d0, family="binomial", 
                             lambda=lambda.seq.yd0, alpha=alp, 
                             foldid = foldslist.d0[[r]], parallel = TRUE) 
      if (r==1) {
        while (lasso.yd0$lambda.min >= lasso.yd0$lambda[5] & 
               lasso.yd0$cvm[1] != lasso.yd0$cvm[length(lasso.yd0$cvm)] & 
               lasso.yd0$lambda[5]<100000) {
          lambda.seq.yd0 <- lasso.yd0$lambda*10
          lasso.yd0 <- cv.glmnet(x=data.d0, y=y.d0, family="binomial", 
                                 lambda=lambda.seq.yd0, alpha=alp, 
                                 foldid = foldslist.d0[[r]], parallel = TRUE)
        }
        lambda.seq.yd0 <- lasso.yd0$lambda
      }
      lasso.yd1 <- cv.glmnet(x=data.d1, y=y.d1, family="binomial", 
                             lambda=lambda.seq.yd1, alpha=alp, 
                             foldid = foldslist.d1[[r]], parallel = TRUE)
      if (r==1) {
        while (lasso.yd1$lambda.min >= lasso.yd1$lambda[5] & 
               lasso.yd1$cvm[1] != lasso.yd1$cvm[length(lasso.yd1$cvm)] & 
               lasso.yd1$lambda[5]<100000) {
          lambda.seq.yd1 <- lasso.yd1$lambda*10
          lasso.yd1 <- cv.glmnet(x=data.d1, y=y.d1, family="binomial", 
                                 lambda=lambda.seq.yd1, alpha=alp, 
                                 foldid = foldslist.d1[[r]], parallel = TRUE)
        }
        lambda.seq.yd1 <- lasso.yd1$lambda
      }
      lasso.d <- cv.glmnet(x=data.d, y=y.d, family="binomial", 
                           lambda=lambda.seq.d, alpha=alp, 
                           foldid = foldslist.d[[r]], parallel = TRUE)
      if (r==1) {
        while (lasso.d$lambda.min >= lasso.d$lambda[5] & 
               lasso.d$cvm[1] != lasso.d$cvm[length(lasso.d$cvm)] & 
               lasso.d$lambda[5]<100000) {
          lambda.seq.d <- lasso.d$lambda*10
          lasso.d <- cv.glmnet(x=data.d, y=y.d, family="binomial", 
                               lambda=lambda.seq.d, alpha=alp, 
                               foldid = foldslist.d[[r]], parallel = TRUE)
        }
        lambda.seq.d <- lasso.d$lambda
      }
      lasso.rt <- cv.glmnet(x=data.d, y=y.rt, family="binomial", 
                            lambda=lambda.seq.rt, alpha=alp, 
                            foldid = foldslist.rt[[r]], parallel = TRUE)
      if (r==1) {
        while (lasso.rt$lambda.min >= lasso.rt$lambda[5] & 
               lasso.rt$cvm[1] != lasso.rt$cvm[length(lasso.rt$cvm)] & 
               lasso.rt$lambda[5]<100000) {
          lambda.seq.rt <- lasso.rt$lambda*10
          lasso.rt <- cv.glmnet(x=data.d, y=y.rt, family="binomial", 
                                lambda=lambda.seq.rt, alpha=alp, 
                                foldid = foldslist.rt[[r]], parallel = TRUE)
        }
        lambda.seq.rt <- lasso.rt$lambda
      }
      lasso.rc <- cv.glmnet(x=data.d, y=y.rc, family="binomial", 
                            lambda=lambda.seq.rc, alpha=alp, 
                            foldid = foldslist.rc[[r]], parallel = TRUE)
      if (r==1) {
        while (lasso.rc$lambda.min >= lasso.rc$lambda[5] & 
               lasso.rc$cvm[1] != lasso.rc$cvm[length(lasso.rc$cvm)] & 
               lasso.rc$lambda[5]<100000) {
          lambda.seq.rc <- lasso.rc$lambda*10
          lasso.rc <- cv.glmnet(x=data.d, y=y.rc, family="binomial", 
                                lambda=lambda.seq.rc, alpha=alp, 
                                foldid = foldslist.rc[[r]], parallel = TRUE)
        }
        lambda.seq.rc <- lasso.rc$lambda
      }
      lasso.r_t <- cv.glmnet(x=data.r_t, y=y.r_t, family="binomial",
                             lambda=lambda.seq.r_t, alpha=alp, 
                             foldid = foldslist.r_t[[r]], parallel = TRUE)
      if (r==1) {
        while (lasso.r_t$lambda.min >= lasso.r_t$lambda[5] & 
               lasso.r_t$cvm[1] != lasso.r_t$cvm[length(lasso.r_t$cvm)] & 
               lasso.r_t$lambda[5]<100000) {
          lambda.seq.r_t <- lasso.r_t$lambda*10
          lasso.r_t <- cv.glmnet(x=data.r_t, y=y.r_t, family="binomial", 
                                 lambda=lambda.seq.r_t, alpha=alp, 
                                 foldid = foldslist.r_t[[r]], parallel = TRUE)
        }
        lambda.seq.r_t <- lasso.r_t$lambda
      }
      lasso.r_c <- cv.glmnet(x=data.r_c, y=y.r_c, family="binomial", 
                             lambda=lambda.seq.r_c, alpha=alp, 
                             foldid = foldslist.r_c[[r]], parallel = TRUE)
      if (r==1) {
        while (lasso.r_c$lambda.min >= lasso.r_c$lambda[5] & 
               lasso.r_c$cvm[1] != lasso.r_c$cvm[length(lasso.r_c$cvm)] & 
               lasso.r_c$lambda[5]<100000) {
          lambda.seq.r_c <- lasso.r_c$lambda*10
          lasso.r_c <- cv.glmnet(x=data.r_c, y=y.r_c, family="binomial", 
                                 lambda=lambda.seq.r_c, alpha=alp, 
                                 foldid = foldslist.r_c[[r]], parallel = TRUE)
        }
        lambda.seq.r_c <- lasso.r_c$lambda
      }
      cv.error.yd0 <- cv.error.yd0 + lasso.yd0$cvm
      cv.error.yd1 <- cv.error.yd1 + lasso.yd1$cvm
      cv.error.d <- cv.error.d + lasso.d$cvm
      cv.error.rt <- cv.error.rt + lasso.rt$cvm
      cv.error.rc <- cv.error.rc + lasso.rc$cvm
      cv.error.r_t <- cv.error.r_t + lasso.r_t$cvm
      cv.error.r_c <- cv.error.r_c + lasso.r_c$cvm
      print(r)
    }
    lambda.yd0 <- lasso.yd0$lambda[which.min(cv.error.yd0)]
    lambda.yd1 <- lasso.yd1$lambda[which.min(cv.error.yd1)]
    lambda.d <- lasso.d$lambda[which.min(cv.error.d)]
    lambda.rt <- lasso.rt$lambda[which.min(cv.error.rt)]
    lambda.rc <- lasso.rc$lambda[which.min(cv.error.rc)]
    lambda.r_t <- lasso.r_t$lambda[which.min(cv.error.r_t)]
    lambda.r_c <- lasso.r_c$lambda[which.min(cv.error.r_c)]
    
    para <- list(lambda.yd0=lambda.yd0, lambda.yd1=lambda.yd1, 
                 lambda.d=lambda.d, lambda.rt=lambda.rt, lambda.rc=lambda.rc, 
                 lambda.r_t=lambda.r_t, lambda.r_c=lambda.r_c)
    
  }
  
  
  if(method == "Elnet") {
    
    alphas <- seq(0.1, 0.9, by=0.1)
    
    data.d0 <- as.matrix(dplyr::select(data[d.is.0.r.is.1,], one_of(x)))
    y.d0 <- as.matrix(dplyr::select(data[d.is.0.r.is.1,], one_of(y)))
    
    data.d1 <- as.matrix(dplyr::select(data[d.is.1.r.is.1,], one_of(x)))
    y.d1 <- as.matrix(dplyr::select(data[d.is.1.r.is.1,], one_of(y)))
    
    data.d <- as.matrix(dplyr::select(data, one_of(x)))
    y.d <- as.matrix(dplyr::select(data, one_of(d)))
    
    data.rt <- data.d
    data.rc <- data.d
    y.rt <- as.matrix(dplyr::select(data, one_of(rt)))
    y.rc <- as.matrix(dplyr::select(data, one_of(rc)))
    
    data.r_t <- as.matrix(dplyr::select(data[d.is.1,], one_of(x)))
    y.r_t <- as.matrix(dplyr::select(data[d.is.1,], one_of(rt)))
    data.r_c <- as.matrix(dplyr::select(data[-d.is.1,], one_of(x)))
    y.r_c <- as.matrix(dplyr::select(data[-d.is.1,], one_of(rc)))
    
    lambdamat <- matrix(0, nrow = 7, ncol = length(alphas))
    errormat <- matrix(0, nrow = 7, ncol = length(alphas))
    for(alp in alphas) {
      cv.error.yd0 <- 0 
      cv.error.yd1 <- 0
      cv.error.d <- 0
      cv.error.rt <- 0
      cv.error.rc <- 0
      cv.error.r_t <- 0
      cv.error.r_c <- 0
      lambda.seq.yd0 <- NULL
      lambda.seq.yd1 <- NULL
      lambda.seq.d <- NULL
      lambda.seq.rt <- NULL
      lambda.seq.rc <- NULL
      lambda.seq.r_t <- NULL
      lambda.seq.r_c <- NULL
      for(r in 1:reps) {
        lasso.yd0 <- cv.glmnet(x=data.d0, y=y.d0, family="binomial", 
                               lambda=lambda.seq.yd0, alpha=alp, 
                               foldid = foldslist.d0[[r]], parallel = TRUE) 
        if (r==1) {
          while (lasso.yd0$lambda.min >= lasso.yd0$lambda[5] & 
                 lasso.yd0$cvm[1] != lasso.yd0$cvm[length(lasso.yd0$cvm)] & 
                 lasso.yd0$lambda[5]<100000) {
            lambda.seq.yd0 <- lasso.yd0$lambda*10
            lasso.yd0 <- cv.glmnet(x=data.d0, y=y.d0, family="binomial", 
                                   lambda=lambda.seq.yd0, alpha=alp, 
                                   foldid = foldslist.d0[[r]], parallel = TRUE)
          }
          lambda.seq.yd0 <- lasso.yd0$lambda
        }
        lasso.yd1 <- cv.glmnet(x=data.d1, y=y.d1, family="binomial", 
                               lambda=lambda.seq.yd1, alpha=alp, 
                               foldid = foldslist.d1[[r]], parallel = TRUE)
        if (r==1) {
          while (lasso.yd1$lambda.min >= lasso.yd1$lambda[5] & 
                 lasso.yd1$cvm[1] != lasso.yd1$cvm[length(lasso.yd1$cvm)] & 
                 lasso.yd1$lambda[5]<100000) {
            lambda.seq.yd1 <- lasso.yd1$lambda*10
            lasso.yd1 <- cv.glmnet(x=data.d1, y=y.d1, family="binomial",
                                   lambda=lambda.seq.yd1, alpha=alp, 
                                   foldid = foldslist.d1[[r]], parallel = TRUE)
          }
          lambda.seq.yd1 <- lasso.yd1$lambda
        }
        lasso.d <- cv.glmnet(x=data.d, y=y.d, family="binomial",
                             lambda=lambda.seq.d, alpha=alp, 
                             foldid = foldslist.d[[r]], parallel = TRUE)
        if (r==1) {
          while (lasso.d$lambda.min >= lasso.d$lambda[5] & 
                 lasso.d$cvm[1] != lasso.d$cvm[length(lasso.d$cvm)] & 
                 lasso.d$lambda[5]<100000) {
            lambda.seq.d <- lasso.d$lambda*10
            lasso.d <- cv.glmnet(x=data.d, y=y.d, family="binomial", 
                                 lambda=lambda.seq.d, alpha=alp, 
                                 foldid = foldslist.d[[r]], parallel = TRUE)
          }
          lambda.seq.d <- lasso.d$lambda
        }
        lasso.rt <- cv.glmnet(x=data.d, y=y.rt, family="binomial", 
                              lambda=lambda.seq.rt, alpha=alp,
                              foldid = foldslist.rt[[r]], parallel = TRUE)
        if (r==1) {
          while (lasso.rt$lambda.min >= lasso.rt$lambda[5] & 
                 lasso.rt$cvm[1] != lasso.rt$cvm[length(lasso.rt$cvm)] & 
                 lasso.rt$lambda[5]<100000) {
            lambda.seq.rt <- lasso.rt$lambda*10
            lasso.rt <- cv.glmnet(x=data.d, y=y.rt, family="binomial", 
                                  lambda=lambda.seq.rt, alpha=alp,
                                  foldid = foldslist.rt[[r]], parallel = TRUE)
          }
          lambda.seq.rt <- lasso.rt$lambda
        }
        lasso.rc <- cv.glmnet(x=data.d, y=y.rc, family="binomial", 
                              lambda=lambda.seq.rc, alpha=alp, 
                              foldid = foldslist.rc[[r]], parallel = TRUE)
        if (r==1) {
          while (lasso.rc$lambda.min >= lasso.rc$lambda[5] & 
                 lasso.rc$cvm[1] != lasso.rc$cvm[length(lasso.rc$cvm)] & 
                 lasso.rc$lambda[5]<100000) {
            lambda.seq.rc <- lasso.rc$lambda*10
            lasso.rc <- cv.glmnet(x=data.d, y=y.rc, family="binomial",
                                  lambda=lambda.seq.rc, alpha=alp, 
                                  foldid = foldslist.rc[[r]], parallel = TRUE)
          }
          lambda.seq.rc <- lasso.rc$lambda
        }
        lasso.r_t <- cv.glmnet(x=data.r_t, y=y.r_t, family="binomial", 
                               lambda=lambda.seq.r_t, alpha=alp, 
                               foldid = foldslist.r_t[[r]], parallel = TRUE)
        if (r==1) {
          while (lasso.r_t$lambda.min >= lasso.r_t$lambda[5] & 
                 lasso.r_t$cvm[1] != lasso.r_t$cvm[length(lasso.r_t$cvm)] & 
                 lasso.r_t$lambda[5]<100000) {
            lambda.seq.r_t <- lasso.r_t$lambda*10
            lasso.r_t <- cv.glmnet(x=data.r_t, y=y.r_t, family="binomial", 
                                   lambda=lambda.seq.r_t, alpha=alp, 
                                   foldid = foldslist.r_t[[r]], parallel = TRUE)
          }
          lambda.seq.r_t <- lasso.r_t$lambda
        }
        lasso.r_c <- cv.glmnet(x=data.r_c, y=y.r_c, family="binomial", 
                               lambda=lambda.seq.r_c, alpha=alp, 
                               foldid = foldslist.r_c[[r]], parallel = TRUE)
        if (r==1) {
          while (lasso.r_c$lambda.min >= lasso.r_c$lambda[5] & 
                 lasso.r_c$cvm[1] != lasso.r_c$cvm[length(lasso.r_c$cvm)] & 
                 lasso.r_c$lambda[5]<100000) {
            lambda.seq.r_c <- lasso.r_c$lambda*10
            lasso.r_c <- cv.glmnet(x=data.r_c, y=y.r_c, family="binomial", 
                                   lambda=lambda.seq.r_c, alpha=alp, 
                                   foldid = foldslist.r_c[[r]], parallel = TRUE)
          }
          lambda.seq.r_c <- lasso.r_c$lambda
        }
        cv.error.yd0 <- cv.error.yd0 + lasso.yd0$cvm
        cv.error.yd1 <- cv.error.yd1 + lasso.yd1$cvm
        cv.error.d <- cv.error.d + lasso.d$cvm
        cv.error.rt <- cv.error.rt + lasso.rt$cvm
        cv.error.rc <- cv.error.rc + lasso.rc$cvm
        cv.error.r_t <- cv.error.r_t + lasso.r_t$cvm
        cv.error.r_c <- cv.error.r_c + lasso.r_c$cvm
        print(r)
      }
      lambdamat[1,which(alphas == alp)] <- 
        lasso.yd0$lambda[which.min(cv.error.yd0)]
      lambdamat[2,which(alphas == alp)] <- 
        lasso.yd1$lambda[which.min(cv.error.yd1)]
      lambdamat[3,which(alphas == alp)] <- 
        lasso.d$lambda[which.min(cv.error.d)]
      lambdamat[4,which(alphas == alp)] <- 
        lasso.rt$lambda[which.min(cv.error.rt)]
      lambdamat[5,which(alphas == alp)] <- 
        lasso.rc$lambda[which.min(cv.error.rc)]
      lambdamat[6,which(alphas == alp)] <- 
        lasso.r_t$lambda[which.min(cv.error.r_t)]
      lambdamat[7,which(alphas == alp)] <- 
        lasso.r_c$lambda[which.min(cv.error.r_c)]
      errormat[1,which(alphas == alp)] <- min(cv.error.yd0)
      errormat[2,which(alphas == alp)] <- min(cv.error.yd1)
      errormat[3,which(alphas == alp)] <- min(cv.error.d)
      errormat[4,which(alphas == alp)] <- min(cv.error.rt)
      errormat[5,which(alphas == alp)] <- min(cv.error.rc)
      errormat[6,which(alphas == alp)] <- min(cv.error.r_t)
      errormat[7,which(alphas == alp)] <- min(cv.error.r_c)
    }
    
    alpha.yd0 <- alphas[which(errormat == rowMins(errormat, rows = 1),
                              arr.ind = TRUE)[2]]
    lambda.yd0 <- lambdamat[1, which(errormat == rowMins(errormat, rows = 1), 
                                     arr.ind = TRUE)[2]]
    alpha.yd1 <- alphas[which(errormat == rowMins(errormat, rows = 2), 
                              arr.ind = TRUE)[2]]
    lambda.yd1 <- lambdamat[2, which(errormat == rowMins(errormat, rows = 2), 
                                     arr.ind = TRUE)[2]]
    alpha.d <- alphas[which(errormat == rowMins(errormat, rows = 3), 
                            arr.ind = TRUE)[2]]
    lambda.d <- lambdamat[3, which(errormat == rowMins(errormat, rows = 3), 
                                   arr.ind = TRUE)[2]]
    alpha.rt <- alphas[which(errormat == rowMins(errormat, rows = 4), 
                             arr.ind = TRUE)[2]]
    lambda.rt <- lambdamat[4, which(errormat == rowMins(errormat, rows = 4), 
                                    arr.ind = TRUE)[2]]
    alpha.rc <- alphas[which(errormat == rowMins(errormat, rows = 5), 
                             arr.ind = TRUE)[2]]
    lambda.rc <- lambdamat[5, which(errormat == rowMins(errormat, rows = 5), 
                                    arr.ind = TRUE)[2]]
    alpha.r_t <- alphas[which(errormat == rowMins(errormat, rows = 6), 
                              arr.ind = TRUE)[2]]
    lambda.r_t <- lambdamat[6, which(errormat == rowMins(errormat, rows = 6), 
                                     arr.ind = TRUE)[2]]
    alpha.r_c <- alphas[which(errormat == rowMins(errormat, rows = 7), 
                              arr.ind = TRUE)[2]]
    lambda.r_c <- lambdamat[7, which(errormat == rowMins(errormat, rows = 7), 
                                     arr.ind = TRUE)[2]]
    
    para <- list(lambda.yd0=lambda.yd0, lambda.yd1=lambda.yd1, 
                 lambda.d=lambda.d, lambda.rt=lambda.rt, lambda.rc=lambda.rc, 
                 lambda.r_t=lambda.r_t, lambda.r_c=lambda.r_c, 
                 alpha.yd0=alpha.yd0, alpha.yd1=alpha.yd1, alpha.d=alpha.d, 
                 alpha.rt=alpha.rt, alpha.rc=alpha.rc, alpha.r_t=alpha.r_t, 
                 alpha.r_c=alpha.r_c)
    
  }
  
  
  if(method == "SVM") {
    
    costvector <- c(0.1, 0.25, 0.5, 0.75, 1, 2, 3, 5)
    gammavector <- c(0.001, 0.003, 0.006, 0.01, 0.02, 0.04, 0.1)
    paragrid <- expand.grid(C = costvector, sigma = gammavector)
    
    data.y <- as.data.frame(dplyr::select(data, one_of(x)))
    y.y <- as.factor(data[,y]) 
    levels(y.y) <- c("X0", "X1")
    
    y.d <- as.factor(data[,d])
    levels(y.d) <- c("X0", "X1")
    
    y.rt <- as.factor(data[,rt])
    levels(y.rt) <- c("X0", "X1")
    
    y.rc <- as.factor(data[,rc])
    levels(y.rc) <- c("X0", "X1")
    
    svm_trcontrol.d0 <- 
      trainControl(index = indexlist.d0, indexOut = indexoutlist.d0, 
                   indexFinal = d.is.0.r.is.1, verboseIter = TRUE, 
                   returnData = FALSE, summaryFunction=twoClassSummary,	
                   classProbs=TRUE, returnResamp = "all", allowParallel = TRUE)
    
    svm.tune.d0 <- 
      train(x = data.y, y = y.y, method = "svmRadial", metric = "ROC", 
            tuneGrid=paragrid, scale = FALSE, trControl=svm_trcontrol.d0, 
            nthread=1)$bestTune
    
    svm_trcontrol.d1 <- 
      trainControl(index = indexlist.d1, indexOut = indexoutlist.d1, 
                   indexFinal = d.is.1.r.is.1, verboseIter = TRUE, 
                   returnData = FALSE, summaryFunction=twoClassSummary,
                   classProbs=TRUE, returnResamp = "all", allowParallel = TRUE)
    
    svm.tune.d1 <- 
      train(x = data.y, y = y.y, method = "svmRadial", metric = "ROC", 
            tuneGrid=paragrid, scale = FALSE, trControl=svm_trcontrol.d1, 
            nthread=1)$bestTune
    
    svm_trcontrol.d <- 
      trainControl(index = indexlist.d, indexOut = indexoutlist.d, 
                   verboseIter = TRUE, returnData = FALSE, 
                   summaryFunction=twoClassSummary,	classProbs=TRUE, 
                   returnResamp = "all", allowParallel = TRUE)
    svm.tune.d <- 
      train(x = data.y, y = y.d, method = "svmRadial", metric = "ROC", 
            tuneGrid=paragrid, scale = FALSE, trControl=svm_trcontrol.d, 
            nthread=1)$bestTune
    
    svm_trcontrol.rt <- 
      trainControl(index = indexlist.rt, indexOut = indexoutlist.rt, 
                   verboseIter = TRUE, returnData = FALSE, 
                   summaryFunction=twoClassSummary,	classProbs=TRUE, 
                   returnResamp = "all", allowParallel = TRUE)
    svm.tune.rt <- 
      train(x = data.y, y = y.rt, method = "svmRadial", metric = "ROC", 
            tuneGrid=paragrid, scale = FALSE, trControl=svm_trcontrol.rt, 
            nthread=1)$bestTune
    
    svm_trcontrol.rc <- 
      trainControl(index = indexlist.rc, indexOut = indexoutlist.rc, 
                   verboseIter = TRUE, returnData = FALSE, 
                   summaryFunction=twoClassSummary,	classProbs=TRUE, 
                   returnResamp = "all", allowParallel = TRUE)
    svm.tune.rc <- 
      train(x = data.y, y = y.rc, method = "svmRadial", metric = "ROC", 
            tuneGrid=paragrid, scale = FALSE, trControl=svm_trcontrol.rc, 
            nthread=1)$bestTune
    
    svm_trcontrol.r_t <- 
      trainControl(index = indexlist.r_t, indexOut = indexoutlist.r_t, 
                   indexFinal = d.is.1, verboseIter = TRUE, returnData = FALSE, 
                   summaryFunction=twoClassSummary,	classProbs=TRUE, 
                   returnResamp = "all", allowParallel = TRUE)
    svm.tune.r_t <- 
      train(x = data.y, y = y.rt, method = "svmRadial", metric = "ROC", 
            tuneGrid=paragrid, scale = FALSE, trControl=svm_trcontrol.r_t, 
            nthread=1)$bestTune
    
    svm_trcontrol.r_c <- 
      trainControl(index = indexlist.r_c, indexOut = indexoutlist.r_c,
                   indexFinal = d.is.0, verboseIter = TRUE, returnData = FALSE, 
                   summaryFunction=twoClassSummary,	classProbs=TRUE, 
                   returnResamp = "all", allowParallel = TRUE)
    svm.tune.r_c <- 
      train(x = data.y, y = y.rc, method = "svmRadial", metric = "ROC", 
            tuneGrid=paragrid, scale = FALSE, trControl=svm_trcontrol.r_c, 
            nthread=1)$bestTune
    
    para <- list(cost_hsd0 = svm.tune.d0$C, 
                 gamma_hsd0 = svm.tune.d0$sigma, 
                 cost_hsd1 = svm.tune.d1$C, 
                 gamma_hsd1 = svm.tune.d1$sigma,
                 cost_treat = svm.tune.d$C, 
                 gamma_treat = svm.tune.d$sigma,
                 cost_rt = svm.tune.rt$C, 
                 gamma_rt = svm.tune.rt$sigma,
                 cost_rc = svm.tune.rc$C, 
                 gamma_rc = svm.tune.rc$sigma,
                 cost_r_t = svm.tune.r_t$C, 
                 gamma_r_t = svm.tune.r_t$sigma,
                 cost_r_c = svm.tune.r_c$C, 
                 gamma_r_c = svm.tune.r_c$sigma)
    
  }
  
  
  if(method == "Boosting") {
    
    data.y <- as.data.frame(dplyr::select(data, one_of(x)))
    y.y <- as.factor(data[,y]) 
    levels(y.y) <- c("X0", "X1")
    
    y.d <- as.factor(data[,d])
    levels(y.d) <- c("X0", "X1")
    
    y.rt <- as.factor(data[,rt])
    levels(y.rt) <- c("X0", "X1")
    
    y.rc <- as.factor(data[,rc])
    levels(y.rc) <- c("X0", "X1")
    
    
    # set up the cross-validated hyper-parameter search
    
    # First, tune only eta and nrounds
    xgb_grid.1 <- 
      expand.grid(nrounds = seq(from=1, to=500), eta = 10**(0:-3), 
                  max_depth = c(1,2,4,7,10,15), gamma = 0, subsample = 1,
                  colsample_bytree = 1, min_child_weight = 1)
    
    xgb_trcontrol.d <- 
      trainControl(index = indexlist.d, indexOut = indexoutlist.d, 
                   verboseIter = TRUE, returnData = FALSE, returnResamp = "all", 
                   classProbs = TRUE, summaryFunction = twoClassSummary, 
                   allowParallel = TRUE)
    
    xgb_trcontrol.d0 <- 
      trainControl(index = indexlist.d0, indexOut = indexoutlist.d0, 
                   indexFinal = d.is.0.r.is.1, verboseIter = TRUE, 
                   returnData = FALSE, returnResamp = "all", classProbs = TRUE, 
                   summaryFunction = twoClassSummary, allowParallel = TRUE)
    
    xgb_trcontrol.d1 <- 
      trainControl(index = indexlist.d1, indexOut = indexoutlist.d1, 
                   indexFinal = d.is.1.r.is.1, verboseIter = TRUE, 
                   returnData = FALSE, returnResamp = "all", classProbs = TRUE, 
                   summaryFunction = twoClassSummary, allowParallel = TRUE)
    
    xgb_trcontrol.rt <- 
      trainControl(index = indexlist.rt, indexOut = indexoutlist.rt, 
                   verboseIter = TRUE, returnData = FALSE, returnResamp = "all", 
                   classProbs = TRUE, summaryFunction = twoClassSummary,
                   allowParallel = TRUE)
    
    xgb_trcontrol.rc <- 
      trainControl(index = indexlist.rc, indexOut = indexoutlist.rc, 
                   verboseIter = TRUE, returnData = FALSE, returnResamp = "all", 
                   classProbs = TRUE, summaryFunction = twoClassSummary, 
                   allowParallel = TRUE)
    
    xgb_trcontrol.r_t <- 
      trainControl(index = indexlist.r_t, indexOut = indexoutlist.r_t, 
                   indexFinal = d.is.1, verboseIter = TRUE, returnData = FALSE, 
                   returnResamp = "all", classProbs = TRUE, 
                   summaryFunction = twoClassSummary, allowParallel = TRUE)
    
    xgb_trcontrol.r_c <- 
      trainControl(index = indexlist.r_c, indexOut = indexoutlist.r_c, 
                   indexFinal = d.is.0, verboseIter = TRUE, returnData = FALSE, 
                   returnResamp = "all", classProbs = TRUE, 
                   summaryFunction = twoClassSummary, allowParallel = TRUE)
    
    xgb_train_d0.1 <-
      train(x = data.y, y = y.y, trControl = xgb_trcontrol.d0, 
            tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.d0     <- xgb_train_d0.1$nrounds
    eta.d0         <- xgb_train_d0.1$eta
    max_depth.d0   <- xgb_train_d0.1$max_depth
    
    xgb_train_d1.1 <- 
      train(x = data.y, y = y.y, trControl = xgb_trcontrol.d1, 
            tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.d1     <- xgb_train_d1.1$nrounds
    eta.d1         <- xgb_train_d1.1$eta
    max_depth.d1   <- xgb_train_d1.1$max_depth
    
    xgb_train_d.1 <- 
      train(x = data.y, y = y.d, trControl = xgb_trcontrol.d, 
            tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.d     <- xgb_train_d.1$nrounds
    eta.d         <- xgb_train_d.1$eta
    max_depth.d   <- xgb_train_d.1$max_depth
    
    xgb_train_rt.1 <- 
      train(x = data.y, y = y.rt, trControl = xgb_trcontrol.rt, 
            tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.rt     <- xgb_train_rt.1$nrounds
    eta.rt         <- xgb_train_rt.1$eta
    max_depth.rt   <- xgb_train_rt.1$max_depth
    
    xgb_train_rc.1 <- 
      train(x = data.y, y = y.rc, trControl = xgb_trcontrol.rc, 
            tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.rc     <- xgb_train_rc.1$nrounds
    eta.rc         <- xgb_train_rc.1$eta
    max_depth.rc   <- xgb_train_rc.1$max_depth
    
    xgb_train_r_t.1 <- 
      train(x = data.y, y = y.rt, trControl = xgb_trcontrol.r_t, 
            tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.r_t     <- xgb_train_r_t.1$nrounds
    eta.r_t         <- xgb_train_r_t.1$eta
    max_depth.r_t   <- xgb_train_r_t.1$max_depth
    
    xgb_train_r_c.1 <- 
      train(x = data.y, y = y.rc, trControl = xgb_trcontrol.r_c, 
            tuneGrid = xgb_grid.1, method = "xgbTree", nthread=1)$bestTune
    nrounds.r_c     <- xgb_train_r_c.1$nrounds
    eta.r_c         <- xgb_train_r_c.1$eta
    max_depth.r_c   <- xgb_train_r_c.1$max_depth
    
    # Second, fix the optimal values from the first step and tune max_depth, 
    # gamma, subsample, colsample_bytree
    
    xgb_grid.2.d0 <- 
      expand.grid(nrounds = nrounds.d0, eta = eta.d0, max_depth = max_depth.d0, 
                  gamma = 0, subsample = c(0.4,0.7,1), 
                  colsample_bytree = c(0.5,0.7,1), 
                  min_child_weight = c(0.5,1,2,4))
    
    xgb_grid.2.d1 <- 
      expand.grid(nrounds = nrounds.d1, eta = eta.d1, max_depth = max_depth.d1,
                  gamma = 0, subsample = c(0.4,0.7,1),
                  colsample_bytree = c(0.5,0.7,1), 
                  min_child_weight = c(0.5,1,2,4))
    
    xgb_grid.2.d <- 
      expand.grid(nrounds = nrounds.d, eta = eta.d, max_depth = max_depth.d,
                  gamma = 0, subsample = c(0.4,0.7,1),
                  colsample_bytree = c(0.5,0.7,1), 
                  min_child_weight = c(0.5,1,2,4))
    
    xgb_grid.2.rt <- 
      expand.grid(nrounds = nrounds.rt, eta = eta.rt, max_depth = max_depth.rt, 
                  gamma = 0, subsample = c(0.4,0.7,1),
                  colsample_bytree = c(0.5,0.7,1), 
                  min_child_weight = c(0.5,1,2,4))
    
    xgb_grid.2.rc <- 
      expand.grid(nrounds = nrounds.rc, eta = eta.rc, max_depth = max_depth.rc, 
                  gamma = 0, subsample = c(0.4,0.7,1),
                  colsample_bytree = c(0.5,0.7,1), 
                  min_child_weight = c(0.5,1,2,4))
    
    xgb_grid.2.r_t <- 
      expand.grid(nrounds = nrounds.r_t, eta = eta.r_t, 
                  max_depth = max_depth.r_t, gamma = 0, 
                  subsample = c(0.4,0.7,1), colsample_bytree = c(0.5,0.7,1), 
                  min_child_weight = c(0.5,1,2,4))
    
    xgb_grid.2.r_c <- 
      expand.grid(nrounds = nrounds.r_c, eta = eta.r_c, 
                  max_depth = max_depth.r_c, gamma = 0, 
                  subsample = c(0.4,0.7,1), colsample_bytree = c(0.5,0.7,1), 
                  min_child_weight = c(0.5,1,2,4))
    
    
    # train the model for each parameter combination in the grid, using CV to evaluate
    
    xgb_train_d0.2 <- 
      train(x = data.y, y = y.y, trControl = xgb_trcontrol.d0, 
            tuneGrid = xgb_grid.2.d0, method = "xgbTree", nthread=1)$bestTune
    
    xgb_train_d1.2 <- 
      train(x = data.y, y = y.y, trControl = xgb_trcontrol.d1, 
            tuneGrid = xgb_grid.2.d1, method = "xgbTree", nthread=1)$bestTune
    
    xgb_train_d.2 <- 
      train(x = data.y, y = y.d, trControl = xgb_trcontrol.d, 
            tuneGrid = xgb_grid.2.d, method = "xgbTree", nthread=1)$bestTune
    
    xgb_train_rt.2 <- 
      train(x = data.y, y = y.rt, trControl = xgb_trcontrol.rt, 
            tuneGrid = xgb_grid.2.rt, method = "xgbTree", nthread=1)$bestTune
    
    xgb_train_rc.2 <- 
      train(x = data.y, y = y.rc, trControl = xgb_trcontrol.rc, 
            tuneGrid = xgb_grid.2.rc, method = "xgbTree", nthread=1)$bestTune
    
    xgb_train_r_t.2 <- 
      train(x = data.y, y = y.rt, trControl = xgb_trcontrol.r_t, 
            tuneGrid = xgb_grid.2.r_t, method = "xgbTree", nthread=1)$bestTune
    
    xgb_train_r_c.2 <- 
      train(x = data.y, y = y.rc, trControl = xgb_trcontrol.r_c, 
            tuneGrid = xgb_grid.2.r_c, method = "xgbTree", nthread=1)$bestTune
    
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
                 min_child_weight.d = xgb_train_d.2$min_child_weight, 
                 nrounds.rt = xgb_train_rt.2$nrounds, 
                 eta.rt = xgb_train_rt.2$eta, 
                 max_depth.rt = xgb_train_rt.2$max_depth, 
                 subsample.rt = xgb_train_rt.2$subsample,
                 colsample_bytree.rt = xgb_train_rt.2$colsample_bytree, 
                 min_child_weight.rt = xgb_train_rt.2$min_child_weight, 
                 nrounds.rc = xgb_train_rc.2$nrounds, 
                 eta.rc = xgb_train_rc.2$eta, 
                 max_depth.rc = xgb_train_rc.2$max_depth, 
                 subsample.rc = xgb_train_rc.2$subsample,
                 colsample_bytree.rc = xgb_train_rc.2$colsample_bytree, 
                 min_child_weight.rc = xgb_train_rc.2$min_child_weight, 
                 nrounds.r_t = xgb_train_r_t.2$nrounds, 
                 eta.r_t = xgb_train_r_t.2$eta, 
                 max_depth.r_t = xgb_train_r_t.2$max_depth, 
                 subsample.r_t = xgb_train_r_t.2$subsample,
                 colsample_bytree.r_t = xgb_train_r_t.2$colsample_bytree, 
                 min_child_weight.r_t = xgb_train_r_t.2$min_child_weight, 
                 nrounds.r_c = xgb_train_r_c.2$nrounds, 
                 eta.r_c = xgb_train_r_c.2$eta, 
                 max_depth.r_c = xgb_train_r_c.2$max_depth, 
                 subsample.r_c = xgb_train_r_c.2$subsample,
                 colsample_bytree.r_c = xgb_train_r_c.2$colsample_bytree, 
                 min_child_weight.r_c = xgb_train_r_c.2$min_child_weight)
    
  }
  
  return(para)
  
}