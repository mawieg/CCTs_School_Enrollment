# This function runs the DML procedure with non-missing outcomes for a number of 
# pre-specified ML methods. It produces the "result" matrix and the vector 
# "delta.0" of relative importances. 
# Author: Martin Wiegand 
# Partially based on code provided by V.Chernozhukov, D. Chetverikov, 
# M. Demirer, E. Duflo, C. Hansen, W. Newey, J. Robins for their paper 
# "Double/debiased machine learning for treatment and structural parameters", 
# Econometrics Journal (2018).
# Last changed: 26.04.2022


methods         <- c("Lasso", "Ridge", "Elnet", "Forest", "Boosting","SVM")

large.list <- foreach(ite = 1:iterations, .errorhandling='remove', 
                      .packages = c('tidyverse', 'randomForest', 'partykit', 
                                    'Matching', 'CausalGAM', 'survey', 'glmnet', 
                                    'e1071', 'caret', 'splines2', 'foreign', 
                                    'quantreg', 'mnormt', 'gbm', 'glmnet', 
                                    'MASS', 'rpart', 'doParallel', 'sandwich', 
                                    'hdm', 'matrixStats', 'quadprog', 'lsei', 
                                    'xgboost', 'groupdata2', 
                                    'multiwayvcov')) %dopar% { 
  set.seed(12345+ite)
  res <- DML(databig, y, d, groupvar, xx, xo, xL, methods=methods, nfold=nfolds, 
             arguments=arguments, outcomename=outcomename, silent=FALSE)
  res <- res
}

result      <- matrix(0,9, length(methods)+1)
colnames(result) <- cbind(t(methods), "Best")
rownames(result) <- cbind("Median ATE", "bse", "pval", 
                          "Median ATEN", "bse", "pval", 
                          "RMSE[Y|X, D=0]", "RMSE[Y|X, D=1]", "RMSE[D|X]")

N <- dim(databig)[1]
M <- length(methods)
for (k in 1:(M+1)) {
  TE.mat <- TET.mat <- TEN.mat <- matrix(0,N,iterations)
  for(ite in 1:iterations) {
    TE.mat[,ite] <- large.list[[ite]]$TE[,k]
    TEN.mat[,ite] <- large.list[[ite]]$TEN[,k]
  }
  
  # Wild cluster bootstrap
  set.seed(12345)
  runs.per.ite <- 100000 / iterations
  TE.boot <- TEN.boot <- rep(0,runs.per.ite*iterations)
  for (ite in 1:iterations){
    groups <- large.list[[ite]]$groupID
    epsilon.TE <- TE.mat[,ite] - mean(TE.mat[,ite], na.rm = TRUE)
    epsilon.TEN <- TEN.mat[,ite] - mean(TEN.mat[,ite], na.rm = TRUE)
    for (run in 1:runs.per.ite) {
      mammen.draw <- 
        rbernoulli(n = max(groups), 
                   p = (sqrt(5)-1)/(2*sqrt(5))) * sqrt(5) + (1-sqrt(5))/2
      boot.sample.TE <- 
        mean(TE.mat[,ite], na.rm = TRUE) + epsilon.TE*mammen.draw[groups]
      boot.sample.TEN <- 
        mean(TEN.mat[,ite], na.rm = TRUE) + epsilon.TEN*mammen.draw[groups]
      TE.boot[(ite-1)*runs.per.ite+run] <- mean(boot.sample.TE, na.rm = TRUE)
      TEN.boot[(ite-1)*runs.per.ite+run] <- mean(boot.sample.TEN, na.rm = TRUE)
    }
  }
  
  var.TE <- var.TEN <- rep(0,iterations)
  for (ite in 1:iterations) {
    var.TE[ite] <- var(TE.boot[((ite-1)*runs.per.ite+1):(ite*runs.per.ite)]) + 
      (median(TE.boot[((ite-1)*runs.per.ite+1):(ite*runs.per.ite)]) - 
         median(TE.boot))^2
    var.TEN[ite] <- var(TEN.boot[((ite-1)*runs.per.ite+1):(ite*runs.per.ite)]) + 
      (median(TEN.boot[((ite-1)*runs.per.ite+1):(ite*runs.per.ite)]) - 
         median(TEN.boot))^2
  }
  
  result[1,k] <- median(colMeans(TE.mat, na.rm = TRUE))
  result[2,k] <- sqrt(median(var.TE))
  result[3,k] <- 2 * (1 - pnorm(abs(result[1,k]/result[2,k])))
  result[4,k] <- median(colMeans(TEN.mat, na.rm = TRUE))
  result[5,k] <- sqrt(median(var.TEN))
  result[6,k] <- 2 * (1 - pnorm(abs(result[4,k]/result[5,k])))
}

for (k in 1:(M+1)) {
  RMSE.vector.y0 <- rep(0, iterations)
  RMSE.vector.y1 <- rep(0, iterations)
  RMSE.vector.d <- rep(0, iterations)
  for (ite in 1:iterations) {
    RMSE.vector.y0[ite] <- large.list[[ite]]$RMSE[1,k]
    RMSE.vector.y1[ite] <- large.list[[ite]]$RMSE[2,k]
    RMSE.vector.d[ite] <- large.list[[ite]]$RMSE[3,k]
  }
  result[7,k] <- median(RMSE.vector.y0)
  result[8,k] <- median(RMSE.vector.y1)
  result[9,k] <- median(RMSE.vector.d)
}

# Bias test for "Best" method
# R2 of regression of outcome on treatment status only
simple.reg <- lm(outcome ~ treatment, data=databig, na.action = na.omit)
theta.simple <- simple.reg$coefficients[2]
r2.simple <- summary(simple.reg)$r.squared
theta.best <- result[1,(M+1)]
r2.y <- rep(0,(M+1))
for (ite in 1:iterations) {
  for (k in 1:(M+1)) {
    r2.y[k] <- r2.y[k] + large.list[[ite]]$R.squared.y[k]/iterations
  }
}
r2.max <- c(2*r2.y[(M+1)], 3*r2.y[(M+1)], 4*r2.y[(M+1)], 5*r2.y[(M+1)], 1)
delta.0 <- theta.best/(theta.simple-theta.best) * 
  (r2.y[(M+1)]-r2.simple)/(r2.max-r2.y[(M+1)])

# Also compute R2 for treatment status
r2.d <- rep(0,(M+1))
for (ite in 1:iterations) {
  for (k in 1:(M+1)) {
    r2.d[k] <- r2.d[k] + large.list[[ite]]$R.squared.d[k]/iterations
  }
}

# Show distribution of trimmed observations
keepmat <- trimmat <- matrix(0,iterations,(M+1))
for (ite in 1:iterations) {
  for (k in 1:(M+1)) {
    keepmat[ite,k] <- large.list[[ite]]$keepvec[k]
    trimmat[ite,k] <- large.list[[ite]]$trimvec[k]
  }
}

rm(databig, large.list)
