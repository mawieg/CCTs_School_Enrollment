# This function contains various programs used for the calculation of DML treatment effects, 
# including the actual machine learning algorithms to predict nuisance parameters, score 
# functions, and further auxially functions. 
# Author: Martin Wiegand; based on code provided by V.Chernozhukov, D. Chetverikov, 
# M. Demirer, E. Duflo, C. Hansen, W. Newey, J. Robins for their paper "Double/debiased machine 
# learning for treatment and structural parameters", Econometrics Journal (2018).
# Last changed: 23.09.2020


# Regularized regression
RegRegF <- function(datause, dataout, form_x, form_y, logit=FALSE, lambda=lambda, alp=alp, post=FALSE){
  
  form            <- as.formula(paste(form_y, "~", form_x));
  
  Xuse             <- as.matrix(dplyr::select(datause, one_of(all.vars(form)[-1])))
  yuse             <- dplyr::select(datause, one_of(all.vars(form)[1]))[[1]]
  
  Xout            <- as.matrix(dplyr::select(dataout, one_of(all.vars(form)[-1])))
  yout            <- dplyr::select(dataout, one_of(all.vars(form)[1]))[[1]]
  
  if(logit==TRUE){
    regreg         <- glmnet(x=Xuse, y=yuse, family="binomial", alpha=alp)
  }
  
  if(logit==FALSE){
    regreg         <- glmnet(x=Xuse, y=yuse, alpha=alp)
  }
  
  if(post==FALSE){
    yhatuse         <- predict(regreg, newx=Xuse, s=lambda, type="response") 
    yhatout         <- predict(regreg, newx=Xout, s=lambda, type="response")
  } else {
    beta            <- coef(regreg, s=lambda)
    yhatuse <- matrix(nrow=dim(datause)[1],ncol=0) 
    yhatout <- matrix(nrow=dim(dataout)[1],ncol=0) 
    for (l in 1:length(lambda)) {
      beta.not.0      <- rownames(beta)[which(beta[,l]!=0)][-1]
      form_x.post     <- paste(beta.not.0, collapse='+')
      if(length(beta.not.0)==0) {form_x.post <- "1"}
      formpostlasso   <- as.formula(paste(form_y, "~", form_x.post))
      if(logit==FALSE) {
        postlasso     <- lm(formpostlasso, data=datause)
        yhatout         <- cbind(yhatout, predict(postlasso, newdata=dataout))
      } else {
        postlasso     <- glm(formpostlasso, family=binomial, data=datause)
        yhatout         <- cbind(yhatout, predict(postlasso, newdata=dataout, type="response"))
      }
      yhatuse         <- cbind(yhatuse, predict(postlasso, newdata=datause, type="response"))
    }
  }
   
  resuse          <- 1 
  resout          <- 1 

  model <- regreg
  if(post==TRUE){model <- postlasso}
  
  return(list(yhatuse = yhatuse, resuse=resuse, yhatout = yhatout, resout=resout, xuse=Xuse, xout=Xout, model=model, yout=yout, form=form));
  
}

# Regression trees
tree <- function(datause, dataout, form_x, form_y, method=method, arg=arg){

  form           <- as.formula(paste(form_y, "~", form_x));
  trees          <- do.call(rpart, append(list(formula=form, data=datause), arg))
  bestcp         <- trees$cptable[which.min(trees$cptable[,"xerror"]),"CP"]
  ptree          <- prune(trees,cp=bestcp)

  fit.p          <- lm(form,  x = TRUE, y = TRUE, data=datause); 
  yhatuse        <- predict(ptree, newdata=datause)
  resuse         <- fit.p$y - yhatuse
  xuse           <- fit.p$x
  
  fit.p           <- lm(form,  x = TRUE, y = TRUE, data=dataout); 
  yhatout         <- predict(ptree, newdata=dataout)
  resout          <- fit.p$y - yhatout
  xout            <- fit.p$x
  
  return(list(yhatuse = yhatuse, resuse=resuse, yhatout = yhatout, resout=resout, xuse=xuse, xout=xout, model=ptree));
  
}

# Neural Networks
nnetF <- function(datause, dataout, form_x, form_y, clas=FALSE, size, decay){
  
  linout=FALSE
  if(clas==TRUE){ linout=FALSE}
  
  form           <- as.formula(paste(form_y, "~", form_x))
  nn             <- nnet(formula=form, data=datause, linout=linout, size=size, decay=decay, maxit=1000, MaxNWts=50000, trace=FALSE)

  yhatuse        <- predict(nn, datause)
  yuse           <- dplyr::select(datause, one_of(all.vars(form)[1]))[[1]]
  resuse         <- yuse - yhatuse
  Xuse           <- dplyr::select(datause, one_of(all.vars(form)[-1]))  
  
  yhatout        <- predict(nn, dataout)
  yout           <- dplyr::select(dataout, one_of(all.vars(form)[1]))[[1]]
  resout         <- yout - yhatout
  Xout           <- dplyr::select(dataout, one_of(all.vars(form)[-1])) 

  return(list(yhatuse = yhatuse, resuse=resuse, yhatout = yhatout, resout=resout, xuse=Xuse, xout=Xout, model=nn));
}

# Boosting
boost <- function(datause, dataout, form_x, form_y, bag.fraction = .5, interaction.depth=2, n.trees=1000, shrinkage=.01, distribution='gaussian', option){
  
  form           <- as.formula(paste(form_y, "~", form_x));
  boostfit       <- do.call(gbm, append(list(formula=form, distribution=distribution, data=datause), option))
  if(option[['cv.folds']]>0)  {best           <- gbm.perf(boostfit,plot.it=FALSE,method="cv")}
  else                        {best           <- gbm.perf(boostfit,plot.it=FALSE,method="OOB")}

  yhatuse        <- predict(boostfit, n.trees=best)
  yuse           <- dplyr::select(datause, one_of(all.vars(form)[1]))[[1]]
  resuse         <- yuse - yhatuse
  Xuse            <- dplyr::select(datause, one_of(all.vars(form)[-1]))   

  yhatout        <- predict(boostfit, n.trees=best, newdata=dataout,  type="response")
  yout           <- dplyr::select(dataout, one_of(all.vars(form)[1]))[[1]]
  resout         <- yout - yhatout
  Xout            <- dplyr::select(dataout, one_of(all.vars(form)[-1])) 
  
  return(list(yhatuse = yhatuse, resuse=resuse, yhatout = yhatout, resout=resout, xuse=Xuse, xout=Xout, model=boostfit, best=best));
}

# Extreme gradient boosting
xg.boost <- function(datause, dataout, form_x, form_y, nround, eta, max.depth, subsample, colsample_bytree, min_child_weight, binary=0, double=FALSE, startmodel=0){
  
  form           <- as.formula(paste(form_y, "~", form_x));
  Xuse           <- as.matrix(dplyr::select(datause, one_of(all.vars(form)[-1])))
  yuse           <- as.matrix(dplyr::select(datause, one_of(all.vars(form)[1]))[[1]])

  if (binary==1) {
    boostfit       <- xgboost::xgboost(data = Xuse, label = yuse, max.depth = max.depth, eta = eta, nround = nround, subsample = subsample, colsample_bytree = colsample_bytree, min_child_weight = min_child_weight, objective = "binary:logistic", verbose = FALSE, save_period = NULL, nthread = 1)
  } else {
    boostfit       <- xgboost::xgboost(data = Xuse, label = yuse, max.depth = max.depth, eta = eta, nround = nround, subsample = subsample, colsample_bytree = colsample_bytree, min_child_weight = min_child_weight, objective = "reg:linear", verbose = FALSE, save_period = NULL, nthread = 1)
  }
  
  yhatuse        <- predict(boostfit, Xuse)
  if(binary==1){yhatuse <- ptrim(yhatuse)}
  resuse         <- yuse - yhatuse
  
  Xout           <- as.matrix(dplyr::select(dataout, one_of(all.vars(form)[-1])) )
  yout           <- as.matrix(dplyr::select(dataout, one_of(all.vars(form)[1]))[[1]])
  yhatout        <- predict(boostfit, Xout)
  if(binary==1){yhatout <- ptrim(yhatout)}
  resout         <- yout - yhatout
  
  
  return(list(yhatuse = yhatuse, resuse=resuse, yhatout = yhatout, resout=resout, xuse=Xuse, xout=Xout, model=boostfit));
}

# Random forest
RF <- function(datause, dataout,  form_x, form_y, x=NA, y=NA, xout=NA, yout=NA, nodesize, arg, reg=FALSE, tune=FALSE){
  
  yhatout <- NA
  reuse   <- NA
  yhatuse <- NA
  resout  <- NA
  

    form            <- as.formula(paste(form_y, "~", form_x));
    
    yuse            <- datause[,form_y]
    if (reg == FALSE) {
      datause[,form_y]      <- as.factor(datause[,form_y])
    }
    
    if(tune==FALSE){
        forest       <- do.call(randomForest, append(list(formula=form, nodesize=nodesize, data=datause), arg))
    }
    
    yhatuse         <- as.numeric(forest$predicted) 
    resuse          <- yuse - yhatuse
    
    yout            <- dplyr::select(dataout, one_of(all.vars(form)[1]))[[1]]   
    if(reg==TRUE)  {yhatout         <- predict(forest, dataout)}
    if(reg==FALSE) {yhatout         <- predict(forest, dataout, type="prob")[,2]}

    resout          <- as.numeric(yout) - as.numeric(yhatout)
  
  return(list(yhatuse = yhatuse, resuse=resuse, yhatout = yhatout, resout=resout, model = forest));
}

# Support vector machine/regression
svmF <- function(datause, dataout, form_x, form_y, cost, gamma, binary){
  
  form           <- as.formula(paste(form_y, "~", form_x));
  Xuse           <- as.matrix(dplyr::select(datause, one_of(all.vars(form)[-1])))
  yuse           <- as.matrix(dplyr::select(datause, one_of(form_y)))
  
  if(binary==1){
    svmfit         <- svm(x=Xuse, y=as.factor(yuse), kernel = "radial", cost = cost, gamma = gamma, probability = TRUE, fitted = TRUE)

    yhatuse        <- predict(svmfit, Xuse, probability = TRUE)
    yhatuse        <- attributes(yhatuse)$probabilities[,"1"]
    resuse         <- yuse - yhatuse
    
    Xout           <- as.matrix(dplyr::select(dataout, one_of(all.vars(form)[-1])) )
    yout           <- as.matrix(dplyr::select(dataout, one_of(y)))
    yhatout        <- predict(svmfit, Xout, probability = TRUE)
    yhatout        <- attributes(yhatout)$probabilities[,"1"]
    resout         <- yout - yhatout
  }
  if(binary==0){
    svmfit         <- svm(x=Xuse, y=yuse, kernel = "radial", cost = cost, fitted = TRUE)
    
    yhatuse        <- predict(svmfit, Xuse)
    resuse         <- yuse - yhatuse
    
    Xout           <- as.matrix(dplyr::select(dataout, one_of(all.vars(form)[-1])) )
    yout           <- as.matrix(dplyr::select(dataout, one_of(y)))
    yhatout        <- predict(svmfit, Xout)
    resout         <- yout - yhatout
  }
  
  return(list(yhatuse = yhatuse, resuse=resuse, yhatout = yhatout, resout=resout, xuse=Xuse, xout=Xout, model=svmfit));
}


# Score functions for treatment effects and their standard errors (analytical and bootstrap)

ATE <- function(y, d, my_d1x, my_d0x, md_x){
  return( mean( (d * (y - my_d1x) / md_x) -  ((1 - d) * (y - my_d0x) / (1 - md_x)) + my_d1x - my_d0x ) );
}

SE.ATE <- function(y, d, my_d1x, my_d0x, md_x, groupid){
  return( sd( (d * (y - my_d1x) / md_x) -  ((1 - d) * (y - my_d0x) / (1 - md_x)) + my_d1x - my_d0x )/sqrt(length(y)) );
}

BSSE.ATE <- function(y, d, my_d1x, my_d0x, md_x, groupid, B){
  depvar <- (d * (y - my_d1x) / md_x) -  ((1 - d) * (y - my_d0x) / (1 - md_x)) + my_d1x - my_d0x
  ones <- rep(1,length(depvar))
  helpmodel <- lm(depvar ~ ones)
  SE <- sqrt(cluster.boot(helpmodel, groupid, R = B, boot_type = "wild", wild_type = "norm"))[1,1]
  return(SE);
}


ATE_mis <- function(y, d, r, my_d1x, my_d0x, prd, prc){
  return( mean( (r * d * (y - my_d1x) / prd) -  (r * (1 - d) * (y - my_d0x) / prc) + my_d1x - my_d0x ) );
}

SE.ATE_mis <- function(y, d, r, my_d1x, my_d0x, prd, prc, groupid){
  return( sd( (r * d * (y - my_d1x) / prd) -  (r * (1 - d) * (y - my_d0x) / prc) + my_d1x - my_d0x )/sqrt(length(y)) );
}

BSSE.ATE_mis <- function(y, d, r, my_d1x, my_d0x, prd, prc, groupid, B){
  depvar <- (r * d * (y - my_d1x) / prd) -  (r * (1 - d) * (y - my_d0x) / prc) + my_d1x - my_d0x
  helpmodel <- lm(depvar ~ 1)
  SE <- sqrt(cluster.boot(helpmodel, groupid, R = B, boot_type = "wild", wild_type = "mammen"))[1,1]
  return(SE);
}


ATET <- function(y, d, my_d0x, md_x, p){
  return( mean( (d * (y - my_d0x) / p) - ((md_x / (1 - md_x)) * (1 - d) * (y - my_d0x) / p)  ) );
}

SE.ATET <- function(y, d, my_d0x, md_x, p, groupid){
  return( sd( (d * (y - my_d0x) / p) - ((md_x / (1 - md_x)) * (1 - d) * (y - my_d0x) / p)  )/sqrt(length(y)) );
}

BSSE.ATET <- function(y, d, my_d0x, md_x, p, groupid, B){
  depvar <- (d * (y - my_d0x) / p) - ((md_x / (1 - md_x)) * (1 - d) * (y - my_d0x) / p)
  ones <- rep(1,length(depvar))
  helpmodel <- lm(depvar ~ ones)
  SE <- sqrt(cluster.boot(helpmodel, groupid, R = B, boot_type = "wild", wild_type = "norm"))[1,1]
  return(SE);
}


ATET_mis <- function(y, d, r, my_d1x, my_d0x, md_x, prc, pr_t, p){
  return( mean( (r * d * (y - my_d1x) / (p * pr_t) ) - (r * (1 - d) * (y - my_d0x) * md_x) / (p * prc) + (d/p) * (my_d1x - my_d0x) )  );
}

SE.ATET_mis <- function(y, d, r, my_d1x, my_d0x, md_x, prc, pr_t, p, groupid){
  return( sd( (r * d * (y - my_d1x) / (p * pr_t) ) - (r * (1 - d) * (y - my_d0x) * md_x) / (p * prc) + (d/p) * (my_d1x - my_d0x)  )/sqrt(length(y)) );
}

BSSE.ATET_mis <- function(y, d, r, my_d1x, my_d0x, md_x, prc, pr_t, p, groupid, B){
  depvar <- (r * d * (y - my_d1x) / (p * pr_t) ) - (r * (1 - d) * (y - my_d0x) * md_x) / (p * prc) + (d/p) * (my_d1x - my_d0x)
  helpmodel <- lm(depvar ~ 1)
  SE <- sqrt(cluster.boot(helpmodel, groupid, R = B, boot_type = "wild", wild_type = "mammen"))[1,1]
  return(SE);
}


ATEN <- function(y, d, my_d1x, md_x, p){
  return( - mean( ((1 - d) * (y - my_d1x) / (1 - p)) - (((1 - md_x) / md_x) * d * (y - my_d1x) / (1 - p))  ) );
}

SE.ATEN <- function(y, d, my_d1x, md_x, p, groupid){
  return( sd( ((1 - d) * (y - my_d1x) / (1 - p)) - (((1 - md_x) / md_x) * d * (y - my_d1x) / (1 - p))  )/sqrt(length(y)) );
}

BSSE.ATEN <- function(y, d, my_d1x, md_x, p, groupid, B){
  depvar <- -(((1 - d) * (y - my_d1x) / (1 - p)) - (((1 - md_x) / md_x) * d * (y - my_d1x) / (1 - p)))
  ones <- rep(1,length(depvar))
  helpmodel <- lm(depvar ~ ones)
  SE <- sqrt(cluster.boot(helpmodel, groupid, R = B, boot_type = "wild", wild_type = "norm"))[1,1]
  return(SE);
}


ATEN_mis <- function(y, d, r, my_d1x, my_d0x, md_x, prt, pr_c, p){
  return( mean( (r * d * (y - my_d1x) * (1 - md_x) / ((1 - p) * prt) ) - (r * (1 - d) * (y - my_d0x)) / ((1 - p) * pr_c) + ((1 - d) / (1 - p)) * (my_d1x - my_d0x) )  );
}

SE.ATEN_mis <- function(y, d, r, my_d1x, my_d0x, md_x, prt, pr_c, p, groupid){
  return( sd( (r * d * (y - my_d1x) * (1 - md_x) / ((1 - p) * prt) ) - (r * (1 - d) * (y - my_d0x)) / ((1 - p) * pr_c) + ((1 - d) / (1 - p)) * (my_d1x - my_d0x)  )/sqrt(length(y)) );
}

BSSE.ATEN_mis <- function(y, d, r, my_d1x, my_d0x, md_x, prt, pr_c, p, groupid, B){
  depvar <- (r * d * (y - my_d1x) * (1 - md_x) / ((1 - p) * prt) ) - (r * (1 - d) * (y - my_d0x)) / ((1 - p) * pr_c) + ((1 - d) / (1 - p)) * (my_d1x - my_d0x)
  helpmodel <- lm(depvar ~ 1)
  SE <- sqrt(cluster.boot(helpmodel, groupid, R = B, boot_type = "wild", wild_type = "mammen"))[1,1]
  return(SE);
}




# Auxilary functions

checkBinary = function(v){
    x <- unique(v)
    length(x) - sum(is.na(x)) == 2L && all(sort(x[1:2]) == 0:1)
}

error <- function(yhat,y){
  if (length(yhat) == length(y)) {
    if (length(y)==0) {
      err         <- 0
    } else {
      err         <- sqrt(mean((yhat-y)^2))
    }
  } else {
    err <- 666
  }
  mis         <- sum(abs(as.numeric(yhat > .5)-(as.numeric(y))))/length(y)   
  return(list(err = err, mis=mis));
}

formC <- function(form_y,form_x, data){
  form            <- as.formula(paste(form_y, "~", form_x));    
  fit.p           <- lm(form,  x = TRUE, y = TRUE, data=data); 
  return(list(x = fit.p$x, y=fit.p$y));
}

clx <- function(fm, dfcw, cluster){
  # https://www.ne.su.se/polopoly_fs/1.216115.1426234213!/menu/standard/file/clustering1.pdf
  # library(sandwich)
  # library(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
  u <- apply(estfun(fm),2,
             function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
  coeftest(fm, vcovCL) 
}






# Trimming function (after Imbens & Rubin 2015)
optimal.trim_mis <- function(prop.score.1, prop.score.2)
{
  score <- 1 / (prop.score.1 * prop.score.2)
  if(max(score) <= 2*mean(score) & max(score) != Inf) {
    trim.opt <- c(0,1) 
  } else {
    score <- sort(score)
    gamma <- score[1]
    N <- length(score[!is.na(score)])
    i <- 1
    stop <- 0
    while(stop < 2 & i <= N) {
      if(((gamma / N) * sum(score <= gamma)) < ((2 / N) * sum(score * (score <= gamma), na.rm = TRUE))) {
        stop <- 0
      } else {
        stop <- stop + 1
      }
      i <- i + 1
      gamma <- score[i]
    }
    gamma <- score[i-1-stop]
    if (gamma<4) {
      alpha <- 0.1
    } else {
      alpha <- 0.5 - sqrt(0.25 - 1/gamma)
    }
    trim.opt <- c(alpha, 1 - alpha)
  }
  return(trim.opt)
}

overlap.trim <- function(prop.score, treatment) {
  min <- min(prop.score[treatment==1])
  max <- max(prop.score[treatment==0])
  trim.opt <- c(min, max)
  return(trim.opt)
}

ptrim <- function(prop.score) {
  for (i in 1:length(prop.score)){
    if (prop.score[i] <= 0){
      prop.score[i] <- 0.00001
    }
    if (prop.score[i] >= 1){
      prop.score[i] <- 0.99999
    }
  }
  return(prop.score)
}




# Fill missing values: for categorical variables, replace NAs by "Missing" category;
# for numerical variables, create new indicator variables for missing and replace NAs with average
# Use missing_data.R (by Bob Stine)

fill.missing <- function(dataFrame) {
  fill.missing.numerical(fill.missing.categorical(dataFrame))
}

# --- This function replaces NA in categorical variables by Missing label
#     Categorical variables must be represented as factors, not just text.
fill.missing.categorical <- function(data) {
  for(j in 1:ncol(data)) {
    if(is.factor(data[,j])) { # cat("Checking col ",j," for missing\n")
      i <- is.na(data[,j])
      if(any(i)) {	v <- as.vector(data[,j]); 
      v[i] <- "Missing"; data[,j]<-as.factor(v)
      }
    }}
  data
}

# --- This function fills NA in numerical cols by avg of those present
#     and tacks on extra columns for indicators of missing data. Returns
#     data frame with results.
fill.missing.numerical <- function(data) {
  missing.names <- rep("0",ncol(data))    # empty space to fill
  missing.cols  <- matrix(0,nrow=nrow(data),ncol=ncol(data))
  col <- 1;
  for(j in 1:ncol(data)) {
    if(!is.factor(data[,j])) { # cat("Checking col ",j," for missing\n")
      i <- is.na(data[,j])
      if(any(i)) {
        m <- mean(data[,j],na.rm=TRUE);
        data[i,j] <- m;
        missing.names[col] <- paste("Miss.",names(data)[j],sep="")
        missing.cols[,col]  <- 0+i;
        col <- col+1
      }}}
  missing.names <- missing.names[1:(col-1)]
  missing.cols  <- lapply(data.frame(missing.cols[,1:(col-1)]), as.factor); 
  names(missing.cols)<-missing.names
  data.frame(data,missing.cols)
}


# Construct dataset that includes interactions and cubic terms of all numerical variables.
# Based on code by Bob Stine.

build.predictors <- function(data, interactions) {
  n <- nrow(data)
  found.cat <- any(cat.cols <- sapply(data,is.factor))
  if(!found.cat) 		cat("Found no categorical columns in data.\n");
  num.cols <- !cat.cols
  X <- as.matrix(data[,num.cols])
  X2 <- t( (t(X)-colMeans(X))^2 )
  colnames(X2) <- paste(colnames(X),".SQR",sep='')
  X3 <- t( (t(X)-colMeans(X))^3 )
  colnames(X3) <- paste(colnames(X),".CUB",sep='')
  X  <- model.matrix(~., data=data)[,-1] # -1 removes the intercept
  newdata <- cbind(X,X2,X3)
  newdata.df <- as.data.frame(X)
  int <- as.data.frame(model.matrix(~., data=data[intersect(interactions,colnames(data))])[,-1])
  inter <- names(int)
  for (var in inter) {
    mat.temp <- as.matrix(newdata.df * int[c(var)][[1]])
    colnames(mat.temp) <- paste(colnames(mat.temp), var, sep = ".")
    newdata <- cbind(newdata, mat.temp)
  }
  var.not.0 <- !unlist(lapply(as.data.frame(newdata), function(x) 0 == var(x))) 
  newdata <- newdata[,var.not.0==1] # drop variables with zero variance
  newdata <- scale(newdata, center = TRUE, scale = TRUE) # normalize variables (mean 0 sd 1)
  newdata <- t(unique(t(newdata))) # drop duplicates
  return( as.data.frame(newdata) )
}

# Same, but also making cubic splines
build.predictors2 <- function(data, interactions) {
  n <- nrow(data)
  found.cat <- any(cat.cols <- sapply(data,is.factor))
  if(!found.cat) 		cat("Found no categorical columns in data.\n");
  num.cols <- !cat.cols
  X <- as.matrix(data[,num.cols])
  contvars <- ncol(X)
  # make cubic splines with 5 knots
  splines <- matrix(nrow=n, ncol=0)
  degree <- 3
  knots <- 5
  for (var in 1:contvars) {
    intermat <- bSpline(X[,var] + rnorm(contvars)*0.00000001, df = knots + degree + 1, degree = degree, intercept = TRUE)
    colnames(intermat) <- paste(colnames(X)[var],".BS.",colnames(intermat),sep='')
    splines <- cbind(splines, intermat)
  }
  intersplinenames <- paste(c('yycali', 'yycali', 'yycali', 'yycali', 'yycali', 'yycali', 'yycali', 'yycali', 'yycali', 
                              'mpcalif', 'mpcalif', 'mpcalif', 'mpcalif', 'mpcalif', 'mpcalif', 'mpcalif', 'mpcalif', 'mpcalif'),".BS.",1:(knots+degree+1),sep='')
  intersplines <- splines[,intersplinenames]
  X2 <- t( (t(X)-colMeans(X))^2 )
  colnames(X2) <- paste(colnames(X),".SQR",sep='')
  X3 <- t( (t(X)-colMeans(X))^3 )
  colnames(X3) <- paste(colnames(X),".CUB",sep='')
  X  <- model.matrix(~., data=data)[,-1] # -1 removes the intercept
  newdata <- cbind(X,X2,X3,intersplines)
  newdata.df <- as.data.frame(newdata)
  int <- as.data.frame(model.matrix(~., data=data[intersect(interactions,colnames(data))])[,-1])
  inter <- names(int)
  for (var in inter) {
    mat.temp <- as.matrix(newdata.df * int[c(var)][[1]])
    colnames(mat.temp) <- paste(colnames(mat.temp), var, sep = ".")
    newdata <- cbind(newdata, mat.temp)
  }
  newdata <- cbind(newdata, splines)
  var.not.0 <- !unlist(lapply(as.data.frame(newdata), function(x) 0 == var(x))) 
  newdata <- newdata[,var.not.0==1] # drop variables with zero variance
  newdata <- scale(newdata, center = TRUE, scale = TRUE) # normalize variables (mean 0 sd 1)
  newdata <- t(unique(t(newdata))) # drop perfectly collinear variables (or, since they are scaled, duplicates)
  return( as.data.frame(newdata) )
}
