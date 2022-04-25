# This program produces a dataset containing the results of the DML method for specified
# outcome, set of observations, and poverty status. Result tables containg ATE, ATEN, and 
# RMSE of the various nuisance functions are stored in matrix "result". Relative importance 
# is stored in vector "delta.0". 
# Author: Martin Wiegand; based on code provided by V.Chernozhukov, D. Chetverikov, 
# M. Demirer, E. Duflo, C. Hansen, W. Newey, J. Robins for their paper "Double/debiased machine 
# learning for treatment and structural parameters", Econometrics Journal (2018)
# Last changed: 24.10.2020

# Before running this specify the following variables below:
# status: "poor", "nonpoor"
# outcomename: "highschool", "highschool03", "highschool03fin", "middleschool00", 
#              "somemiddleschool00", "pcexp", "lnpcexp", "moneyreason", "otherreason"
# set: "big", "small", "uncond"

rm(list = ls())
setwd("Directory\Progresa") # change this to Progresa directory
set <- "big"
status <- "poor"
outcomename <- "highschool"

# Activate libraries
library(tidyverse)
library(dplyr)
library(randomForest)
library(kernlab)
library(party)
library(partykit)
library(survey)
library(glmnet)
library(e1071)
library(caret)
library(splines2)
library(foreign)
library(quantreg)
library(mnormt)
library(gbm)
library(glmnet)
library(CausalGAM)
library(MASS)
library(rpart)
library(doParallel)
library(sandwich)
library(hdm)
library(nnet)
library(matrixStats)
library(quadprog)
library(lsei)
library(xgboost)
library(groupdata2)
library(multiwayvcov)
library(lmtest)
library(Matching)
library(doSNOW)

nfolds <- 10        # number of folds for cross-validation and cross-fitting
reps <- 10          # repetitions for cross-validation procedure (for hyperparameter tuning)
iterations <- 100   # number of iterations for cross-fitting (DML) procedure

# Load needed programs
source("ML_Functions.R")  
source("cond_comp.R")
source("cond_comp_mis.R")
source("DML.R")
source("DML_mis.R")
source("Best_Parameters.R")
source("Best_Parameters_mis.R")
source("Best_Parameters_Reg.R")

# Prepare dataset
if (set != "uncond") {
  source("Prepare.R")
} else {
  source("Prepare_uncond.R")
}
databig$locid <- as.factor(databig$locid)

cl <- makePSOCKcluster(3, outfile='') # assign number of CPU cores to be used for parallel processing
registerDoParallel(cl) 
# clusterEvalQ(cl, .libPaths("libpath")) # Make sure to set library path on each worker

# Hyperparameter selection
if (set != "uncond" & (outcomename == "highschool" | outcomename == "moneyreason" | outcomename == "otherreason")) {
  param.Lasso    <- Best_Parameters("Lasso", databig, y, d, groupvar, xL, folds=nfolds, reps=reps, seed = 1)
  param.Ridge    <- Best_Parameters("Ridge", databig, y, d, groupvar, xL, folds=nfolds, reps=reps, seed = 1)
  param.Elnet    <- Best_Parameters("Elnet", databig, y, d, groupvar, xL, folds=nfolds, reps=reps, seed = 1)
  param.Boosting <- Best_Parameters("Boosting", databig, y, d, groupvar, xo, folds=nfolds, reps=reps, seed = 1)
  param.SVM      <- Best_Parameters("SVM", databig, y, d, groupvar, xx, folds=nfolds, reps=reps, seed = 1)
}
if (set == "uncond" | outcomename == "highschool03" | outcomename == "highschool03fin") {
  param.Lasso    <- Best_Parameters_mis("Lasso", databig, y, d, r, rt, rc, groupvar, xL, folds=nfolds, reps=reps, seed = 1)
  param.Ridge    <- Best_Parameters_mis("Ridge", databig, y, d, r, rt, rc, groupvar, xL, folds=nfolds, reps=reps, seed = 1)
  param.Elnet    <- Best_Parameters_mis("Elnet", databig, y, d, r, rt, rc, groupvar, xL, folds=nfolds, reps=reps, seed = 1)
  param.Boosting <- Best_Parameters_mis("Boosting", databig, y, d, r, rt, rc, groupvar, xo, folds=nfolds, reps=reps, seed = 1)
  param.SVM      <- Best_Parameters_mis("SVM", databig, y, d, r, rt, rc, groupvar, xx, folds=nfolds, reps=reps, seed = 1)
}
if (outcomename == "pcexp" | outcomename == "lnpcexp") {
  param.Lasso    <- Best_Parameters_Reg("Lasso", databig, y, d, groupvar, xL, folds=nfolds, reps=reps, seed = 1)
  param.Ridge    <- Best_Parameters_Reg("Ridge", databig, y, d, groupvar, xL, folds=nfolds, reps=reps, seed = 1)
  param.Elnet    <- Best_Parameters_Reg("Elnet", databig, y, d, groupvar, xL, folds=nfolds, reps=reps, seed = 1)
  param.Boosting <- Best_Parameters_Reg("Boosting", databig, y, d, groupvar, xo, folds=nfolds, reps=reps, seed = 1)
  param.SVM      <- Best_Parameters_Reg("SVM", databig, y, d, groupvar, xx, folds=nfolds, reps=reps, seed = 1)
}
param.Lasso$alpha.yd0 <- 1
param.Lasso$alpha.yd1 <- 1
param.Lasso$alpha.d <- 1
param.Lasso$alpha.rt <- 1
param.Lasso$alpha.rc <- 1
param.Lasso$alpha.r_t <- 1
param.Lasso$alpha.r_c <- 1
param.Ridge$alpha.yd0 <- 0
param.Ridge$alpha.yd1 <- 0
param.Ridge$alpha.d <- 0
param.Ridge$alpha.rt <- 0
param.Ridge$alpha.rc <- 0
param.Ridge$alpha.r_t <- 0
param.Ridge$alpha.r_c <- 0
param.Forest      <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
arguments <- list(Lasso=param.Lasso, Ridge=param.Ridge, Elnet=param.Elnet, 
                  Boosting=param.Boosting, Forest=param.Forest, SVM=param.SVM)

# Remove '#' to save here
#savename <- paste(set, outcomename, status, "data", sep = '_')
#savename <- paste(savename, "RData", sep = ".")
#save.image(savename)

# DML estimation
if (set == "uncond" | outcomename == "highschool03" | outcomename == "highschool03fin") {
  source("Estimate_mis.R")
} else {
  source("Estimate.R")
}

savename <- paste(set, outcomename, status, "results", sep = '_')
savename <- paste(savename, "RData", sep = ".")
save.image(savename)

