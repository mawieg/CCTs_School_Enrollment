# This program produces a dataset containing the results of the DML method for 
# specified outcome, set of observations, and poverty status. Result tables 
# containing ATE, ATEN, and RMSE of the various nuisance functions are stored in 
# matrix "result". Relative importance is stored in vector "delta.0". 
# Author: Martin Wiegand 
# Partially based on code provided by V.Chernozhukov, D. Chetverikov, 
# M. Demirer, E. Duflo, C. Hansen, W. Newey, J. Robins for their paper 
# "Double/debiased machine learning for treatment and structural parameters", 
# Econometrics Journal (2018).
# Last changed: 25.04.2022
#
#
# Before running this, change "Directory" to the Progresa directory and specify 
# the following variables:
#
# status: 
#   - "poor": the subset of students eligible for Progresa payments
#   - "nonpoor": the subset of students not eligible for Progresa payments
# outcomename: 
#   - "highschool": student going to high school in November 2000
#   - "highschool03": student has ever been to high school in 2003
#   - "highschool03fin": student has graduated or was in last grade of high
#                        school in 2003
#   - "middleschool00": student graduated from middle school in 1999/2000
#   - "somemiddleschool00": student graduated from middle school or was in last
#                           grade of middle school in 1999/2000
#   - "pcexp": monthly per capita expenditure of student's household
#   - "lnpcexp": log monthly per capita expenditure of student's household
#   - "moneyreason": student not going to high school has financial reasons
#   - "otherreason": student not going to high school has non-financial reasons
# set: 
#   - "uncond": unconditional sample (students who were 11-14 and graduated from
#               primary school in 1997)
#   - "big": unrestricted conditional sample (adolescents who were 14-17 and who
#            finished the last year of middle school in 2000)
#   - "small": restricted contitional sample (adolescents who were 14-17 and who
#              finished the last year of middle school in 2000, and who were 
#              confirmed to be enrolled in the second last grade of middle 
#              school during the academic year 1999/2000.


rm(list = ls())
setwd("Directory/Progresa") # change this accordingly
set <- "uncond"
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

# number of folds for cross-validation and cross-fitting
nfolds <- 10
# repetitions for cross-validation procedure (for hyperparameter tuning)
reps <- 10
# number of iterations for cross-fitting (DML) procedure
iterations <- 100

# Load needed programs
source("R files/ML_Functions.R")  
source("R files/cond_comp.R")
source("R files/cond_comp_mis.R")
source("R files/DML.R")
source("R files/DML_mis.R")
source("R files/Best_Parameters.R")
source("R files/Best_Parameters_mis.R")
source("R files/Best_Parameters_Reg.R")

# Prepare dataset
if (set != "uncond") {
  source("R files/Prepare.R")
} else {
  source("R files/Prepare_uncond.R")
}

# assign number of CPU cores to be used for parallel processing
cl <- makePSOCKcluster(3, outfile='') 
registerDoParallel(cl)
# Make sure to set library path on each worker
# clusterEvalQ(cl, .libPaths("libpath")) 

# Hyperparameter selection
if (set != "uncond" & (outcomename == "highschool" | 
                       outcomename == "moneyreason" | 
                       outcomename == "otherreason")) {
  param.Lasso    <- Best_Parameters("Lasso", databig, y, d, groupvar, xL, 
                                    folds=nfolds, reps=reps, seed = 1)
  param.Ridge    <- Best_Parameters("Ridge", databig, y, d, groupvar, xL, 
                                    folds=nfolds, reps=reps, seed = 1)
  param.Elnet    <- Best_Parameters("Elnet", databig, y, d, groupvar, xL, 
                                    folds=nfolds, reps=reps, seed = 1)
  param.Boosting <- Best_Parameters("Boosting", databig, y, d, groupvar, xo, 
                                    folds=nfolds, reps=reps, seed = 1)
  param.SVM      <- Best_Parameters("SVM", databig, y, d, groupvar, xx, 
                                    folds=nfolds, reps=reps, seed = 1)
}
if (set == "uncond" | 
    outcomename == "highschool03" | 
    outcomename == "highschool03fin") {
  param.Lasso    <- Best_Parameters_mis("Lasso", databig, y, d, r, rt, rc, 
                                        groupvar, xL, folds=nfolds, reps=reps, 
                                        seed = 1)
  param.Ridge    <- Best_Parameters_mis("Ridge", databig, y, d, r, rt, rc, 
                                        groupvar, xL, folds=nfolds, reps=reps, 
                                        seed = 1)
  param.Elnet    <- Best_Parameters_mis("Elnet", databig, y, d, r, rt, rc, 
                                        groupvar, xL, folds=nfolds, reps=reps, 
                                        seed = 1)
  param.Boosting <- Best_Parameters_mis("Boosting", databig, y, d, r, rt, rc, 
                                        groupvar, xo, folds=nfolds, reps=reps, 
                                        seed = 1)
  param.SVM      <- Best_Parameters_mis("SVM", databig, y, d, r, rt, rc, 
                                        groupvar, xx, folds=nfolds, reps=reps, 
                                        seed = 1)
}
if (outcomename == "pcexp" | outcomename == "lnpcexp") {
  param.Lasso    <- Best_Parameters_Reg("Lasso", databig, y, d, groupvar, xL, 
                                        folds=nfolds, reps=reps, seed = 1)
  param.Ridge    <- Best_Parameters_Reg("Ridge", databig, y, d, groupvar, xL, 
                                        folds=nfolds, reps=reps, seed = 1)
  param.Elnet    <- Best_Parameters_Reg("Elnet", databig, y, d, groupvar, xL, 
                                        folds=nfolds, reps=reps, seed = 1)
  param.Boosting <- Best_Parameters_Reg("Boosting", databig, y, d, groupvar, xo, 
                                        folds=nfolds, reps=reps, seed = 1)
  param.SVM      <- Best_Parameters_Reg("SVM", databig, y, d, groupvar, xx, 
                                        folds=nfolds, reps=reps, seed = 1)
}
param.Lasso$alpha.yd0 <- 1
param.Lasso$alpha.yd1 <- 1
param.Lasso$alpha.d   <- 1
param.Lasso$alpha.rt  <- 1
param.Lasso$alpha.rc  <- 1
param.Lasso$alpha.r_t <- 1
param.Lasso$alpha.r_c <- 1
param.Ridge$alpha.yd0 <- 0
param.Ridge$alpha.yd1 <- 0
param.Ridge$alpha.d   <- 0
param.Ridge$alpha.rt  <- 0
param.Ridge$alpha.rc  <- 0
param.Ridge$alpha.r_t <- 0
param.Ridge$alpha.r_c <- 0
param.Forest <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, 
                     na.action=na.omit, replace=TRUE)
arguments <- list(Lasso=param.Lasso, Ridge=param.Ridge, Elnet=param.Elnet, 
                  Boosting=param.Boosting, Forest=param.Forest, SVM=param.SVM)

# DML estimation
if (set == "uncond" | 
    outcomename == "highschool03" | 
    outcomename == "highschool03fin") {
  source("R files/Estimate_mis.R")
} else {
  source("R files/Estimate.R")
}

# Remove '#' to save the workspace
#savename <- paste(set, outcomename, status, "results", sep = '_')
#savename <- paste(savename, "RData", sep = ".")
#save.image(savename)

View(result)