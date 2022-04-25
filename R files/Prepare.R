# This program prepares the conditional sample datasets for the DML analysis. In 
# particular, it creates the predictors to be used by the various machine 
# learning methods. 
# Author: Martin Wiegand
# Last changed: 25.04.2022

options(warn=-1)
set.seed(1211);
cl <- makeCluster(12, outfile="")
y        <- "outcome";
d        <- "treatment";    
r        <- "R03";
rt       <- "R03.t";
rc       <- "R03.c";
groupvar <- "locid";

if (set=="big"){
  if (status == "poor") {
    data <- read_csv("Intermediate data/Highschool_big.csv", 
                     na = c("", "NA", "99", "999", "9999")) %>%
      filter(pobre == 'Pobre') %>%
      rename(statusqualifier = verypoor) %>%
      mutate(outcome = get(outcomename), R03 = (1-missing03), 
             R03.t = (1-missing03)*treatment, 
             R03.c = (1-missing03)*(1-treatment)) %>%
      dplyr::select(-pobre, -poor, -highschool, -highschool03, -highschool03fin, 
                    -moneyreason, -otherreason, -missing03, -pcexp, -lnpcexp)
  }
  if (status == "nonpoor") {
    data <- read_csv("Intermediate data/Highschool_big.csv", 
                     na = c("", "NA", "99", "999", "9999")) %>%
      filter(pobre == 'No Pobre') %>%
      rename(statusqualifier = notrich) %>%
      mutate(outcome = get(outcomename), R03 = (1-missing03), 
             R03.t = (1-missing03)*treatment, 
             R03.c = (1-missing03)*(1-treatment)) %>%
      dplyr::select(-pobre, -poor, -highschool, -highschool03, -highschool03fin, 
                    -moneyreason, -otherreason, -missing03, -pcexp, -lnpcexp)
  }
}

if (set=="small"){
  if (status == "poor") {
    data <- read_csv("Intermediate data/Highschool_small.csv", 
                     na = c("", "NA", "99", "999", "9999")) %>%
      filter(pobre == 'Pobre') %>%
      rename(statusqualifier = verypoor) %>%
      mutate(outcome = get(outcomename), R03 = (1-missing03), 
             R03.t = (1-missing03)*treatment, 
             R03.c = (1-missing03)*(1-treatment)) %>%
      dplyr::select(-pobre, -poor, -highschool, -highschool03, -highschool03fin, 
                    -moneyreason, -otherreason, -missing03, -pcexp, -lnpcexp)
  }
  if (status == "nonpoor") {
    data <- read_csv("Intermediate data/Highschool_small.csv", 
                     na = c("", "NA", "99", "999", "9999")) %>%
      filter(pobre == 'No Pobre') %>%
      rename(statusqualifier = notrich) %>%
      mutate(outcome = get(outcomename), R03 = (1-missing03), 
             R03.t = (1-missing03)*treatment, 
             R03.c = (1-missing03)*(1-treatment)) %>%
      dplyr::select(-pobre, -poor, -highschool, -highschool03, -highschool03fin, 
                    -moneyreason, -otherreason, -missing03, -pcexp, -lnpcexp)
  }
}

# Assign correct variable categories
nofactor <- c('treatment', 'outcome', 'R03', 'R03.t', 'R03.c', 'age97', 
              'hhsize97', 'numunder15', 'yycali', 'mpcalif', 'helpsibgirl', 
              'helpsibboy', 'helpworkgirl', 'helpworkboy', 'earnmongirl', 
              'earnmonboy', 'weekexp1', 'weekexp2', 'weekexp3', 'weekexp4', 
              'weekexp5', 'monthexp1', 'monthexp2', 'monthexp3', 'sixmonthexp1', 
              'sixmonthexp2', 'sixmonthexp3', 'sixmonthexp4', 'sixmonthexp5', 
              'sixmonthexp6', 'sixmonthexp7', 'sixmonthexp8', 'sixmonthexp9', 
              'sixmonthexp10', 'sixmonthexp11', 'sixmonthexp12', 
              'numberpreschools', 'numberprimschools', 'numbertelesecondaria', 
              'numbersecondary', 'avchildwage', 'ttprivsec', 'ttpubsec', 
              'tttelesec', 'ttclosestsec', 'ttCONALEP', 'ttCETA', 'ttCETIS', 
              'ttCEBTA', 'ttCEBTIS', 'ttprivhigh', 'ttpubhigh', 'ttclosesthigh', 
              'indice', 'vilcount', 'vilcountpoor', 'rightagetotal', 
              'rightageshare', 'rightagepoortotal', 'rightagepoorshare', 
              'rightageattend97total', 'rightageattend97share', 
              'rightageattend97poortotal', 'rightageattend97poorshare', 
              'minprimtotal', 'minprimtotal15to20', 'minprimtotal21to30', 
              'minprimshare', 'minprimshare15to20', 'minprimshare21to30', 
              'minprimtotalpoor', 'minprimtotal15to20poor', 
              'minprimtotal21to30poor', 'minprimsharepoor', 
              'minprimshare15to20poor', 'minprimshare21to30poor', 'minsectotal', 
              'minsectotal15to20', 'minsectotal21to30', 'minsecshare', 
              'minsecshare15to20', 'minsecshare21to30', 'minsectotalpoor', 
              'minsectotal15to20poor', 'minsectotal21to30poor', 
              'minsecsharepoor', 'minsecshare15to20poor', 
              'minsecshare21to30poor', 'minhightotal', 'minhightotal15to20', 
              'minhightotal21to30', 'minhighshare', 'minhighshare15to20', 
              'minhighshare21to30', 'minhightotalpoor', 
              'minhightotal15to20poor', 'minhightotal21to30poor', 
              'minhighsharepoor', 'minhighshare15to20poor', 
              'minhighshare21to30poor')
for (i in 1:ncol(data)) {
  if (names(data)[i] %in% nofactor) {
    data[[i]] <- as.numeric(data[[i]])
  }
  else {
    data[[i]] <- as.factor(data[[i]])
  }
}

X <- dplyr::select(data, -outcome, -treatment, -R03, -R03.t, -R03.c)
var.not.0 <- !unlist(lapply(X, function(x) length(unique(x)) <2 | 
                              (length(unique(x)) == 2 & sum(is.na(x)) > 0)))
X <- X[var.not.0]
n <- nrow(X)
p <- length(X)

options(na.action='na.pass')
X.mm <- as.data.frame(model.matrix(~., X))[-1]
# normalize variables (mean 0 sd 1)
X.mm <- as.data.frame(scale(X.mm, center = TRUE, scale = TRUE)) 
# drop perfectly collinear variables (or, since they are scaled, duplicates)
X.mm <- as.data.frame(t(unique(t(X.mm)))) 
data.mm <- data.frame(data$outcome, data$treatment, data$R03, data$R03.t, 
                      data$R03.c, X.mm) %>%
  rename(outcome=data.outcome, treatment=data.treatment, R03=data.R03, 
         R03.t=data.R03.t, R03.c=data.R03.c)

Xnew <- as.tibble(fill.missing(as.data.frame(X)))
datanew <- data.frame(data$outcome, data$treatment, data$R03, data$R03.t, 
                      data$R03.c, as.data.frame(Xnew)) %>%
  rename(outcome=data.outcome, treatment=data.treatment, R03=data.R03, 
         R03.t=data.R03.t, R03.c=data.R03.c)

interactionsvars <- c("yycali", "mpcalif", "numunder15", "fatherathome", 
                      "motherathome", "fatherlit", "motherlit", "fatherschool", 
                      "motherschool", "fatherminprim", "motherminprim", 
                      "fatherminsec", "motherminsec", "statusqualifier", 
                      "goodatschool", "cansec", "canfurther", "canhigh", 
                      "canuni", "childlabor", "avchildwage", "indice", "grado", 
                      "female", "age97", "Miss.avchildwage", "indloc", 
                      "attend97", "attend98", "vilcount", "rightagetotal", 
                      "rightagepoortotal", "rightageattend97total", 
                      "rightageattend97poortotal")
Xbig <- build.predictors2(Xnew, interactionsvars)
names(X.mm) <- paste(names(X.mm), sep = ".", "orig")

# xo: original variables, factors turned into indicators, missing values kept
# (used for xgboost)
xo <- names(X.mm)

# xL: variables with interaction terms and cubic splines of all numerical 
# variables (used for regularized regressions methods)
xL <- names(Xbig)

Xverynew <- as.data.frame(model.matrix(~., Xnew))[-1]
# normalize variables (mean 0 sd 1)
Xverynew <- as.data.frame(scale(Xverynew, center = TRUE, scale = TRUE))
# drop perfectly collinear variables (or, since they are scaled, duplicates)
Xverynew <- as.data.frame(t(unique(t(Xverynew))))
dataverynew <- data.frame(data$outcome, data$treatment, data$R03, data$R03.t, 
                          data$R03.c, Xverynew) %>%
  rename(outcome=data.outcome, treatment=data.treatment, R03=data.R03, 
         R03.t=data.R03.t, R03.c=data.R03.c)
names(Xverynew) <- paste(names(Xverynew), sep = ".", "imp")

# xx: original variables, factors turned into indicators, missing values imputed 
# (used for Random Forest, SVM, Nnet)
xx <- names(Xverynew) 

databig <- data.frame(data$outcome, data$treatment, data$R03, data$R03.t, 
                      data$R03.c, Xverynew, X.mm, Xbig) %>%
  rename(outcome=data.outcome, treatment=data.treatment, R03=data.R03, 
         R03.t=data.R03.t, R03.c=data.R03.c)
# location id (constructed by indice, which is unique for every location)
databig$locid <- as.factor(databig$indice)
# turn into factor variable with integers
levels(databig$locid) <- 1:length(unique(databig$locid))
databig$locid <- as.integer(databig$locid)
names(xx) <- xx
nogroupvar <- c()
for (name in names(xx)){
  if (anova(lm(databig[,name] ~ factor(databig$locid)))$`F value`[1] > 20) {
    nogroupvar <- c(nogroupvar, 0)
  } else {
    nogroupvar <- c(nogroupvar, 1)
  }
}
xxhh <- xx[nogroupvar==1]
xxvil <- setdiff(xx, xxhh)

names(xo) <- xo
nogroupvar <- c()
for (name in names(xo)){
  if (anova(lm(databig[,name] ~ factor(databig$locid), 
               na.action=na.omit))$`F value`[1] > 20) {
    nogroupvar <- c(nogroupvar, 0)
  } else {
    nogroupvar <- c(nogroupvar, 1)
  }
}
xohh <- xo[nogroupvar==1]
xovil <- setdiff(xo, xohh)

names(xL) <- xL
nogroupvar <- c()
for (name in names(xL)){
  if (anova(lm(databig[,name] ~ factor(databig$locid), 
               na.action=na.omit))$`F value`[1] > 20) {
    nogroupvar <- c(nogroupvar, 0)
  } else {
    nogroupvar <- c(nogroupvar, 1)
  }
}
xLhh <- xL[nogroupvar==1]
xLvil <- setdiff(xL, xLhh)


# Assign variable firstobs, assigning 1 to the first observation in each cluster 
# and 0 otherwise, and variable weight, indicating the number of observations 
# per cluster (only accurate for firstobs==1). These are being used to create a 
# small version of the dataset, with only one observation per cluster.  
databig$firstobs <- 1
databig$weight <- 1
counter <- 0
for(i in 2:length(databig$locid)) {
  if(databig$locid[i]==databig$locid[i-1]){
    databig$firstobs[i] <- 0
    counter <- counter + 1
  } else {
    databig$weight[i-counter-1] <- databig$weight[i-counter] + counter
    counter <- 0
  }
}
databig$locid <- as.factor(databig$locid)

rm(data, data.mm, datanew, dataverynew, X, X.mm, Xbig, Xnew, Xverynew)
