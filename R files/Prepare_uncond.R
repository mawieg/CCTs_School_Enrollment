# This program prepares the unconditional sample datasets for the DML analysis. In particular,
# it creates the predictors to be used by the various machine learning methods. 
# Author: Martin Wiegand
# Last changed: 23.09.2020

options(warn=-1)
set.seed(1211);
cl   <- makeCluster(12, outfile="")
y      <- "outcome";
d      <- "treatment";    
r      <- "R03";
rt      <- "R03.t";
rc      <- "R03.c";
groupvar <- "locid";
if (outcomename == "highschool") {
  whichmis <- "missing00"
} 
if (outcomename == "highschool03" | outcomename == "highschool03fin") {
  whichmis <- "missing03"
}
if (outcomename == "middleschool00" | outcomename == "somemiddleschool00") {
  whichmis <- "missingmiddle00"
} 


if (status == "poor") {
  data <- read_csv("Unconditional.csv", na = c("", "NA", "99", "999", "9999")) %>%
    filter(pobre == 'Pobre') %>%
    rename(statusqualifier = verypoor) %>%
    mutate(outcome = get(outcomename), R03 = (1-get(whichmis)), R03.t = (1-get(whichmis))*treatment, R03.c = (1-get(whichmis))*(1-treatment)) %>%
    dplyr::select(-pobre, -poor, -highschool, -highschool03, -highschool03fin, -missing00, -missing03, -middleschool00, -somemiddleschool00, -missingmiddle00)
}

if (status == "nonpoor") {
  data <- read_csv("Unconditional.csv", na = c("", "NA", "99", "999", "9999")) %>%
    filter(pobre == 'No Pobre') %>%
    rename(statusqualifier = notrich) %>%
    mutate(outcome = get(outcomename), R03 = (1-get(whichmis)), R03.t = (1-get(whichmis))*treatment, R03.c = (1-get(whichmis))*(1-treatment)) %>%
    dplyr::select(-pobre, -poor, -highschool, -highschool03, -highschool03fin, -missing00, -missing03, -middleschool00, -somemiddleschool00, -missingmiddle00)
}


# Assign correct variable categories
nofactor <- c('treatment', 'outcome', 'R03', 'R03.t', 'R03.c', 'age97', 'hhsize97', 'numunder15', 'yycali', 'mpcalif', 'helpsibgirl', 'helpsibboy', 'helpworkgirl', 'helpworkboy', 'earnmongirl', 'earnmonboy', 'weekexp1', 'weekexp2', 'weekexp3', 'weekexp4', 'weekexp5', 'monthexp1', 'monthexp2', 'monthexp3', 'sixmonthexp1', 'sixmonthexp2', 'sixmonthexp3', 'sixmonthexp4', 'sixmonthexp5', 'sixmonthexp6', 'sixmonthexp7', 'sixmonthexp8', 'sixmonthexp9', 'sixmonthexp10', 'sixmonthexp11', 'sixmonthexp12', 'numberpreschools', 'numberprimschools', 'numbertelesecondaria', 'numbersecondary', 'avchildwage', 'ttprivsec', 'ttpubsec', 'tttelesec', 'ttclosestsec', 'ttCONALEP', 'ttCETA', 'ttCETIS', 'ttCEBTA', 'ttCEBTIS', 'ttprivhigh', 'ttpubhigh', 'ttclosesthigh', 'indice')
for (i in 1:ncol(data)) {
  if (names(data)[i] %in% nofactor) {
    data[[i]] <- as.numeric(data[[i]])
  }
  else {
    data[[i]] <- as.factor(data[[i]])
  }
}

X <- dplyr::select(data, -outcome, -treatment, -R03, -R03.t, -R03.c)


var.not.0 <- !unlist(lapply(X, function(x) length(unique(x)) <2 | (length(unique(x)) == 2 & sum(is.na(x)) > 0)))
X <- X[var.not.0]
n <- nrow(X)
p <- length(X)

options(na.action='na.pass')
X.mm <- as.data.frame(model.matrix(~., X))[-1]
X.mm <- as.data.frame(scale(X.mm, center = TRUE, scale = TRUE)) # normalize variables (mean 0 sd 1)
X.mm <- as.data.frame(t(unique(t(X.mm)))) # drop perfectly collinear variables (or, since they are scaled, duplicates)
data.mm <- data.frame(data$outcome, data$treatment, data$R03, data$R03.t, data$R03.c, X.mm) %>%
  rename(outcome=data.outcome, treatment=data.treatment, R03=data.R03, R03.t=data.R03.t, R03.c=data.R03.c)

Xnew <- as.tibble(fill.missing(as.data.frame(X)))
datanew <- data.frame(data$outcome, data$treatment, data$R03, data$R03.t, data$R03.c, as.data.frame(Xnew)) %>%
  rename(outcome=data.outcome, treatment=data.treatment, R03=data.R03, R03.t=data.R03.t, R03.c=data.R03.c)

interactionsvars <- c("yycali", "mpcalif", "numunder15", "fatherathome", "motherathome", "fatherlit", "motherlit", "fatherschool", "motherschool", "fatherminprim", "motherminprim", "fatherminsec", "motherminsec", "statusqualifier", "goodatschool", "cansec", "canfurther", "canhigh", "canuni", "childlabor", "avchildwage", "indice", "grado", "female", "age97", "Miss.avchildwage", "indloc", "attend97", "attend98")
Xbig <- build.predictors2(Xnew, interactionsvars)
names(X.mm) <- paste(names(X.mm), sep = ".", "orig")
xo <- names(X.mm)  # original variables, factors turned into indicators, missing values kept
xL <- names(Xbig)

Xverynew <- as.data.frame(model.matrix(~., Xnew))[-1]
Xverynew <- as.data.frame(scale(Xverynew, center = TRUE, scale = TRUE)) # normalize variables (mean 0 sd 1)
Xverynew <- as.data.frame(t(unique(t(Xverynew)))) # drop perfectly collinear variables (or, since they are scaled, duplicates)
dataverynew <- data.frame(data$outcome, data$treatment, data$R03, data$R03.t, data$R03.c, Xverynew) %>%
  rename(outcome=data.outcome, treatment=data.treatment, R03=data.R03, R03.t=data.R03.t, R03.c=data.R03.c)
names(Xverynew) <- paste(names(Xverynew), sep = ".", "imp")
xx <- names(Xverynew) # original variables, factors turned into indicators, missing values imputed (used for SVM)
databig <- data.frame(data$outcome, data$treatment, data$R03, data$R03.t, data$R03.c, Xverynew, X.mm, Xbig) %>%
  rename(outcome=data.outcome, treatment=data.treatment, R03=data.R03, R03.t=data.R03.t, R03.c=data.R03.c)
databig$locid <- as.factor(databig$indice)  # location id (constructed by indice, which is unique for every location)
levels(databig$locid) <- 1:length(unique(databig$locid))  # turn into factor variable with integers
databig$locid <- as.integer(databig$locid)
names(xx) <- xx
nogroupvar <- c()
for (name in names(xx)){
  if (anova(lm(databig[,name] ~ factor(databig$locid)))$`Pr(>F)`[1] < 0.00001 & length(unique(databig[,name])) > 3) {
    nogroupvar <- c(nogroupvar, 0)
  } else {
    nogroupvar <- c(nogroupvar, 1)
  }
}
xxshort <- xx[nogroupvar==1]
names(xo) <- xo
nogroupvar <- c()
for (name in names(xo)){
  if (anova(lm(databig[,name] ~ factor(databig$locid), na.action=na.omit))$`Pr(>F)`[1] < 0.00001 & length(unique(databig[,name])) > 2) {
    nogroupvar <- c(nogroupvar, 0)
  } else {
    nogroupvar <- c(nogroupvar, 1)
  }
}
xoshort <- xo[nogroupvar==1]

rm(data, data.mm, datanew, dataverynew, X, X.mm, Xbig, Xnew, Xverynew)
