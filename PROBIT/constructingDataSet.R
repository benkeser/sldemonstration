## Constructing analysis dataset
library(foreign)
library(SuperLearner)
library(randomForest)

# do we need to run imputation models
runImpute <- FALSE

dataDir <- "H:/GHAP/PROBIT/PROBIT/sdtm"
saveDir <- "H:/GHAP/PROBIT/"
# empty data set
fullDat <- data.frame(NULL)

# antrho data
anthdat <- read.xport(file.path(dataDir, "ANTHRO.xpt"))
anthdat <- anthdat[anthdat$AGEDAYS <=7, 
                   c("SUBJID","WTKG","BMI","HCIRCM",'HAZ',"WAZ","WHZ","BAZ")]

# pregnancy data
pregdat <- read.xport(file.path(dataDir, "PREGHX.xpt"))
pregdat$LIGHTSMOKE <- as.numeric(pregdat$SMOKAMT == "1 to 4")
pregdat$HEAVYSMOKE <- as.numeric(!pregdat$SMOKAMT %in% c("None", "1 to 4"))
COMPRISK <- table(pregdat$COMPRISK)
for(k in which(COMPRISK >= 10)){
  eval(parse(text=paste0("pregdat$COMPRISK",k," <- as.numeric(pregdat$COMPRISK == '", names(COMPRISK[k]), "')")))
}
pregdat$ALCOHOL <- as.numeric(!grepl("<1/mo",pregdat$ETOHAMT))

pregdat <- pregdat[, c("SUBJID","LIGHTSMOKE","HEAVYSMOKE",
                       "NLCHILD","ALCOHOL",
                       paste0("COMPRISK",which(COMPRISK >= 10)))]

# subject data
subjdat <- read.xport(file.path(dataDir, "SUBJ.xpt"))

subjdat$SEX <- subjdat$SEXN - 1
subjdat$STRATUM1 <- as.numeric(subjdat$STRATUMN==1)
subjdat$STRATUM2 <- as.numeric(subjdat$STRATUMN==2)
subjdat$STRATUM3 <- as.numeric(subjdat$STRATUMN==3)

subjdat$VAGBIRTH <- as.numeric(subjdat$DELIVERY == "Spontaneous vaginal")
subjdat$CSEC <- as.numeric(subjdat$DELIVERY == "Caesarean")

subjdat <- subjdat[,c("SUBJID","SITEID","SEX","ARMCD",paste0("STRATUM",1:3), 
                      "GAGEBRTH","BIRTHWT","BIRTHLEN","BIRTHHC",
                      "VAGBIRTH","CSEC","APGAR1","APGAR5")]

# merge together what we got
merge1 <- merge(x=pregdat,y=subjdat)

# parents data w/ imputation
pardat <- read.xport(file.path(dataDir,"PARENTS.xpt"))
pardat$MJOB1 <- as.numeric(pardat$MWORK == "Farmer")
pardat$MJOB2 <- as.numeric(pardat$MWORK == "Housewife")
pardat$MJOB3 <- as.numeric(pardat$MWORK == "Manual worker")
pardat$MJOB4 <- as.numeric(pardat$MWORK == "Pupil")
pardat$MJOB5 <- as.numeric(pardat$MWORK == "Service worker")
pardat$MJOB6 <- as.numeric(pardat$MWORK == "Student")


fullpardat <- pardat[,c("SUBJID","MAGE","MMARITN","MEDUCYRS",paste0("MJOB",1:6))]
merge2 <- merge(x=merge1,y=fullpardat)
impdat <- merge2[complete.cases(merge2),]


# imputation of variables

# screen function for variables most relevant
screen.corP01 <- function(...,minPvalue=0.01){
  screen.corP(...,minPvalue = minPvalue)
}
if(runImpute){
  for(yVar in c("MHTCM","MWTKG","FAGE","FHTCM","FEDUCYRS")){
    fullY <- pardat[complete.cases(merge2),yVar]
    Y <- fullY[!is.na(fullY)]
    X <- impdat[!is.na(fullY),-1]
  
    thisimp <- SuperLearner(
      Y=Y, verbose=TRUE,
      X=X,
      SL.library=list(c("SL.glm","All"),
                      c("SL.glm","screen.corP"),
                      c("SL.glm","screen.corP"),
                      c("SL.randomForest","screen.corP"),
                      c("SL.randomForest","screen.corP01"),
                      c("SL.step.forward","All"),
                      c("SL.step.forward","screen.corP"),
                      c("SL.step.interaction","screen.corP01")
                      )
    )
  save(thisimp, file=file.path(saveDir,paste0("SLimpfit_",yVar,".RData")))
  }
}else{
  for(yVar in c("MHTCM","MWTKG","FAGE","FHTCM","FEDUCYRS")){
    load(file.path(saveDir,paste0("SLimpfit_",yVar,".RData")))
    # which are missing
    theseMissings <- is.na(pardat[complete.cases(merge2),yVar])
    # missing indicator
    eval(parse(text=paste0(
      "impdat$",yVar,"_IMPIND <- as.numeric(theseMissings)"
    )))
    eval(parse(text=paste0(
      "impdat$",yVar,"_IMP[!theseMissings] <- pardat$",yVar,"[complete.cases(merge2)][!theseMissings]"
    )))
    eval(parse(text=paste0(
      "impdat$",yVar,"_IMP[theseMissings] <- predict(thisimp,newdata=impdat[theseMissings,-1])$pred"
    )))
    rm(thisimp)
  }
}

impdat$MBMI_IMP <- with(impdat, MWTKG_IMP/((MHTCM_IMP/100)^2))
impdat$MBMI_IMPIND <- with(impdat, as.numeric(MWTKG_IMPIND==1 | MHTCM_IMPIND==1))

## outcome measures
qsdat <- read.xport(file.path(dataDir, "QS.xpt"))

viqdat <- data.frame(SUBJID = qsdat$SUBJID[qsdat$QSTEST=="WASI Verbal IQ"],
                     VIQ_Outcome = qsdat$QSSTRESN[qsdat$QSTEST=="WASI Verbal IQ"])
piqdat <- data.frame(SUBJID = qsdat$SUBJID[qsdat$QSTEST=="WASI Performance IQ"],
                     PIQ_Outcome = qsdat$QSSTRESN[qsdat$QSTEST=="WASI Performance IQ"])
fiqdat <- data.frame(SUBJID = qsdat$SUBJID[qsdat$QSTEST=="WASI Full IQ"],
                     FIQ_Outcome = qsdat$QSSTRESN[qsdat$QSTEST=="WASI Full IQ"])

blockdat <- data.frame(SUBJID = qsdat$SUBJID[qsdat$QSTEST=="WASI Block Design Score - Raw"],
                       BLOCK_Outcome = qsdat$QSSTRESN[qsdat$QSTEST=="WASI Block Design Score - Raw"])
vocabdat <- data.frame(SUBJID = qsdat$SUBJID[qsdat$QSTEST=="WASI Vocabulary Score - Raw"],
                       VOCAB_Outcome = qsdat$QSSTRESN[qsdat$QSTEST=="WASI Vocabulary Score - Raw"])
matrixdat <- data.frame(SUBJID = qsdat$SUBJID[qsdat$QSTEST=="WASI Matrix Reasoning Score - Raw"],
                        MATRIX_Outcome = qsdat$QSSTRESN[qsdat$QSTEST=="WASI Matrix Reasoning Score - Raw"])
simdat <- data.frame(SUBJID = qsdat$SUBJID[qsdat$QSTEST=="WASI Similarities Score - Raw"],
                     SIM_Outcome = qsdat$QSSTRESN[qsdat$QSTEST=="WASI Similarities Score - Raw"])

## merge outcome measures
dat1 <- merge(x=impdat, y=viqdat, all.x=FALSE)
dat2 <- merge(x=dat1, y=piqdat)
dat3 <- merge(x=dat2, y=fiqdat)
dat4 <- merge(x=dat3, y=blockdat)
dat5 <- merge(x=dat4, y=vocabdat)
dat6 <- merge(x=dat5, y=matrixdat)
dat7 <- merge(x=dat6, y=simdat)

# create hospital indicators
for(h in unique(dat7$SITEID)[-1]){
  eval(parse(text=paste0(
    "dat7$SITEID",h," <- as.numeric(dat7$SITEID==h)"
  )))
}

# SEX ARMCD STRATUM1 STRATUM2 STRATUM3
# GAGEBRTH BIRTHWT
# BIRTHLEN BIRTHHC APGAR1 APGAR5
dat7 <- dat7[,-which(colnames(dat7)=="SITEID")]

dat7$SEX_ARMCD_Interaction<- with(dat7, SEX*ARMCD)
dat7$SEX_GAGEBRTH_Interaction<- with(dat7, SEX*GAGEBRTH)
dat7$SEX_BIRTHWT_Interaction<- with(dat7, SEX*BIRTHWT)
dat7$SEX_BIRTHLEN_Interaction<- with(dat7, SEX*BIRTHLEN)
dat7$SEX_APGAR1_Interaction<- with(dat7, SEX*APGAR1)
dat7$SEX_APGAR5_Interaction<- with(dat7, SEX*APGAR5)
dat7$SEX_ARMCD_Interaction<- with(dat7, SEX*ARMCD)

dat7$ARMCD_GAGEBRTH_Interaction<- with(dat7, GAGEBRTH*ARMCD)
dat7$BIRTHWT_GAGEBRTH_Interaction<- with(dat7, BIRTHWT*GAGEBRTH)
dat7$BIRTHLEN_GAGEBRTH_Interaction<- with(dat7, BIRTHLEN*GAGEBRTH)
dat7$APGAR1_GAGEBRTH_Interaction<- with(dat7, APGAR1*GAGEBRTH)
dat7$APGAR5_GAGEBRTH_Interaction<- with(dat7, APGAR5*GAGEBRTH)
dat7$ARMCD_GAGEBRTH_Interaction<- with(dat7, ARMCD*GAGEBRTH)

dat7$BIRTHWT_BIRTHLEN_Interaction <- with(dat7, BIRTHWT*BIRTHLEN)
dat7$BIRTHWT_BIRTHHC_Interaction <- with(dat7, BIRTHWT*BIRTHHC)
dat7$BIRTHWT_APGAR1_Interaction <- with(dat7, BIRTHWT*APGAR1)
dat7$BIRTHWT_APGAR5_Interaction <- with(dat7, BIRTHWT*APGAR5)
dat7$BIRTHWT_ARMCD_Interaction <- with(dat7, BIRTHWT*ARMCD)

dat7$BIRTHLEN_BIRTHHC_Interaction <- with(dat7, BIRTHLEN*BIRTHHC)
dat7$BIRTHLEN_APGAR1_Interaction <- with(dat7, BIRTHLEN*APGAR1)
dat7$BIRTHLEN_APGAR5_Interaction <- with(dat7, BIRTHLEN*APGAR5)
dat7$BIRTHLEN_ARMCD_Interaction <- with(dat7, BIRTHLEN*ARMCD)

dat7$APGAR1_BIRTHHC_Interaction <- with(dat7, APGAR1*BIRTHHC)
dat7$APGAR5_BIRTHHC_Interaction <- with(dat7, APGAR5*BIRTHHC)
dat7$APGAR5_ARMCD_Interaction <- with(dat7, APGAR5*ARMCD)

dat7$APGAR1_APGAR5_Interaction <- with(dat7, APGAR5*APGAR1)
dat7$ARMCD_APGAR5_Interaction <- with(dat7, APGAR5*ARMCD)

dat <- dat7
save(dat,file="H:/GHAP/PROBIT/analysisData.RData")

validRows <- split(sample(1:nrow(dat)), rep(1:10, length = nrow(dat)))
save(validRows, file="H:/GHAP/PROBIT/cvRows.RData")


# Super Learner library
mySL.library <- list(
  # basic glms
  c("SL.glm","screen.noInt"),
  c("SL.glm.interaction","screen.hosptrt"),
  c("SL.glm","screen.noInt.corP"),
  c("SL.glm","screen.noInt.corP01"),
  c("SL.glm","screen.glmnet"),
  # stepwise glms
  c("SL.step.forward", "All"),
  c("SL.step.forward","screen.noInt"),
  c("SL.step.forward","screen.noInt.corP"),
  c("SL.step.forward","screen.noInt.corP01"),
  c("SL.step.forward","screen.glmnet"),
  # gams
  c("SL.gam","screen.noInt.corP01"),
  c("SL.gam","screen.glmnet"),
  # polymars
  c("SL.polymars","screen.noInt.corP01"),
  c("SL.polymars","screen.glmnet"),
  # random forest
  c("SL.randomForest","screen.noInt"),
  # gbm
  c("SL.gbm","screen.noInt"),
  c("SL.gbm3","screen.noInt"),
  # neural network
  c("SL.nnet2","screen.noInt"),
  c("SL.nnet3","screen.noInt"),
  c("SL.nnet5","screen.noInt"),
  # glm net
  c("SL.glmnet","All"),
  c("SL.glmnet05","All"),
  c("SL.glmnet0","All"),
  # rpart
  c("SL.rpartPrune","screen.noInt"),
  c("SL.rpartPrune","screen.noInt.corP"),
  c("SL.rpartPrune","screen.noInt.corP01"),
  # support vector machine
  c("SL.svm","screen.noInt"),
  # mean 
  c("SL.mean","screen.noInt")
)



