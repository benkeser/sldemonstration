screen.hosptrt <- function(X,...){
  ind <- rep(FALSE, ncol(X))
  ind[which(colnames(X) %in% c("ARMCD","SITEID"))] <- TRUE
  return(ind)
}

screen.noInt.corP <- function(X,minPvalue = 0.05,...){
  ind <- rep(FALSE, ncol(X))
  ind.int <- grepl("Interaction",colnames(X))
  ind.noint <- !ind.int
  
  which.noint <- screen.corP(X=X[ind.noint], minPvalue = minPvalue, ...)
  
  # get rid of other ones
  ind[ind.noint] <- which.noint
  
  return(ind)
}
screen.noInt.corP01 <- function(...,minPvalue=0.01){
  screen.noInt.corP(...,minPvalue=minPvalue)
}

screen.noInt <- function(X,...){
  ind <- rep(TRUE, ncol(X))
  ind[grep("Interaction",colnames(X))] <- FALSE
  return(ind)
}

SL.nnet2 <- SL.nnet
SL.nnet3 <- SL.nnet(...,size=3)
SL.nnet5 <- SL.nnet(...,size=5)

SL.glmnet05 <- function(...,alpha=0.5){
  SL.glmnet(...,alpha=alpha) 
}

SL.glmnet0 <- function(...,alpha=0){
  SL.glmnet(...,alpha=alpha)
}

SL.gbm3 <- function(...,interaction.depth=3){
  SL.gbm(...,interaction.depth=interaction.depth)
}