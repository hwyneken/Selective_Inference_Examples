set.seed(1234)

NSim <- 1000


DecisionVec <- rep(NA,NSim)

alphaThresh <- 0.05 / 2

for (i in 1:NSim) {
  tempX <- rnorm(100)
  
  tempXBar <- mean(tempX)
  tempZ <- 10 * tempXBar / 1
  
  if (tempXBar > 0) {
    tempPVal <- pnorm(tempZ,lower=FALSE)
  }
  else {
    tempPVal <- pnorm(tempZ)
  }
  
  DecisionVec[i] <- ifelse(tempPVal <= alphaThresh,1,0)
}


### next scenario - file drawer if  | tempXBar | < 0.1

DecisionVec2 <- rep(NA,NSim)
alphaThresh2 <- 0.05 / 4 # FPR at 0.05 = 0.30, FPR at 0.05 / 2 = 0.11, FPR at 0.05 / 4 = 0.05
for (i in 1:NSim) {
  tempX <- rnorm(100)
  
  tempXBar <- mean(tempX)
  tempZ <- 10 * tempXBar / 1
  
  if (tempXBar > 0.1) {
    tempPVal <- pnorm(tempZ,lower=FALSE)
  }
  else if (tempXBar < -0.1) {
    tempPVal <- pnorm(tempZ)
  }
  else{
    tempPVal <- NA
  }
  
  if (!is.na(tempPVal)) {
    DecisionVec2[i] <- ifelse(tempPVal <= alphaThresh2,1,0)
  }
}