
##hv: Supporting functions
library(trustOptim)
library(GJRM)   # Version controle: 0.2  09.01.2019
library(ggplot2)
library(copula)
#library(VineCopula)
library(VC2copula)
library(parallel)
CopChooser <- function(Cop, theta) {
  require(VC2copula)
  if(Cop=="C0" | Cop=="C") return(claytonCopula(param = theta, dim = 2))
  if(Cop=="C90") return(VC2copula::r90ClaytonCopula(param = theta))
  if(Cop=="C180") return(surClaytonCopula(param = theta))
  if(Cop=="C270") return(r270ClaytonCopula(param = theta))
  if(Cop=="F") return(frankCopula(param = theta, dim = 2))
  if(Cop=="G0" | Cop=="G") return(gumbelCopula(param = theta, dim = 2))
  if(Cop=="J0" | Cop=="J") return(joeCopula(param = theta, dim = 2))
  if(Cop=="N") return(normalCopula(param = theta, dim = 2))
  stop(paste(Cop, "is not a valid copula.", collapse=""))
}

rps <- function(x,d.test){
  rps.vec <- c()
  result <- rep(1, dim(d.test)[1])
  result[which(d.test[,3]==d.test[,38])] <- 2
  result[which(d.test[,3] < d.test[,38])] <- 3
  result.vec <- matrix(rep(0,3*dim(d.test)[1]), ncol=3)
  for (i in 1:dim(d.test)[1]) {
    result.vec[i,result[i]] <- 1
  }
  for (i in 1:dim(d.test)[1]) {
    rps.i <- 0.5*sum((cumsum(as.numeric(x[i,2:3])) - cumsum(as.numeric(result.vec[i,-3])))^2)
    rps.vec <- c(rps.vec,rps.i)
  }
  likelihood <- result.vec*x[,2:4]
  welche <- which(likelihood>0)
  probs <- c(c(likelihood)$win1, c(likelihood)$draw, c(likelihood)$win2)
  likelihood <- probs[welche]
  
  list(rps.vec, result, likelihood)
}

## Data generating process

## gen() - Function to generate Data from bivariate copula-distribution
##   Cop - Chosen Copula, from which is drawn. Options are (ABC)
##   mar - margins, default both poisson, more (maybe) added later.
##     n - sample size
## theta - copula parameter, 1 or 2 dimensional, depending on chosen copula
##  seed - seed to set for randomness. Default is a seed, drawn without seed
##         itself from (1, 2^31)
##  beta - list (of 2) true betas to build the model, beta0 as intercept should 
##         be included

gen <- function(Cop, beta, theta, mar=c("pois","pois"), n, seed) {
  require(copula)
  set.seed(seed)
  p1 <- length(beta[[1]])-1
  p2 <- length(beta[[2]])-1
  dat <- data.frame(i=1:n, y_1=0, y_2=0)
  # create covariates
  X <- matrix(runif(n*(p1+p2), -1, 1), nrow=n)
  
  Copula <- CopChooser(Cop, theta)
  
  myMVDC <- mvdc(Copula, margins=mar, paramMargins = list(
    list(lambda = exp(beta[[1]][1] + X[,1:p1]%*%beta[[1]][-1])),
    list(lambda = exp(beta[[2]][1] + X[,(p1+1):(p1+p2)]%*%beta[[2]][-1]))
  )
  )
  ## sample
  response <- rMvdc(n, myMVDC)
  Erg <- as.data.frame(cbind(response,X))
  return(Erg)
}

plotcop <- function(Cop, theta, n) {
  Copula <- r90ClaytonCopula(param = theta)
  myMVDC <- mvdc(Copula, margins=c("pois","pois"), paramMargins = list(
    list(lambda=1.2), list(lambda=1.4)))
  a <- rMvdc(n, myMVDC)
  plot(jitter(a))
}
#plotcop("C", -1.5, 3000)
## abc: Weird shit with Clayton and negative theta (its [-1, \inf)])



#################### simulation ############################

## calculate fit with independence copula (simple glms)

## single study
## Cop    - True Copula to sample from
## beta   - True beta to sample from
## theta  - True theta (cop paramter) to sample from
##  n     - sample size

Hendrik <- function(Cop, beta, theta, mar=c("pois","pois"), n, 
                    seed=sample(2^31,1)) {
  require(GJRM)
  require(copula)
  require(trustOptim)
  
  p1 <- length(beta[[1]])-1
  p2 <- length(beta[[2]])-1
  
  V <- gen(Cop=Cop, beta=beta, theta=theta, mar=mar, n=n, seed=seed)
  V.1 <- V[,c(1,3:(3+p1-1))]
  V.2 <- V[,c(2,(3+p1):(p1+p2+2))]
  fit.ind1 <- glm(V1 ~ ., data=V.1, family="poisson")
  fit.ind2 <- glm(V2 ~ ., data=V.2, family="poisson")
  eq1 <- paste("V1~", paste(names(V.1)[2:length(names(V.1))], collapse=" + "),
               collapse="")
  eq2 <- paste("V2~",paste(names(V.2)[2:length(names(V.2))], collapse=" + "))  
  eq_list <- list(as.formula(eq1),as.formula(eq2))
  Cops <- c("N","F","G0","C0", "C90","J0")
  gjrmwrapper <- function(Copu) {
    return(gjrm(margins=c("PO","PO"), formula=eq_list, data=V, Model="B", gamlssfit=FALSE,
                BivD=Copu))
  }
  models <- lapply(as.list(Cops), gjrmwrapper)
  aics <- lapply(models, FUN=stats::AIC)
  aicsglm <- AIC(fit.ind1) + AIC(fit.ind2)
  coefs <- matrix(unlist(lapply(models, coefficients)), byrow=T, nrow=length(Cops))
  coef.indep <- c(coefficients(fit.ind1), coefficients(fit.ind2))
  aicresult <- c(aicsglm, unlist(aics))
  coef.gjrm <- coefs[,1:(p1+p2+2)]
  coefs.overall <- rbind(coef.indep,coef.gjrm)
  dimnames(coefs.overall)[[1]] <- c("indep", "N", "F", "G0", "C0", "C90", "J0")
  return(list(coefs.overall, aicresult))
}

## DoSim generates data (n) from chosen copula with chosen parameters and fits
## all models. Repeat 'times' times and report the MSE in beta for each copula.

DoSim <- function(trueCopula, truebeta, truetheta, n, times) {
  ## generate data for n=500 and fit the model. Repeat 100 times with same parameters.
  Sim100 <- replicate(times, Hendrik(trueCopula, beta=truebeta, n=n, theta=truetheta))
  ## Number of copulas in comparison: 6
  zw <- unlist(Sim100[2,])
  SimAICs <- matrix(zw, byrow=TRUE, ncol=length(zw)/times)
  dimnames(SimAICs)[[2]] <- c("indep", "N","F","G0","C0","C90","J0")
  true <- matrix(rep(unlist(truebeta),length(zw)/times), byrow=TRUE, nrow=length(zw)/times)
  Sim100dif <- array(NA, dim=c(length(zw)/times,length(unlist(truebeta)),times))
  for (i in 1:length(Sim100[1,])) {
    Sim100dif[,,i] <- ((Sim100[1,][[i]]) - true)^2
  }
  Sim100dif <- apply(Sim100dif, c(1,3), mean)
  dimnames(Sim100dif)[[1]] <- c("indep", "N","F","G0","C0", "C90","J0")
  return(list(MSEperfit=t(Sim100dif), aics=SimAICs))
}

## function kendall calculates for given tau the corresponding thetas for 
## our copulas.

kendall <- function(tau) {
  require(copula)
  Frankhelper <- function(theta) {
    tau <- 1 - 4/theta * (1 - debye1(theta))
    return(tau)
  }
  Joehelper <- function(theta) {
    k <- 1:(2^20)
    tau <- 1 - 4 * sum( 1 / (k*(theta*k + 2) * (theta*(k-1)+2)))
    return(tau)
  }
  Normalhelper <- function(theta) {
    tau <- 2/pi * asin(theta)
    return(tau)
  }
  inverseFrank <- function(tau) {
    unname(unlist(uniroot((function(x) Frankhelper(x) - tau), lower = -10000, upper = 10000)[1]))
  }
  inverseJoe <- function(tau) {
    unname(unlist(uniroot((function(x) Joehelper(x) - tau), lower = 1, upper = 10000)[1]))
  }
  inverseNormal <- function(tau) {
    unname(unlist(uniroot((function(x) Normalhelper(x) - tau), lower = -1, upper=1)[1]))
  }
  
  ## Normal
  th.N <- inverseNormal(tau)
  ## Frank
  th.F <- inverseFrank(tau)
  ## Gumbel
  th.G <- 1 / (1-tau)
  if(tau<0)
    th.G <- NA
  ## Clayton
  th.C <- 2*tau / (1-tau)
  if(tau<0) th.C <- NA
  ## Clayton90
  th.C90 <- -2*(-tau) / (1+tau)
  if(tau>0) th.C90 <- NA
  ## Joe
  th.J <- ifelse(tau < 0, NA, inverseJoe(tau))
  return(data.frame(th.N = th.N, th.F = th.F,
                    th.G = th.G, th.C = th.C,
                    th.C90 = th.C90, th.J = th.J))
}


## Function DoAllSims is a wrapper for DoSim and calculates the simulations
## for all Copulas.

DoAllSims <- function(n, times, tau, truebeta){
  require(trustOptim)
  require(GJRM) 
  AICpicks <- NULL
  MSEErg <- NULL
  kend.tau <- kendall(tau)
  for (i in c("N","F","G0","C0","C90","J0")) {   #true copula
    print(paste("#### TRUE COPULA IS", i, collapse=""))
    truetheta <- as.numeric(kend.tau[which(i == c("N","F","G0","C0", "C90","J0"))])
    if(is.na(truetheta))
      next
    zw <- DoSim(i, truebeta=truebeta, truetheta = truetheta,
                n = 250, times = times)
    MSEs <- c(zw$MSEperfit)
    
    ## current data.frame
    m <- data.frame(MSE = c(zw$MSEperfit), fit = c(rep("indep", times), rep("N", times),
                                                   rep("F", times), rep("G0", times),
                                                   rep("C0", times), rep("C90",times),
                                                   rep("J0", times)),
                    true = rep(i, times*7))
    ## bind to previous data
    MSEErg <- rbind(MSEErg,m)
    
    ## look at AICs:
    AICpicks <- rbind(AICpicks, table(factor(c("indep","N","F","G0","C0","C90","J0")[apply(zw$aics, 1, which.min)],
                                             levels=c("indep","N","F","G0","C0","C90","J0"))))
  }
  if(tau>=0)
    dimnames(AICpicks)[[1]] <- c("N","F","G0","C0","J0")
  if(tau<0)
    dimnames(AICpicks)[[1]] <- c("N", "F", "C90")
  return(list(MSEErg=MSEErg, AICpicks=AICpicks))
}

DoAllSimswrapper <- function(tau) {
  print(tau)
  return(DoAllSims(250, 100, tau = tau, truebeta = list(c(0.5, 0.2, -0.2, 0), c(0.2, -0.3, 0.1, 0.5))))
}


## Sim2 fuerhrt die zweite Simulation (gleiche betas) Marra vs. Neu fuer eine
## ausgewaehlte Copula times mal durch mit Stichprobenumfang n.

Sim2 <- function(Cop, theta, times, n, truebeta) {
  msemarra <- c()
  msemeine <- c()
  for (i in 1:times) {
    V <- gen(Cop=Cop, beta=list(truebeta, truebeta), n = n, theta=as.numeric(theta), seed=sample(2^31,1))
    p1 <- p2 <- length(truebeta)-1
    V.1 <- V[,c(1,3:(3+p1-1))]
    V.2 <- V[,c(2,(3+p1):(p1+p2+2))]
    eq1 <- paste("V1~", paste(names(V.1)[2:length(names(V.1))], collapse=" + "),
                 collapse="")
    eq2 <- paste("V2~",paste(names(V.2)[2:length(names(V.2))], collapse=" + "))  
    eq3 <- ~ 1
    eq_list <- list(as.formula(eq1),as.formula(eq2), eq3)
    fit <- gjrm(eq_list, data=V, BivD=Cop, margins=c("PO","PO"), Model="B", linear.equal = NULL)
    fit2 <- gjrm(eq_list, data=V, BivD=Cop, margins=c("PO","PO"), Model="B", linear.equal = rep(TRUE, 4))
    coef1 <- coefficients(fit)
    coef2 <- coefficients(fit2)
    msemarrazw <- mean((coef1[-length(coef1)] - c(truebeta, truebeta))^2)
    msemeinezw <- mean((coef2[-length(coef2)] - c(truebeta, truebeta))^2)
    msemarra <- c(msemarra, msemarrazw)
    msemeine <- c(msemeine, msemeinezw)
  }
  if(Cop=="N" & theta > 0){ Cop <- "N+"}
  if(Cop=="N" & theta < 0){ Cop <- "N-"}
  if(Cop=="F" & theta > 0){ Cop <- "F+"}
  if(Cop=="F" & theta < 0){ Cop <- "F-"}
  d <- data.frame(MSE=c(msemarra, msemeine),
                  version=c(rep("unpenalised", length(msemarra)),
                            rep("penalised", length(msemeine))),
                  "true and fit" = rep(Cop, length(msemarra)+length(msemeine)))
  return(d)
}


