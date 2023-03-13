## CopChooser straightens the copula notations for gjrm usage.

CopChooser <- function(Cop, theta) {
  require(VineCopula)
  require(VC2copula)
  if(Cop=="C0" | Cop=="C") return(claytonCopula(param = theta, dim = 2))
  if(Cop=="C90") return(r90ClaytonCopula(param = theta))
  if(Cop=="C180") return(surClaytonCopula(param = theta))
  if(Cop=="C270") return(r270ClaytonCopula(param = theta))
  if(Cop=="F") return(frankCopula(param = theta, dim = 2))
  if(Cop=="G0" | Cop=="G") return(gumbelCopula(param = theta, dim = 2))
  if(Cop=="J0" | Cop=="J") return(joeCopula(param = theta, dim = 2))
  if(Cop=="N") return(normalCopula(param = theta, dim = 2))
  stop(paste(Cop, "is not a valid copula.", collapse=""))
}

## genData used in Simulations, generates data.

genData <- function(Cop, theta, seed, n = 1000) {
  require(copula)
  require(MASS)
  require(mnormt)
  set.seed(seed)
  #  x1 <- rnorm(n, mean = 0.5, sd = 3)
  #  x2 <- runif(n, -10, 6)
  #  x3 <- rexp(n, rate = 1)/2
  #  x4 <- rchisq(n, 1)/2
  ####
  #  x5 <- rnorm(n, mean = 2, sd = 2.25)/3
  #  x6 <- runif(n, -6, 3)/4
  #  x7 <- rexp(n, rate = 0.75) /2
  #  x8 <- rchisq(n, 1.5)/2
  
  ## Correlations: x1x2 = 0.8, x1x3 = 0.5, x2x3 = 0.3
  m <- matrix(c(1, 0.9797959, 0.5, 
                0.9797959, 1.5, 0.3674235, 
                0.5, 0.3674235, 1),
              byrow=TRUE, nrow=3)
  temp <- mvrnorm(n = n, mu = c(-1, -1, -1.5), Sigma = m)
  x1 <- temp[,1]
  x2 <- temp[,2]
  x3 <- temp[,3]
  
  ## next collinear block: x4 (margin1) x5 and x6 (margin2), 
  ## correlations as above.
  
  temp <- rmt(n = n, mean = c(-0.25, 1, -1), S = m)
  x4 <- temp[,1]
  x5 <- temp[,2]
  x6 <- temp[,3]
  rm(temp)
  ## Rest:
  x7 <- rnorm(n=n, mean = 2, sd = 0.5)
  x8 <- rnorm(n=n, mean = -1, sd = 0.5)
  
  ## Noise for both margins
  #  nb <- matrix(rnorm(5*n, mean = 5, sd = 2), ncol = 5)
  nb <- matrix(rt(n = 5*n, df = 2), ncol = 5)
  dimnames(nb)[[2]] <- paste0("nb", 1:5)
  
  ## Noise for either margin
  m <- matrix(rnorm(30*n, mean = 5, sd = 2), nrow = n)
  dimnames(m)[[2]] <- c(paste0("n1.", 1:15), paste0("n2.", 1:15))
  
  
  lambda1 <- exp(0.55 + 0.1 * x1 + 0.15 * x2 + 0.1 * x3 - 0.1 * x4)
  lambda2 <- exp(0.75 - 0.2 * x5 + 0.1 * x6 - 0.2 * x7 - 0.25 * x8) 
  
  Copula <- CopChooser(Cop, theta)
  mar <- c("pois","pois")
  myMVDC <- mvdc(Copula, margins=mar, paramMargins = list(
    list(lambda = lambda1),
    list(lambda = lambda2))
  )
  y <- rMvdc(n, myMVDC)
  
  dat <- cbind(y1 = y[,1], y2 = y[,2], x1, x2, x3, x4, x5, x6, x7, x8,
               nb, m)
  
  return(list(as.data.frame(dat),
              eqlist = list(y1 ~ x1 + x2 + x3 + x4 + 
                              nb1 + nb2 + nb3 + nb4 + nb5 +
                              n1.1 + n1.2 + n1.3 + n1.4 + n1.5 +
                              n1.6 + n1.7 + n1.8 + n1.9 + n1.10 + 
                              n1.11 + n1.12 + n1.13 + n1.14 + n1.15,
                            y2 ~ x5 + x6 + x7 + x8 +
                              nb1 + nb2 + nb3 + nb4 + nb5 +
                              n2.1 + n2.2 + n2.3 + n2.4 + n2.5 + 
                              n2.6 + n2.7 + n2.8 + n2.9 + n2.10 +
                              n2.11 + n2.12 + n2.13 + n2.14 + n2.15,
                            ~ 1)))
}

## kendall calculates copula parameters theta for given dependency strength
## kendalls tau. 

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

## calc.probs calculates three-way-probabilties for a validation data set.

calc.probs <- function(fit, d.test, N){
  zw <- sapply(0:N, function(n) jc.probs(fit, n, n, newdata=d.test)$p12)
  if(is.vector(zw))
    prob.draw <- sum(zw)
  else
    prob.draw <- rowSums(zw)
  ## calculate wins
  zw <- c()
  zw2 <- c()
  for (i in 1:N){
    for (j in 0:(i-1)) {
      zw <- cbind(zw, jc.probs(fit, i, j, newdata=d.test)$p12)
      zw2 <- cbind(zw2, jc.probs(fit, j, i, newdata=d.test)$p12)
    }
  }
  prob.win <- rowSums(zw)
  prob.win2 <- rowSums(zw2)
  summ <- prob.win + prob.draw + prob.win2
  prob.draw <- prob.draw / summ
  prob.win <- prob.win / summ
  prob.win2 <- prob.win2 / summ
  return(data.frame(tag1 = d.test$Team, 
                    win1 = prob.win, draw = prob.draw,
                    win2 = prob.win2,
                    tag2 = d.test$Opponent))
}

## rps calculates measures like rps, multinomial llh, class. rate for validation
## data set.

rps <- function(x, d.test){
  rps.vec <- c()
  result <- rep(1, dim(d.test)[1])
  result[which(d.test$Goals==d.test$Goals.oppo)] <- 2
  result[which(d.test$Goals < d.test$Goals.oppo)] <- 3
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
  
  ## classification rate
  classpred <- apply(x[,2:4], 1, which.max)
  list(rps.vec=rps.vec, result=result, likelihood=likelihood, classpred=classpred)
}

## BIC calculation to make sure this works as intended.

myBIC <- function(fit) {
  VCneu <- fit$VC
  VCneu$X1 <- VCneu$X1backup
  VCneu$X2 <- VCneu$X2backup
  n <- dim(VCneu$X1)[2] + dim(VCneu$X2)[2] + dim(VCneu$X3)[2]
  ps <- list(S.h = matrix(0, nrow = n, ncol = n), qu.mag = NULL)
  as.numeric(log(fit$n) * (sum(!fit$coefficients == 0)) - 2 * -fit$VC$func(params = fit$coefficients,
                                                                           respvec = fit$respvec,
                                                                           VC = VCneu, ps = ps)$value)
}

## AIC calculation to make this works as intended.

myAIC <- function(fit) {
  VCneu <- fit$VC
  VCneu$X1 <- VCneu$X1backup
  VCneu$X2 <- VCneu$X2backup
  n <- dim(VCneu$X1)[2] + dim(VCneu$X2)[2] + dim(VCneu$X3)[2]
  ps <- list(S.h = matrix(0, nrow = n, ncol = n), qu.mag = NULL)
  as.numeric(2 * (sum(!fit$coefficients == 0)) - 2 * -fit$VC$func(params = fit$coefficients,
                                                                  respvec = fit$respvec,
                                                                  VC = VCneu, ps = ps)$value)
}

## Likelihood calculation to make sure this works as inteded.

myllh <- function(fit) {
  VCneu <- fit$VC
  VCneu$X1 <- VCneu$X1backup
  VCneu$X2 <- VCneu$X2backup
  n <- dim(VCneu$X1)[2] + dim(VCneu$X2)[2] + dim(VCneu$X3)[2]
  ps <- list(S.h = matrix(0, nrow = n, ncol = n), qu.mag = NULL)
  as.numeric(-fit$VC$func(params = fit$coefficients,respvec = fit$respvec, VC = VCneu, ps = ps)$value)
}


## SingleSim perfoms a single simulation iteration. 

SingleSim <- function(Cop, theta, seed, n, grid.l = 100, K = 25, threshold = 1e-03){
  
  data <- genData(Cop, theta, seed, n)
  zw <- gjrm.lasso(data, Cop, grid.l = grid.l, K = K, CV = TRUE, threshold = threshold)
  
  fit.lasso.bic <- zw[[1]]
  fit.lasso.aic <- zw[[2]]
  fit.lasso.exLLH <- zw[[3]]
  fit.vanilla <- gjrm(data[[2]], data=data[[1]], BivD = Cop, margins = c("PO","PO"), Model="B", 
                      linear.equal = rep(F, 25), LASSO = FALSE)
  
  # from genData:
  #lambda1 <- exp(0.55 + 0.1 * x1 + 0.15 * x2 + 0.1 * x3 - 0.1 * x4)
  #lambda2 <- exp(0.75 - 0.2 * x5 + 0.1 * x6 - 0.2 * x7 - 0.25 * x8) 
  
  echte <- c(0.55, 0.1, 0.15, 0.1, -0.1, rep(0, 20),
             0.75, -0.2, 0.1, -0.2, -0.25, rep(0, 20))
  

  
  MSE.lasso.bic <- sum((fit.lasso.bic$coefficients[-51] - echte)^2)
  MSE.lasso.aic <- sum((fit.lasso.aic$coefficients[-51] - echte)^2)
  MSE.lasso.exLLH <- sum((fit.lasso.exLLH$coefficients[-51] - echte)^2)
  MSE.vanilla <- sum((fit.vanilla$coefficients[-51] - echte)^2)
  
  ## positive/negative rates:
  bic.true.positive <- sum((fit.lasso.bic$coefficients[-51] != 0) & (echte != 0))/10
  bic.true.negative <- sum((fit.lasso.bic$coefficients[-51] == 0) & (echte == 0))/40
  aic.true.positive <- sum((fit.lasso.aic$coefficients[-51] != 0) & (echte != 0))/10
  aic.true.negative <- sum((fit.lasso.aic$coefficients[-51] == 0) & (echte == 0))/40
  exllh.true.positive <- sum((fit.lasso.exLLH$coefficients[-51] != 0) & (echte != 0))/10
  exllh.true.negative <- sum((fit.lasso.exLLH$coefficients[-51] == 0) & (echte == 0))/40
  vanilla.true.positive <- sum((fit.vanilla$coefficients[-51] != 0) & (echte != 0))/10
  vanilla.true.negative <- sum((fit.vanilla$coefficients[-51] == 0) & (echte == 0))/40
  nus <- c(zw[[4]][c(3,1,2)])
  dfs <- numeric(4)
  dfs[1] <- zw$dfs[zw$grid == nus[1]]
  dfs[2] <- zw$dfs[zw$grid == nus[2]]
  dfs[3] <- zw$dfs[zw$grid == nus[3]]
  dfs[4] <- max(zw$dfs)
  
  erg <- data.frame(MSE = c(MSE.lasso.exLLH,
                            MSE.lasso.bic,
                            MSE.lasso.aic,
                            MSE.vanilla),
                    true.positive = c(exllh.true.positive,
                                      bic.true.positive,
                                      aic.true.positive,
                                      vanilla.true.positive),
                    true.negative = c(exllh.true.negative,
                                      bic.true.negative,
                                      aic.true.negative,
                                      vanilla.true.negative),
                    Modell = c("exLLH", "BIC", "AIC", "vanilla"),
                    nu = c(nus,0),
                    dfs = dfs + 3)
  return(erg)
}



## exLLH calculates the external likelihood

exLLH <- function(fit.here, test){
  VC2 <- fit.here$VC
  welche1 <- dimnames(VC2$X1)[[2]]
  welche2 <- dimnames(VC2$X2)[[2]]
  welche3 <- dimnames(VC2$X3)[[2]]
  VC2$X1 <- cbind("(Intercept)" = 1, test[,dimnames(test)[[2]] %in% welche1 |
                                            paste0(dimnames(test)[[2]], "1") %in% welche1 |
                                            paste0(dimnames(test)[[2]], "TRUE") %in% welche1])
  VC2$X2 <- cbind("(Intercept)" = 1, test[,dimnames(test)[[2]] %in% welche2 |
                                            paste0(dimnames(test)[[2]], "1") %in% welche2 |
                                            paste0(dimnames(test)[[2]], "TRUE") %in% welche2])
  VC2$X3 <- cbind(1, test[,dimnames(test)[[2]] %in% welche3 |
                            paste0(dimnames(test)[[2]], "1") %in% welche3 |
                            paste0(dimnames(test)[[2]], "TRUE") %in% welche3])
  ## reparatur per Hand:
  if(length(welche2) != 2) #Sonderfall
    VC2$X2 <- VC2$X2[,dimnames(fit.here$VC$X2)[[2]]]
  if(length(welche1) != 2) #Sonderfall
    VC2$X1 <- VC2$X1[,dimnames(fit.here$VC$X1)[[2]]]
  
  eq1 <- fit.here$formula[[1]]
  eq2 <- fit.here$formula[[2]]
  dim1 <- length(test[,as.character(eq1[2])])
  maxg <- max(c(test[,as.character(eq1[2])], test[,as.character(eq2[2])]))
  #browser()
  VC2$y1m <- matrix(NA, nrow = dim1,
                    ncol = maxg+1)
  VC2$y2m <- matrix(NA, nrow = dim1,
                    ncol = maxg+1)
  VC2$n <- dim1
  VC2$weights <- rep(1, dim1)
  ## 
  zwischen1 <- data.matrix(VC2$X1)
  zwischen1[VC2$X1 != zwischen1] <- zwischen1[VC2$X1 != zwischen1] - 1
  VC2$X1 <- zwischen1
  zwischen2 <- data.matrix(VC2$X2)
  zwischen2[VC2$X2 != zwischen2] <- zwischen2[VC2$X2 != zwischen2] - 1
  VC2$X2 <- zwischen2
  VC2$X3[,-1] <- VC2$X3[,-1] -1
  rm(zwischen1, zwischen2)
  for(i in 1:dim1){
    VC2$y1m[i,1:(test[,as.character(eq1[2])][i]+1)] <- 0:(test[,as.character(eq1[2])][i])
    VC2$y2m[i,1:(test[,as.character(eq2[2])][i]+1)] <- 0:(test[,as.character(eq2[2])][i])
  }
  VC2$X3 <- as.matrix(VC2$X3)
  ps <- NULL
  ps$S.h <- 0
  res <- -fit.here$VC$func(fit.here$fit$argument, 
                                                list(y1 = test[,as.character(eq1[2])], 
                                                     y2 = test[,as.character(eq2[2])]), 
                                                VC = VC2, ps = ps)$value
  return(res)
}

## blockstand used in the case of group lasso, when a group of covariates needs
## to be standardized as a group.

blockstand <- function(x, ipen.which, inotpen.which)
{
  ## Author: Lukas Meier, Date:  4 Aug 2006, 08:50
  
  n <- nrow(x)
  x.ort <- x
  scale.pen <- list(); length(scale.pen) <- length(ipen.which)
  scale.notpen <- NULL
  
  if(length(inotpen.which) > 0){
    one <- rep(1, n)
    scale.notpen <- sqrt(drop(one %*% (x[,inotpen.which]^2)) / n)
    x.ort[,inotpen.which] <- scale(x[,inotpen.which], FALSE, scale.notpen)
  }
  
  for(j in 1:length(ipen.which)){
    ind <- ipen.which[[j]]
    decomp <- qr(x[,ind])
    if(decomp$rank < length(ind)) ## Warn if block has not full rank
      stop("Block belonging to columns ", paste(ind, collapse = ", "),
           " has not full rank! \n")
    scale.pen[[j]] <- qr.R(decomp) * 1 / sqrt(n)
    x.ort[,ind] <- qr.Q(decomp) * sqrt(n)
  }
  list(x = x.ort, scale.pen = scale.pen, scale.notpen = scale.notpen)
}

## tipico.cor is used to calculate fictional betting results for the World Cup
## 2018. cor in this case refers to a corrected version.
# GamePred - Game predictions in specific data.frame format.
# tau.bet - threshold for bets taken. 1 = all bets that are fair and better.
# kelly - indicates, if kelly-strategy to calculate stakes should be used.
# trueresult - data frame with true results of world cup

tipico.cor <- function(GamePred, tau.bet = 1, kelly = FALSE, trueresult, bet){
  expected <- GamePred[,2:4] * bet[,3:5]
  bets.to.take <- (expected >= tau.bet) * bet[,3:5] 
  ## only one per game
  for (i in 1:64) {
    bets.to.take[i,-which.max(expected[i,])] <- 0
  }
  bets.to.take <- bets.to.take >= tau.bet
  n <- sum(bets.to.take)
  
  if(kelly==FALSE){
    entry.bets <- n
    wins <- sum((bets.to.take * trueresult) * bet[,3:5]) - entry.bets
    win.ratio <- wins/entry.bets
  }
  if(kelly==TRUE){
    K <- ((bet[,3:5]-1) * GamePred[,2:4] - (1-GamePred[,2:4])) /
      (bet[,3:5]-1)
    entry.bets <- sum(K * bets.to.take)
    wins <- sum((bets.to.take * trueresult) * bet[,3:5] * K) - entry.bets
    win.ratio <- wins/entry.bets
  }
  return(data.frame(win.ratio=win.ratio, bets.placed = n))
}

## a little helper function to create pathplots
pathplot <- function(fit, ...){
  par(mfrow = c(1,2))
  m <- fit$m
  grid.l <- dim(m)[2]
  p <- dim(m)[1]
  minim <- min(m[-c(1, (p/2)+1),])
  maxim <- max(m[-c(1, (p/2)+1),])
  plot(fit$grid, m[1,], type = "n", ylim = c(minim, maxim), xlab = expression(nu),
       ylab = expression(beta["i"]), ...)
  for(i in c(2:(p/2), ((p/2)+2):p)){
    points(fit$grid, m[i,], type = "l")
  }
  abline(v = fit$nus[1], lty = 2)
  #text(x = fit$nus[1] - max(fit$grid)*0.03, y = max(m)*0.9, labels = "BIC")
  abline(v = fit$nus[2], lty = 2)
  #text(x = fit$nus[2] - max(fit$grid)*0.03, y = max(m)*0.9, labels = "AIC")
  abline(v = fit$nus[3], lty = 2)
  #text(x = fit$nus[3] + max(fit$grid)*0.04, y = max(m)*0.9, labels = "exLLH")
  
  plot(1:grid.l, rev(m[1,]), type = "n", ylim = c(minim, maxim), xlab = expression("Index of"~~ nu),
       ylab = expression(beta["i"]), ...)
  for(i in c(2:(p/2), ((p/2)+2):p)){
    points(1:length(fit$grid), rev(m[i,]), type = "l")
  }
  abline(v = which(rev(fit$grid) == fit$nus[1]), lty = 2)
  #text(x = which(rev(fit$grid) == fit$nus[1]) - grid.l*0.03, y = max(m)*0.9, labels = "BIC")
  abline(v = which(rev(fit$grid) == fit$nus[2]), lty = 2)
  #text(x = which(rev(fit$grid) == fit$nus[2]) - grid.l*0.03, y = max(m)*0.9, labels = "AIC")
  abline(v = which(rev(fit$grid) == fit$nus[3]), lty = 2)
  #text(x = which(rev(fit$grid) == fit$nus[3]) + grid.l*0.03, y = max(m)*0.9, labels = "exLLH")
}

## Implementation of CV, cycling through 5 tournaments.
## Inside each 4/5 fold gjrm.lasso is called, performing a 10-fold-CV in 
## order to optimise the nu

## as of now, for LASSO-only, the start values will NOT be carried over
## see: carry.start.values = FALSE
## and for LASSO+equal, they will be carried. In our experience (both simulation
## and football application) this leads to more robust results.

WM.KV <- function(Jahr, Cop, bet = NULL){
  train <- data[data$WM != Jahr,]
  test <- data[data$WM == Jahr,]
  ## centering group variables:
  train[,16:19] <- scale(train[,16:19], center = TRUE, scale = FALSE)
  train[,38:41] <- scale(train[,38:41], center = TRUE, scale = FALSE)
  test[,16:19] <- scale(test[,16:19], center = TRUE, scale = FALSE)
  test[,38:41] <- scale(test[,38:41], center = TRUE, scale = FALSE)
  
  fitmargin1 <- glm(eqlist[[1]], train, family = "poisson")
  fitmargin2 <- glm(eqlist[[2]], train, family = "poisson")
  fitted <- gjrm.lasso(list(train, eqlist), Cop, plot = TRUE, grid.l = 250, K = 10, linear.equal = NULL,
                       LASSO.groups = list(c(14:17)), CV = TRUE, max.nu = NULL, threshold = 1e-02,
                       CV.seed = 2505 + as.numeric(Jahr), carry.start.values = FALSE)
  fitcop <- gjrm(formula = eqlist, data = train, Model = "B", BivD = Cop, margins = c("PO", "PO"),
                 linear.equal = NULL)
  fitequal <- gjrm(formula = eqlist, data = train, Model = "B", BivD = Cop, margins = c("PO", "PO"),
                   linear.equal = rep(TRUE, 22))
  fitequal.lasso.exllh.zw <- gjrm.lasso(list(train, eqlist), Cop, plot = TRUE, grid.l = 250, K = 10, linear.equal = rep(TRUE, 22),
                                        LASSO.groups = list(c(14:17)), CV = TRUE, max.nu = NULL, threshold = 1e-02,
                                        CV.seed = 2505 + as.numeric(Jahr), carry.start.values = TRUE)
  
  fitequal.lasso.exllh <- fitequal.lasso.exllh.zw[[3]]
  fitequal.lasso.exllh.nu <- fitequal.lasso.exllh.zw$nus
  rm(fitequal.lasso.exllh.zw)
  
  VC2 <- fitcop$VC
  ## recontruct VC to calculate the external likelihood
  welche1 <- dimnames(VC2$X1)[[2]]
  welche2 <- dimnames(VC2$X2)[[2]]
  welche3 <- dimnames(VC2$X3)[[2]]
  VC2$X1 <- cbind("(Intercept)" = 1, test[,dimnames(test)[[2]] %in% welche1 |
                                            paste0(dimnames(test)[[2]], "1") %in% welche1 |
                                            paste0(dimnames(test)[[2]], "TRUE") %in% welche1])
  VC2$X2 <- cbind("(Intercept)" = 1, test[,dimnames(test)[[2]] %in% welche2 |
                                            paste0(dimnames(test)[[2]], "1") %in% welche2 |
                                            paste0(dimnames(test)[[2]], "TRUE") %in% welche2])
  VC2$X3 <- cbind(1, test[,dimnames(test)[[2]] %in% welche3 |
                            paste0(dimnames(test)[[2]], "1") %in% welche3 |
                            paste0(dimnames(test)[[2]], "TRUE") %in% welche3])
  ## repairing by hand, if order is wrong:
  VC2$X2 <- VC2$X2[,dimnames(fitcop$VC$X2)[[2]]]
  VC2$X1 <- VC2$X1[,dimnames(fitcop$VC$X1)[[2]]]
  
  eq1 <- eqlist[[1]]
  eq2 <- eqlist[[2]]
  dim1 <- length(test[,as.character(eq1[2])])
  maxg <- max(c(test[,as.character(eq1[2])], test[,as.character(eq2[2])]))
  
  VC2$y1m <- matrix(NA, nrow = dim1,
                    ncol = maxg+1)
  VC2$y2m <- matrix(NA, nrow = dim1,
                    ncol = maxg+1)
  VC2$n <- dim1
  VC2$weights <- rep(1, dim1)
  
  zwischen1 <- data.matrix(VC2$X1)
  zwischen1[VC2$X1 != zwischen1] <- zwischen1[VC2$X1 != zwischen1] - 1
  VC2$X1 <- zwischen1
  zwischen2 <- data.matrix(VC2$X2)
  zwischen2[VC2$X2 != zwischen2] <- zwischen2[VC2$X2 != zwischen2] - 1
  VC2$X2 <- zwischen2
  VC2$X3[,-1] <- VC2$X3[,-1] -1
  rm(zwischen1, zwischen2)
  for(i in 1:dim1){
    VC2$y1m[i,1:(test[,as.character(eq1[2])][i]+1)] <- 0:(test[,as.character(eq1[2])][i])
    VC2$y2m[i,1:(test[,as.character(eq2[2])][i]+1)] <- 0:(test[,as.character(eq2[2])][i])
  }
  VC2$X3 <- as.matrix(VC2$X3)
  ## these settings are imported from GJRM. We did not change any of these.
  VC2$max.pr <- 0.999999
  VC2$min.dn <- 1e-40
  VC2$min.pr <- 1e-40
  ps <- NULL
  ps$S.h <- 0
  exllh.exLLH <- -fitted$fit.exLLH$VC$func(fitted$fit.exLLH$fit$argument, 
                                           list(y1 = test[,as.character(eq1[2])], 
                                                y2 = test[,as.character(eq2[2])]), 
                                           VC = VC2, ps = ps)$value
  exllh.bic <- -fitted$fit.exLLH$VC$func(fitted$fit.bic$fit$argument, 
                                         list(y1 = test[,as.character(eq1[2])], 
                                              y2 = test[,as.character(eq2[2])]), 
                                         VC = VC2, ps = ps)$value
  exllh.aic <- -fitted$fit.exLLH$VC$func(fitted$fit.aic$fit$argument, 
                                         list(y1 = test[,as.character(eq1[2])], 
                                              y2 = test[,as.character(eq2[2])]), 
                                         VC = VC2, ps = ps)$value
  exllh.cop <- -fitcop$VC$func(fitcop$fit$argument, 
                               list(y1 = test[,as.character(eq1[2])], 
                                    y2 = test[,as.character(eq2[2])]), 
                               VC = VC2, ps = ps)$value
  
  exllh.equal <- exLLH(fitequal, test)
  exllh.equal.lasso <- exLLH(fitequal.lasso.exllh, test)
  
  #exllh.pois below.
  
  
  #########################################
  lambda.hat.BIC1 <- exp(predict(fitted[[1]], newdata=test, eq=1))
  lambda.hat.BIC2 <- exp(predict(fitted[[1]], newdata=test, eq=2))
  lambda.hat.AIC1 <- exp(predict(fitted[[2]], newdata=test, eq=1))
  lambda.hat.AIC2 <- exp(predict(fitted[[2]], newdata=test, eq=2))
  lambda.hat.exLLH1 <- exp(predict(fitted[[3]], newdata=test, eq=1))
  lambda.hat.exLLH2 <- exp(predict(fitted[[3]], newdata=test, eq=2))
  lambda.hat.cop1 <- exp(predict(fitcop, newdata = test, eq=1))
  lambda.hat.cop2 <- exp(predict(fitcop, newdata = test, eq=2))
  
  lambda.hat.equal1 <- exp(predict(fitequal, newdata=test, eq=1))
  lambda.hat.equal2 <- exp(predict(fitequal, newdata=test, eq=2))
  lambda.hat.equal.lasso1 <- exp(predict(fitequal.lasso.exllh, newdata=test, eq=1))
  lambda.hat.equal.lasso2 <- exp(predict(fitequal.lasso.exllh, newdata=test, eq=2))
  
  pred1 <- predict(fitmargin1, newdata = test, type = "response")
  pred2 <- predict(fitmargin2, newdata = test, type = "response")
  
  ## exllh pois
  exllh.pois <- sum(log(dpois(test$Goals, pred1) * dpois(test$Goals.oppo, pred2)))
  
  ## skellam positive = Win Team 1
  # draw:
  p.draw <- dskellam(0, lambda1 = pred1, lambda2 = pred2)
  # loss:
  p.loss <- pskellam(-1, lambda1 = pred1, lambda2 = pred2)
  # win:
  p.win <- 1 - p.draw - p.loss
  
  Pois.GamePred <- data.frame(tag = test$Team, 
                              win1 = p.win,
                              draw = p.draw,
                              win2 = p.loss,
                              tag2 = test$Opponent)
  BIC.GamePred <- calc.probs(fitted$fit.bic, test, 12) 
  AIC.GamePred <- calc.probs(fitted$fit.aic, test, 12)
  exLLH.GamePred <- calc.probs(fitted$fit.exLLH, test, 12)
  cop.GamePred <- calc.probs(fitcop, test, 12)
  equal.GamePred <- calc.probs(fitequal, test, 12)
  equal.lasso.GamePred <- calc.probs(fitequal.lasso.exllh, test, 12)
  
  zwBIC <- rps(BIC.GamePred, test)
  zwAIC <- rps(AIC.GamePred, test)
  zwPois <- rps(Pois.GamePred, test)
  zwexLLH <- rps(exLLH.GamePred, test)
  zwcop <- rps(cop.GamePred, test)
  zwequal <- rps(equal.GamePred, test)
  zwequal.lasso <- rps(equal.lasso.GamePred, test)
  
  # rps
  rpsBIC <- mean(zwBIC$rps.vec)
  rpsAIC <- mean(zwAIC$rps.vec)
  rpsPois <- mean(zwPois$rps.vec)
  rpsexLLH <- mean(zwexLLH$rps.vec)
  rpscop <- mean(zwcop$rps.vec)
  rpsequal <- mean(zwequal$rps.vec)
  rpsequal.lasso <- mean(zwequal.lasso$rps.vec)
  # likelihood
  llhBIC <- mean(zwBIC$likelihood)
  llhAIC <- mean(zwAIC$likelihood)
  llhPois <- mean(zwPois$likelihood)
  llhexLLH <- mean(zwexLLH$likelihood)
  llhcop <- mean(zwcop$likelihood)
  llhequal <- mean(zwequal$likelihood)
  llhequal.lasso <- mean(zwequal.lasso$likelihood)
  
  crBIC <- mean(zwBIC$classpred == zwBIC$result)
  crAIC <- mean(zwAIC$classpred == zwAIC$result)
  crPois <- mean(zwPois$classpred == zwPois$result)
  crexLLH <- mean(zwexLLH$classpred == zwexLLH$result)
  crcop <- mean(zwcop$classpred == zwcop$result)
  crequal <- mean(zwequal$classpred == zwequal$result)
  crequal.lasso <- mean(zwequal.lasso$classpred == zwequal.lasso$result)
  
  MSEBIC <- mean((test$Goals - lambda.hat.BIC1)^2 + (test$Goals.oppo - lambda.hat.BIC2)^2)
  MSEAIC <- mean((test$Goals - lambda.hat.AIC1)^2 + (test$Goals.oppo - lambda.hat.AIC2)^2)
  MSEPois <- mean((test$Goals - pred1)^2 + (test$Goals.oppo - pred2)^2)
  MSEexLLH <- mean((test$Goals - lambda.hat.exLLH1)^2 + (test$Goals.oppo - lambda.hat.exLLH2)^2)
  MSEcop <- mean((test$Goals - lambda.hat.cop1)^2 + (test$Goals.oppo - lambda.hat.cop2)^2)
  MSEequal <- mean((test$Goals - lambda.hat.equal1)^2 + (test$Goals.oppo - lambda.hat.equal2)^2)
  MSEequal.lasso <- mean((test$Goals - lambda.hat.equal.lasso1)^2 + (test$Goals.oppo - lambda.hat.equal.lasso2)^2)
  
  if(Jahr=="2018") {
    wins1 <- test$Goals > test$Goals.oppo
    draw <- test$Goals == test$Goals.oppo
    wins2 <- test$Goals < test$Goals.oppo
    trueresult <- data.frame(wins1 = wins1, draw = draw, wins2 = wins2)
    ## sort bet, because in data games are coinflipped.
    for(i in 1:64){
      if(bet$V1[i] == cop.GamePred$tag2[i]){
        bet[i,1:5] <- bet[i,c(2,1,5,4,3)]
      }
      ## check for errors in names:
      if(!(bet$V1[i] %in% c(cop.GamePred$tag1[i], cop.GamePred$tag2[i])))
        stop("Something went wrong, team name between bets and preds not identical")
    }
    
    win.BIC <- tipico.cor(BIC.GamePred, trueresult = trueresult, bet = bet)$win.ratio
    win.AIC <- tipico.cor(AIC.GamePred, trueresult = trueresult, bet = bet)$win.ratio
    win.exLLH <- tipico.cor(exLLH.GamePred, trueresult = trueresult, bet = bet)$win.ratio
    win.Pois <- tipico.cor(Pois.GamePred, trueresult = trueresult, bet = bet)$win.ratio
    win.cop <- tipico.cor(cop.GamePred, trueresult = trueresult, bet = bet)$win.ratio
    win.equal <- tipico.cor(equal.GamePred, trueresult = trueresult, bet = bet)$win.ratio
    win.equal.lasso <- tipico.cor(equal.lasso.GamePred, trueresult = trueresult, bet = bet)$win.ratio
  }
  ## betting results only when 2018 is left out and predicted. Older betting
  ## data is missing or not reliable enough.
  if(Jahr!="2018") {   
    win.BIC <- win.AIC <- win.exLLH <- win.cop <- NA
    win.Pois <- NA
    win.equal <- win.equal.lasso <- NA
  }
  return(list(data.frame(WM = rep(Jahr, 7), Copula = c(rep(Cop, 4), "indep", rep(Cop, 2)), 
                         Tuner = c("BIC", "AIC", "exLLH", "none", "none", "none", "exLLHequal"),
                         rps = c(rpsBIC, rpsAIC, rpsexLLH, rpscop, rpsPois, rpsequal, rpsequal.lasso),
                         likelihood = c(llhBIC, llhAIC, llhexLLH, llhcop, llhPois, llhequal, llhequal.lasso),
                         classrate = c(crBIC, crAIC, crexLLH, crcop, crPois, crequal, crequal.lasso),
                         betting = c(win.BIC, win.AIC, win.exLLH, win.cop, win.Pois, win.equal, win.equal.lasso),
                         MSEgoals = c(MSEBIC, MSEAIC, MSEexLLH, MSEcop, MSEPois, MSEequal, MSEequal.lasso),
                         nu = c(fitted$nus, NA, NA, NA, fitequal.lasso.exllh.nu[3]),
                         exllh = c(exllh.bic, exllh.aic, exllh.exLLH, exllh.cop, exllh.pois, exllh.equal, exllh.equal.lasso)),
              fitted))
}
