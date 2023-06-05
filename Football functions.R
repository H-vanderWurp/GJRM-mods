game <- function(lambda1, lambda2) {
  
  prob.draw <- dskellam(0, lambda1, lambda2)
  prob.win1 <- 1- pskellam(0, lambda1, lambda2)
  prob.win2 <- pskellam(-1, lambda1, lambda2)
  
  return(data.frame(win1=prob.win1, draw=prob.draw, win2=prob.win2))
}

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

gameCopula <- function(lambda1, lambda2, Copula, CopParam, method="r") {
  require(copula)
  require(VineCopula)
  if(Copula=="N")
    myCop <- normalCopula(CopParam)
  if(Copula=="C0")
    myCop <- claytonCopula(CopParam)
  if(Copula=="C90")
    myCop <- r90ClaytonCopula(CopParam)
  if(Copula=="C180")
    myCop <- surClaytonCopula(CopParam)
  if(Copula=="C270")
    myCop <- r270ClaytonCopula(CopParam)
  if(Copula=="J0")
    myCop <- joeCopula(CopParam)
  if(Copula=="J90")
    myCop <- r90JoeBiCopula(CopParam)
  if(Copula=="J180")
    myCop <- surJoeBiCopula(CopParam)
  if(Copula=="J270")
    myCop <- r270JoeBiCopula(CopParam)
  if(Copula=="G0")
    myCop <- gumbelCopula(CopParam)
  if(Copula=="G90")
    myCop <- r90GumbelCopula(CopParam)
  if(Copula=="G180")
    myCop <- surGumbelCopula(CopParam)
  if(Copula=="G270")
    myCop <- r270GumbelCopula(CopParam)
  if(Copula=="F")
    myCop <- frankCopula(CopParam)
  if(Copula=="AMH")
    myCop <- amhCopula(CopParam)
  if(Copula=="FGM")
    myCop <- fgmCopula(CopParam)
  if(Copula=="T")
    myCop <- tCopula(CopParam)
  if(Copula=="PL")
    myCop <- plackettCopula(CopParam)
  if(Copula=="HO")
    myCop <- khoudrajiCopula(CopParam) ## unsicher, ob richtige Copula Khoudraji-Gumbel-Hougaard-Copula
  if(!exists("myCop"))
    return(NA)
  
  myCop <- mvdc(myCop, margins = c("pois", "pois"), 
                paramMargins = list(lambda=lambda1,lambda=lambda2))
  
  if(method=="r") {
    Sim <- rMvdc(10000, myCop)
    dif <- Sim[,1] - Sim[,2]
    prob.draw <- length(dif[dif==0])/10000
    prob.win1 <- length(dif[dif>0])/10000
    prob.win2 <- length(dif[dif<0])/10000
    #covs <- cov(Sim[,1], Sim[,2])
  }
  
  if(method=="p") {
    ## all pMvdc values
    pmat <- matrix(NA, nrow = 21, ncol= 21)
    for (i in 1:21) {
      for (j in 1:21) {
        pmat[i,j] <- pMvdc(c(i-1,j-1), myCop)
      }
    }
    ## now probabilities:
    m <- matrix(NA, nrow=21, ncol= 21)
    m[1,1] <- pmat[1,1]
    ## margins of matrix
    for(j in 2:21) { 
      m[1,j] <- pmat[1,j] - pmat[1,j-1]
      m[j,1] <- pmat[j,1] - pmat[j-1,1]
    }
    for(i in 2:21) {
      for(j in 2:21) {
        m[i,j] <- pmat[i,j] - pmat[i-1,j] - pmat[i,j-1] + pmat[i-1,j-1]
      }
    }
    prob.win1 <- sum(m * lower.tri(m))
    prob.draw <- sum(diag(m))
    prob.win2 <- sum(m * upper.tri(m))
    
    ## calc. discrete expected value
#    values <- expand.grid(0:20, 0:20)
#    ## Objekt values geht die Ergebnises aus m Spaltenweise durch
#    summanden <- matrix(NA, nrow=441, ncol=2)
#    for (i in 1:441) {
#      summanden[i,] <- as.numeric(values[i,] * m[i])
#    }
  }
  return(data.frame(win1=prob.win1, draw=prob.draw, win2=prob.win2))  #, covs=covs))
}

calc.probs <- function(fit, d.test, N){
  zw <- sapply(0:N, function(n) jc.probs(fit, n, n, newdata=d.test)$p12)
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


#dat2018 <- dat[dat$WM=="2018",]

tipico.vor <- function(fit, dat2018) {
  
  lambda1 <- exp(predict(fit, newdata=dat2018, eq=1))
  lambda2 <- exp(predict(fit, newdata=dat2018, eq=2))
  
  n.pred <- dim(dat2018)[1]
  GamePred <- c()
  for (j in 1:n.pred) {
    GamePred <- rbind(GamePred, gameCopula(lambda1[j], lambda2[j], fit$BivD, 
                                           summary(fit)$theta))
  }
  ## calculate quality of those predictions
  GamePred <- data.frame(tag1 = dat2018$Team, win1 = GamePred$win1, 
                         draw = GamePred$draw, 
                         win2 = GamePred$win2, tag2=dat2018$Opponent)
  return(GamePred)
}

tipico <- function(GamePred, tau.bet = 1, kelly = FALSE, trueresult){
  
  expected <- GamePred[,2:4] * bet[,3:5]
  bets.to.take <- (expected >= tau.bet) * bet[,3:5] 
  ## only one per game
  for (i in 1:64) {
    bets.to.take[i,-which.max(bets.to.take[i,])] <- 0
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

##hv: 12.10.2020: Fehler gefunden. Wenn mehrere Outcomes einen positiven Erwartungswert haben,
##hv: wurde immer die Wette (mit positiven Erwartungswert > tau) gewaehlt, die die hoechste Quote hat.
##hv: Das ist Varianzmaximierend und daher keine sinnvolle Strategie. Gemeint war, die Wette mit hoechstem
##hv: Erwartungswert zu spielen.

tipico.cor <- function(GamePred, tau.bet = 1, kelly = FALSE, trueresult, bet = bet){
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
