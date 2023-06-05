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

evalWM <- function(fit, test, bets = NULL){
  GamePred <- calc.probs(fit, test, 15)
  res <- rps(GamePred, test)
  rps.r <- mean(res$rps.vec)
  llh.r <- mean(res$likelihood)
  cr.r <- mean(res$result == res$classpred)
  lambda1 <- exp(predict(fit, eq = 1, newdata = test))
  lambda2 <- exp(predict(fit, eq = 2, newdata = test))
  MSE <- 1/64 * sum(sqrt((lambda1 - test$Goals)^2 + (lambda2 - test$Goals.oppo)^2))
  if(!is.null(bets)){
    trueresult <- matrix(0, nrow = 64, ncol = 3)
    for(j in 1:64){
      trueresult[j, res$result[j]] <- 1
    }
    bet <- tipico.cor(GamePred, tau.bet = 1, kelly = FALSE, trueresult, bet = bets)
  }
  else
    bet <- NA
  return(list(GamePred = GamePred, rps = rps.r, llh = llh.r, cr = cr.r,
              MSE = MSE, bet = bet))
}

eval.glm <- function(fit1, fit2, test, bets = NULL){
  require(skellam) # Skellam x = y1 - y2
  lambda1 <- exp(predict(fit1, newdata = test))
  lambda2 <- exp(predict(fit2, newdata = test))
  p.lose <- pskellam(-0.01, lambda1, lambda2)
  p.draw <- dskellam(0, lambda1,)
  p.win <- pskellam(-0.01, lambda2, lambda1)
  GamePred <- data.frame(Team1 = test$Team, win1 = p.win, draw = p.draw, win2 = p.lose, Team2 = test$Opponent)
  res <- rps(GamePred, test)
  rps.r <- mean(res$rps.vec)
  llh.r <- mean(res$likelihood)
  cr.r <- mean(res$result == res$classpred)
  MSE <- 1/64 * sum(sqrt((lambda1 - test$Goals)^2 + (lambda2 - test$Goals.oppo)^2))
  if(!is.null(bets)){
    trueresult <- matrix(0, nrow = 64, ncol = 3)
    for(j in 1:64){
      trueresult[j, res$result[j]] <- 1
    }
    bet <- tipico.cor(GamePred, tau.bet = 1, kelly = FALSE, trueresult, bet = bets)
  }
  else
    bet <- NA
  return(list(GamePred = GamePred, rps = rps.r, llh = llh.r, cr = cr.r,
              MSE = MSE, bet = bet))
}

eval.glm.lasso <- function(fit1, fit2, test1, test2, bets = NULL, test = NULL){
  require(skellam) # Skellam x = y1 - y2
  lambda1 <- exp(predict(fit1, newx = test1))
  lambda2 <- exp(predict(fit2, newx = test2))
  p.lose <- pskellam(-0.01, lambda1, lambda2)
  p.draw <- dskellam(0, lambda1,)
  p.win <- pskellam(-0.01, lambda2, lambda1)
  GamePred <- data.frame(Team1 = test$Team, win1 = p.win, draw = p.draw, win2 = p.lose, Team2 = test$Opponent)
  res <- rps(GamePred, test)
  rps.r <- mean(res$rps.vec)
  llh.r <- mean(res$likelihood)
  cr.r <- mean(res$result == res$classpred)
  MSE <- 1/64 * sum(sqrt((lambda1 - test$Goals)^2 + (lambda2 - test$Goals.oppo)^2))
  if(!is.null(bets)){
    trueresult <- matrix(0, nrow = 64, ncol = 3)
    for(j in 1:64){
      trueresult[j, res$result[j]] <- 1
    }
    bet <- tipico.cor(GamePred, tau.bet = 1, kelly = FALSE, trueresult, bet = bets)
  }
  else
    bet <- NA
  return(list(GamePred = GamePred, rps = rps.r, llh = llh.r, cr = cr.r,
              MSE = MSE, bet = bet))
}


## assumption: matches split and per rbind combined. 128 rows, first 64 are
## home entries, 64 following are "guest". 

eval.glm.equal <- function(fit1, test, bets = NULL, testfull){
  require(skellam) # Skellam x = y1 - y2
  lambda1 <- exp(predict(fit1, newdata = test[1:64,]))
  lambda2 <- exp(predict(fit1, newdata = test[65:128,]))
  p.lose <- pskellam(-0.01, lambda1, lambda2)
  p.draw <- dskellam(0, lambda1,)
  p.win <- pskellam(-0.01, lambda2, lambda1)
  GamePred <- data.frame(Team1 = test$Team[1:64], win1 = p.win, draw = p.draw, win2 = p.lose, Team2 = test$Team[65:128])
  res <- rps(GamePred, testfull)
  rps.r <- mean(res$rps.vec)
  llh.r <- mean(res$likelihood)
  cr.r <- mean(res$result == res$classpred)
  MSE <- 1/64 * sum(sqrt((lambda1 - testfull$Goals)^2 + (lambda2 - testfull$Goals.oppo)^2))
  if(!is.null(bets)){
    trueresult <- matrix(0, nrow = 64, ncol = 3)
    for(j in 1:64){
      trueresult[j, res$result[j]] <- 1
    }
    bet <- tipico.cor(GamePred, tau.bet = 1, kelly = FALSE, trueresult, bet = bets)
  }
  else
    bet <- NA
  return(list(GamePred = GamePred, rps = rps.r, llh = llh.r, cr = cr.r,
              MSE = MSE, bet = bet))
}

## creates pathplot

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
    points(1:grid.l, rev(m[i,]), type = "l")
  }
  abline(v = which(rev(fit$grid) == fit$nus[1]), lty = 2)
  #text(x = which(rev(fit$grid) == fit$nus[1]) - grid.l*0.03, y = max(m)*0.9, labels = "BIC")
  abline(v = which(rev(fit$grid) == fit$nus[2]), lty = 2)
  #text(x = which(rev(fit$grid) == fit$nus[2]) - grid.l*0.03, y = max(m)*0.9, labels = "AIC")
  abline(v = which(rev(fit$grid) == fit$nus[3]), lty = 2)
  #text(x = which(rev(fit$grid) == fit$nus[3]) + grid.l*0.03, y = max(m)*0.9, labels = "exLLH")
}

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