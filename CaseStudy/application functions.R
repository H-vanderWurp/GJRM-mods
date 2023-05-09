
## rps, bekommt x = GamePred und Testdaten.

rps <- function(x, d.test){
  rps.vec <- c()
  result <- rep(1, dim(d.test)[1])
  result[which(d.test$Goals90Home==d.test$Goals90Guest)] <- 2
  result[which(d.test$Goals90Home < d.test$Goals90Guest)] <- 3
  result.vec <- matrix(rep(0,3*dim(d.test)[1]), ncol=3)
  for (i in 1:dim(d.test)[1]) {
    result.vec[i,result[i]] <- 1
  }
  for (i in 1:dim(d.test)[1]) {
    rps.i <- 0.5*sum((cumsum(as.numeric(x[i,2:3])) - cumsum(as.numeric(result.vec[i,-3])))^2)
    rps.vec <- c(rps.vec,rps.i)
  }
  likelihood <- result.vec*x[,2:4]
  #welche <- which(likelihood>0)
  #probs <- c(c(likelihood)$p.home, c(likelihood)$p.draw, c(likelihood)$p.guest)
  #likelihood <- probs[welche]
  likelihood <- apply(likelihood, 1, max)
  
  ## classification rate
  ind <- which(!is.na(rps.vec))
  classpred <- rep(NA, length(rps.vec))
  classpred[ind] <- unlist(apply(x[,2:4], 1, which.max))
  list(rps.vec=rps.vec, result=result, likelihood=likelihood, classpred=classpred)
}



calc.probs <- function(fit, d.test, N){
  # check for NAs, because jc.probs cant work with them.
  ind <- which(!apply(d.test, 1, function(x) any(is.na(x))))
  if(length(ind) == 0)
    return(res = data.frame(Home = d.test$Home, p.home = NA, p.draw = NA, p.guest = NA, Guest = d.test$Guest))
  zw <- sapply(0:N, function(n) jc.probs(fit, n, n, newdata=d.test[ind,])$p12)
  if(is.array(zw))
    prob.draw <- rowSums(zw)
  else
    prob.draw <- sum(zw)
  ## calculate wins
  zw <- c()
  zw2 <- c()
  for (i in 1:N){
    for (j in 0:(i-1)) {
      zw <- cbind(zw, jc.probs(fit, i, j, newdata=d.test[ind,])$p12)
      zw2 <- cbind(zw2, jc.probs(fit, j, i, newdata=d.test[ind,])$p12)
    }
  }
  prob.win <- rowSums(zw)
  prob.win2 <- rowSums(zw2)
  summ <- prob.win + prob.draw + prob.win2
  prob.draw <- prob.draw / summ
  prob.win <- prob.win / summ
  prob.win2 <- prob.win2 / summ
  
  res <- data.frame(Home = d.test$Home,
                    p.home = NA, p.draw = NA, p.guest = NA,
                    Guest = d.test$Guest)
  res$p.home[ind] <- prob.win
  res$p.draw[ind] <- prob.draw
  res$p.guest[ind] <- prob.win2
  return(res)
}


## types: "cop", "pois", "glmnet"
## evalate: calculate GamePred for probabilities and errors on goals.
## eq1 und eq2 nur noetig fuer xgboost
## Home, Guest, ind, newdata2, Y1, Y2 are for glmnet setup.

evaluate1 <- function(type = "cop", fit1 = NULL, fit2 = NULL, newdata, newdata2 = NULL, 
                      xgboost = FALSE, eq1 = NULL, eq2 = NULL, Home = NULL, Guest = NULL, ind = NULL,
                      Y1 = NULL, Y2 = NULL) {
  if(type == "pois"){
    n <- dim(newdata)[1]
    if(xgboost){
      ## Problem: incomplete cases wurden entfernt. Welche waren das?
      ind1 <- !is.na(newdata$pHome) & !is.na(newdata$MVHome.T) & 
        !is.na(newdata$MVGuest.T) & !is.na(newdata$pGuest) & !is.na(newdata$Goals45Home)
      ind2 <- !is.na(newdata$pGuest) & !is.na(newdata$MVGuest.T) & 
        !is.na(newdata$pHome) & !is.na(newdata$MVHome.T) & !is.na(newdata$Goals45Guest)
      newdata.h <- newdata[ind1 & ind2,]
      newdata1 <- model.matrix(eq1, newdata.h)
      newdata2 <- model.matrix(eq2, newdata.h)
      pred1 <- pred2 <- rep(NA, n)
      if(any(c(ind1, ind2))){
        pred1[ind1 & ind2] <- predict(fit1, newdata = newdata1, type = "response")
        pred2[ind2] <- predict(fit2, newdata = newdata2, type = "response")
        ## naechstes Problem: xgboost predicted negative lambdas.
        pred1[pred1<0.001] <- 0.001
        pred2[pred2<0.001] <- 0.001
      }
      else{
        pred1 <- rep(NA, length(ind1))
        pred2 <- rep(NA, length(ind2))
      }
    }
    else{
      pred1 <- predict(fit1, newdata = newdata, type = "response")
      pred2 <- predict(fit2, newdata = newdata, type = "response")
    }

    require(skellam)
    ##  dskellam fails with multiple NAs.
    ind <- which(!is.na(pred1) & !is.na(pred2))
    p.draw <- dskellam(0, as.numeric(pred1[ind]), as.numeric(pred2[ind]))
    p.guest <- pskellam(-0.001, pred1[ind], pred2[ind])
    p.home <- 1 - p.draw - p.guest
    p.drawT <- p.homeT <- p.guestT <- rep(NA, n)
    p.drawT[ind] <- p.draw
    p.homeT[ind] <- p.home
    p.guestT[ind] <- p.guest
    GamePred <- data.frame(Home = newdata$Home, p.home = p.homeT, 
                           p.draw = p.drawT, p.guest = p.guestT, 
                           Guest = newdata$Guest)
    error1 <- pred1 - newdata$Goals90Home
    error2 <- pred2 - newdata$Goals90Guest
  }
  
  if(type == "glmnet"){
    n <- length(ind)
    pred1 <- predict(fit1, newx = newdata, type = "response")
    pred2 <- predict(fit2, newx = newdata2, type = "response")
    
    p.draw <- dskellam(0, as.numeric(pred1), as.numeric(pred2))
    p.guest <- pskellam(-0.001, pred1, pred2)
    p.home <- 1 - p.draw - p.guest
    p.drawT <- p.homeT <- p.guestT <- rep(NA, n)
    p.drawT[ind] <- p.draw
    p.homeT[ind] <- p.home
    p.guestT[ind] <- p.guest
    GamePred <- data.frame(Home = Home, p.home = p.homeT, 
                           p.draw = p.drawT, p.guest = p.guestT, 
                           Guest = Guest)
    error1 <- error2 <- rep(NA, n)
    error1[ind] <- pred1 - Y1
    error2[ind] <- pred2 - Y2
  }
  
  if(type == "cop"){
    GamePred <- calc.probs(fit1, newdata, 10)
    error1 <- exp(predict(fit1, newdata = newdata, eq=1)) - newdata$Goals90Home
    error2 <- exp(predict(fit1, newdata = newdata, eq=2)) - newdata$Goals90Guest
  }
  
  return(list(GamePred, error1, error2))
}

## then calculate measures like rps, mllh, cr, squared errors.
## UPDATE 14.07.2022: Include Brier Score in here.
## argument is output from evaluate1()

evaluate2 <- function(pred, newdata, ordinal = FALSE){
  # rps
  if(!ordinal)
    res <- rps(pred[[1]], d.test = newdata)
  else
    res <- rps(pred, d.test = newdata)
  rps <- res$rps.vec
  mllh <- res$likelihood
  cr <- res$result == res$classpred
  if(!ordinal){
    brier <- (pred[[1]][,2] - (res$result == 1))^2 + 
      (pred[[1]][,3] - (res$result == 2))^2 + 
      (pred[[1]][,4] - (res$result == 3))^2
  }
  if(ordinal){
    brier <- (pred[,2] - (res$result == 1))^2 + 
      (pred[,3] - (res$result == 2))^2 + 
      (pred[,4] - (res$result == 3))^2
    return(list(rps = rps, mllh = mllh, cr = cr, brier = brier))
  }

  SE <- pred[[2]]^2 + pred[[3]]^2
  AE <- abs(pred[[2]]) + abs(pred[[3]])
  return(list(rps = rps, mllh = mllh, cr = cr, SE = SE, AE = AE, brier = brier))
}

## pred is gamepred data.frame with home, 1, X, 2, Guest

tipico <- function(pred, newdata){
  odds <- data.frame("1" = newdata$oddsHome, "X" = newdata$oddsDraw,
                     "2" = newdata$oddsGuest)
  expected <- pred[,2:4] * odds - 1
  expected[expected <= 0] <- 0
  expected[is.na(expected)] <- 0
  n <- dim(newdata)[1]
  for(i in 1:n){  ## Fall: Zwei Wetten positiven Erwartungswert.
    x <- expected[i,]
    if(sum(x > 0) >= 2){ # Loesung: Einfach hoechsten E-Wert.
      x[-which.max(x)] <- 0
      expected[i,] <- x
    }
  }
  n.bets <- sum(expected>0)
  outcome <- numeric(n)
  outcome[newdata$Goals90Home > newdata$Goals90Guest] <- 1
  outcome[newdata$Goals90Home == newdata$Goals90Guest] <- 2
  outcome[newdata$Goals90Home < newdata$Goals90Guest] <- 3
  outcome[is.na(newdata$Goals90Home)] <- NA
  outcome.m <- matrix(nrow=n, ncol = 3)
  for(i in 1:n){
    outcome.m[i, outcome[i]] <- 1
  }
  rm(outcome)
  expected[expected > 0] <- 1 # bets to take
  gains <- sum((odds * outcome.m * expected), na.rm = TRUE) - n.bets
  return(data.frame(n.bets = n.bets, gains = gains))
}
