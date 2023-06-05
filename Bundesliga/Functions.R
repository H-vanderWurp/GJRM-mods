getpred <- function(fit, newdata){
  
  ## Vorbereitungen:
  sMW <- sum(c(newdata$MarktwertHome, newdata$MarktwertGuest))
  newdata$MWHome <- newdata$MarktwertHome / sMW
  newdata$MWGuest <- newdata$MarktwertGuest / sMW
  newdata$AufsteigerHome <- as.logical(newdata$AufsteigerHome)
  newdata$AufsteigerGuest <- as.logical(newdata$AufsteigerGuest)
  newdata$OddsDraw <- newdata$OddsDraw / 100
  newdata$OddsGuest <- newdata$OddsGuest / 100
  newdata$OddsHome <- newdata$OddsHome / 100
  p <- 1/ newdata$OddsHome + 1 / newdata$OddsDraw + 1 / newdata$OddsGuest
  newdata$pHome <- 1 / (p * newdata$OddsHome)
  newdata$pGuest <- 1  / (p * newdata$OddsGuest)
  newdata$pDraw <- 1 / (p * newdata$OddsDraw)
  newdata$MWHomeCorona <- abs(newdata$MWHome * newdata$coronaHome)
  newdata$MWGuestCorona <- abs(newdata$MWGuest * newdata$coronaGuest)
  
  source("Football Functions.R")
  names(newdata)[c(7,8)] <- c("Team", "Opponent")
  a <- calc.probs(fit, newdata, 10)
  a <- cbind(a, a[,2] + a[,3])
  a <- a[,c(1, 6, 2, 3, 4, 5)]
  a <- cbind(a, a[,4] + a[,5])
  a <- a[,c(1:5, 7, 6)]
  a <- cbind(a, a[,3] + a[, 5])
  a <- a[,c(1, 8, 2:7)]
  dimnames(a)[[2]] <- c("Gastgeber","12", "1X", "1", "X", "2", "X2", "Gast")
  EWerte <- newdata[,c("tip_12", "tip_1X", "tipWin", "tipDraw", "topLose", "tip_X2")] * a[,2:7]
  ## Spalten loeschen, falls Zeitpunkt vor matchday 20, weil vorher ohne
  ## 12, 1X, X2
  if(newdata$Matchday[1] < 20 & newdata$SeasonFrom[1] == 2019){
    EWerte <- newdata[,c("tipWin", "tipDraw", "topLose")] * a[,4:6]
  }
  return(list(probs=a,expected=cbind(newdata$Team,EWerte, newdata$Opponent)))
}



## Functions

## Saison 2019/2020 auswerten im Wetterfolg
## eqlist, Auswahl aus Saisonstart oder nicht, siehe Hauptdatei.
## dat, voller Datensatz, siehe Hauptdatei.

fun19 <- function(matchday, eqlist, data, true.count = 8, iterative = TRUE){
  dat <- data
  ## Daten zum Modellierungen auswaehlen:
  train <- dat[dat$SeasonFrom < 2019,]
  train.add <- dat[dat$SeasonFrom == 2019 & dat$Matchday < matchday,]
  train <- rbind(train, train.add)
  
  test <- dat[dat$SeasonFrom == 2019 & dat$Matchday == matchday,]
  
  ## fit:
  fit <- gjrm(eqlist, data=train, BivD = "F", Model = "B", margins = c("PO","PO"),
              linear.equal = c(FALSE, rep(TRUE, true.count)), iterative = iterative)
  ## Uebergangsphase Beginn Pandemie, 4 matchdays Learning
  if(matchday %in% 26:29){
    fit$fit$argument[c(1,14)] <- mean(fit$fit$argument[c(1,14)])
    fit$coefficients[c(1,14)] <- mean(fit$coefficients[c(1,14)])
  }
  
  pred <- getpred(fit, test)
  pred$probs[,2:7] <- round(pred$probs[,2:7]*100, digits = 2)
  rownames(pred$probs) <- NULL
  #if(matchday == 1){
    #browser()
    #pdf(file = paste0("Probs1920_", matchday, ".pdf"), height = 4, width = 6)
    #grid.table(pred$probs, rows = NULL)
    #ceiling(1/(pred$probs[,2:7]/100)*100)/100
    #dev.off()
  #}
  
  ## Wetten und Wettergebnisse: ## erster 19 Spieltage, nur 1,X,2
  if(matchday <= 19){
    bets <- test[,c("tipWin", "tipDraw", "topLose")]
    bets.to.take <- pred$expected[,2:4] > 1
    bets[!bets.to.take] <- 0
    
    ## if multiple bets>1 per game: variance minimizing option:
    for(j in 1:(nrow(bets))){
      bets.here <- bets[j,]
      if(sum(bets.here>0)>1){
        probs.here <- pred$probs[j,2:4]
        probs.here[bets.here == 0] <- 0
        bets.here[-which.max(probs.here)] <- 0
        bets[j,] <- bets.here
      }
    }
  }
  else{
    bets <- test[,c("tip_12", "tip_1X", "tipWin", "tipDraw", "topLose", "tip_X2")]
    bets.to.take <- pred$expected[,2:7] > 1
    bets[!bets.to.take] <- 0
    for(j in 1:(nrow(bets))){
      bets.here <- bets[j,]
      if(sum(bets.here>0)>1){
        probs.here <- pred$probs[j,2:7]
        probs.here[bets.here == 0] <- 0
        bets.here[-which.max(probs.here)] <- 0
        bets[j,] <- bets.here
      }
    }
  } 
  ## check postponed, no bets there:
  bets[test$postponed == 1,] <- 0
  
  ## now place bets:
  bets.placed <- sum(bets>0)
  
  types <- data.frame(r12 = 0, r1X = 0, r1 = 0, rX = 0,
                      rX2 = 0)
  
  if(matchday <= 19){
    trueresult <- matrix(0, nrow=nrow(test), ncol=3)
    trueresult[,1] <- as.numeric(test$Score90Home > test$Score90Guest)
    trueresult[,2] <- as.numeric(test$Score90Home == test$Score90Guest)
    trueresult[,3] <- as.numeric(test$Score90Home < test$Score90Guest)
    types$r1 <- colSums(bets > 0)[1]
    types$rX <- colSums(bets > 0)[2]
    types$r2 <- colSums(bets > 0)[3]
  }
  else{
    trueresult <- matrix(0, nrow=nrow(test), ncol = 6)
    trueresult[,1] <- as.numeric(test$Score90Home != test$Score90Guest) # 12
    trueresult[,2] <- as.numeric(test$Score90Home >= test$Score90Guest) # 1X
    trueresult[,3] <- as.numeric(test$Score90Home > test$Score90Guest) # 1
    trueresult[,4] <- as.numeric(test$Score90Home == test$Score90Guest) # X
    trueresult[,5] <- as.numeric(test$Score90Home < test$Score90Guest) # 2
    trueresult[,6] <- as.numeric(test$Score90Home <= test$Score90Guest) # X2
    types$r12 <- colSums(bets > 0)[1]
    types$r1X <- colSums(bets > 0)[2]
    types$r1 <- colSums(bets > 0)[3]
    types$rX <- colSums(bets > 0)[4]
    types$r2 <- colSums(bets > 0)[5]
    types$rX2 <- colSums(bets > 0)[6]
  }
 
  ## Kelly: 
  ## total stake for matchday: number of bets I would place.
  wealth <- bets.placed
  ## fractions f for each bet
  if(matchday <= 19)
    pr <- pred$expected[,2:4]
  else
    pr <- pred$expected[,2:7]
  
  f <- pr / (bets-1)
  f[f<0] <- 0
  stakes <- f/sum(f) * wealth
  

  
  stopifnot(all(dim(bets) == dim(trueresult)))
  
  gains <- sum(bets * trueresult) - bets.placed
  gains.kelly <- sum(bets * stakes * trueresult) - bets.placed
  return(list(gains = gains, gains.kelly = gains.kelly, bets.placed = bets.placed, 
              bets.types = types))
}


## to do weiter: Saison 20/21

fun20 <- function(matchday, eqlist, data, true.count = 8, iterative = TRUE){
  print(matchday)
  ## Daten zum Modellierungen auswaehlen:
  train <- dat[dat$SeasonFrom < 2020,]
  train.add <- dat[dat$SeasonFrom == 2020 & dat$Matchday < matchday,]
  train <- rbind(train, train.add)
  
  test <- dat[dat$SeasonFrom == 2020 & dat$Matchday == matchday,]
  
  ## fit:
  fit <- gjrm(eqlist, data=train, BivD = "F", Model = "B", margins = c("PO","PO"),
              linear.equal = c(FALSE, rep(TRUE, true.count)), iterative = iterative)
  
  pred <- getpred(fit, test)
  pred$probs[,2:7] <- round(pred$probs[,2:7]*100, digits = 2)
  rownames(pred$probs) <- NULL
  #pdf(file = paste0("Probs2021_", matchday, ".pdf"), height = 4, width = 6)
  #grid.table(pred$probs, rows = NULL)
  #dev.off()
  
  bets <- test[,c("tip_12", "tip_1X", "tipWin", "tipDraw", "topLose", "tip_X2")]
  bets.to.take <- pred$expected[,2:7] > 1
  bets[!bets.to.take] <- 0
  for(j in 1:(nrow(bets))){
    bets.here <- bets[j,]
    if(is.na(sum(bets.here>0)>1)) next
    if(sum(bets.here>0)>1){
      probs.here <- pred$probs[j,2:7]
      probs.here[bets.here == 0] <- 0
      bets.here[-which.max(probs.here)] <- 0
      bets[j,] <- bets.here
    }
  }
  ## check postponed, no bets there:
  bets[test$postponed == 1,] <- 0
  
  ## now place bets:
  bets.placed <- sum(bets>0, na.rm = TRUE)
  
  types <- data.frame(r12 = 0, r1X = 0, r1 = 0, rX = 0,
                      rX2 = 0)
  
  trueresult <- matrix(0, nrow=nrow(test), ncol = 6)
  trueresult[,1] <- as.numeric(test$Score90Home != test$Score90Guest) # 12
  trueresult[,2] <- as.numeric(test$Score90Home >= test$Score90Guest) # 1X
  trueresult[,3] <- as.numeric(test$Score90Home > test$Score90Guest) # 1
  trueresult[,4] <- as.numeric(test$Score90Home == test$Score90Guest) # X
  trueresult[,5] <- as.numeric(test$Score90Home < test$Score90Guest) # 2
  trueresult[,6] <- as.numeric(test$Score90Home <= test$Score90Guest) # X2
  types$r12 <- colSums(bets > 0, na.rm = TRUE)[1]
  types$r1X <- colSums(bets > 0, na.rm = TRUE)[2]
  types$r1 <- colSums(bets > 0, na.rm = TRUE)[3]
  types$rX <- colSums(bets > 0, na.rm = TRUE)[4]
  types$r2 <- colSums(bets > 0, na.rm = TRUE)[5]
  types$rX2 <- colSums(bets > 0, na.rm = TRUE)[6]
  
  wealth <- bets.placed
  ## fractions f for each bet
  pr <- pred$expected[,2:7]
  
  f <- pr / (bets-1)
  f[f<0] <- 0
  stakes <- f/sum(f) * wealth
  
  
  stopifnot(all(dim(bets) == dim(trueresult)))
  gains <- sum(bets * trueresult, na.rm = TRUE) - bets.placed
  gains.kelly <- sum(bets * stakes * trueresult, na.rm = TRUE) - bets.placed
  return(list(gains = gains, gains.kelly = gains.kelly, bets.placed = bets.placed, 
              bets.types = types))
}

