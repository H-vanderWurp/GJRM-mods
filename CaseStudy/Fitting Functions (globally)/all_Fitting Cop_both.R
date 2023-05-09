## Fitting Cop

exe.cop.all.both <- function(Cop){
  require(EUfootball)
  require(GJRM)
  source("../GJRM changes - 2023.R")
  source("application functions.R")
  
  
  Dat <- Matches
  Dat <- Dat[Dat$date < as.Date("2020-03-01"),]
  
  ## sort by date (formerly sorted by league)
  
  Dat <- Dat[order(Dat$date),]
  
  train <- Dat[Dat$SeasonFrom <= 2014,]
  test <- Dat[Dat$SeasonFrom > 2014,]
  
  n <- dim(test)[1]
  
  
  Res <- data.frame(rps = rep(NA, n), mllh = rep(NA, n),
                    cr = rep(NA, n), SE = rep(NA, n), AE = rep(NA, n),
                    SeasonFrom = rep(NA, n), matchday = rep(NA, n))
  n.bets <- 0
  gains <- numeric(0)
  
  repeat{
    # indep equations
    print(dim(test))
    eq1 <- Goals90Home ~ eloHome + eloGuest + 
      MVHome.T + MVGuest.T + 
      pHome + pGuest +
      FormGoals3Home + FormGoals3Guest +
      PromotedHome + PromotedGuest +
      TitleholderHome + TitleholderGuest + CupTitleholderHome + CupTitleholderGuest
    eq2 <- Goals90Guest ~ eloGuest + eloHome + 
      MVGuest.T + MVHome.T + 
      pGuest + pHome +
      FormGoals3Guest + FormGoals3Home +
      PromotedGuest + PromotedHome +
      TitleholderGuest + TitleholderHome + CupTitleholderGuest + CupTitleholderHome
    eq3 <- ~ 1
    
    ## fit:
    fitcop <- gjrm(formula = list(eq1, eq2, eq3), data = train,
                   BivD = Cop, margins = c("PO", "PO"), Model = "B")
    
    ## predict (date instead of matchday)
    match.date <- test$date[1]
    year <- test$SeasonFrom[1]
    ind <- test$date == match.date
    testhere <- test[ind,]
    
    pred <- evaluate1(type = "cop", fit1 = fitcop, newdata = testhere)
    res <- evaluate2(pred, newdata = testhere)
    
    bet <- tipico(pred[[1]], testhere)
    
    n.here <- length(res$rps)
    index1 <- n-(dim(test)[1])+1  # Index on where to write the results
    index2 <- index1+n.here-1
    Res$rps[index1:index2] <- res$rps
    Res$mllh[index1:index2] <- res$mllh
    Res$cr[index1:index2] <- res$cr
    Res$SE[index1:index2] <- res$SE
    Res$AE[index1:index2] <- res$AE
    Res$SeasonFrom[index1:index2] <- year
    Res$matchday[index1:index2] <- testhere$matchday
    Res$League[index1:index2] <- testhere$League
    
    n.bets <- n.bets + bet$n.bets
    gains <- c(gains, bet$gains)
    
    ## current matchday to training data
    train <- rbind(train, testhere)
    test <- test[!ind,]
    
    if(dim(test)[1] == 0)
      break
  }
  Res <- list(Res, n.bets, gains)
  save(file = paste0("Results (globally)/Cop_", Cop, "_", "both", ".rData"), Res)
}

