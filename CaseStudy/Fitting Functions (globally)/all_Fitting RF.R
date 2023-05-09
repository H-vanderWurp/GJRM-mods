## Fitting of independent random forest:

library(party)



exe.rf <- function(date.ind){
  require(EUfootball)
  Dat <- Matches
  source("application functions.R")
  library(skellam)
  
  Dat <- Matches
  Dat <- Dat[Dat$date < as.Date("2020-03-01"),]
  
  ## sort by date (formerly sorted by league)
  
  Dat <- Dat[order(Dat$date),]
  
  ind <- unique(Dat$date)[date.ind]
  train <- Dat[Dat$date < ind,]
  test <- Dat[Dat$date == ind,]
  
  Dat <- Matches
  Dat <- Dat[Dat$date < as.Date("2020-03-01"),]
  
  train <- Dat[Dat$SeasonFrom <= 2014,]
  train <- train[!(is.na(train$Goals90Home)) & !(is.na(train$Goals90Guest)),]
  test <- Dat[Dat$SeasonFrom > 2014,]
  
  n <- dim(test)[1]
  
  Res <- data.frame(rps = rep(NA, n), mllh = rep(NA, n),
                    cr = rep(NA, n), SE = rep(NA, n), AE = rep(NA, n),
                    SeasonFrom = rep(NA, n), matchday = rep(NA, n))
  n.bets <- 0
  gains <- numeric(0)
  #repeat{
    # indep equations
    print(dim(test))
    eq1 <- Goals90Home ~ eloHome + MVHome.T + pHome + FormGoals3Home +
      PromotedHome + TitleholderHome + CupTitleholderHome
    eq2 <- Goals90Guest ~ eloGuest + MVGuest.T + pGuest + FormGoals3Guest +
      PromotedGuest + TitleholderGuest + CupTitleholderGuest
    ## fit:
    fit1 <- cforest(eq1, data = train)
    fit2 <- cforest(eq2, data = train)
    
    
    ## predict:
    ind <- test$date[1]
    year <- test$SeasonFrom[1]
    testhere <- test[test$date == ind,]
    
    pred <- evaluate1(type = "pois", fit1, fit2, newdata = testhere)
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
    
    n.bets <- n.bets + bet$n.bets
    gains <- c(gains, bet$gains)
    
    ## current matchday to training data
 #   train <- rbind(train, testhere)
  #  test <- test[!ind,]
    #rm(fitpois1, fitpois2, pred.pois, res.pois, testhere, ind, index1, index2,
    #   year, md)
   # if(dim(test)[1] == 0)
   #   break
  #}
  Res <- list(Res, n.bets, gains)
  save(file = paste0("Results (globally)/RF_Res_", date.ind, ".rData"), Res)
}

