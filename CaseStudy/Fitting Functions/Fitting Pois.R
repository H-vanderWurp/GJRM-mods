## Fitting of independent Poisson:

exe.pois <- function(Liga){
  require(EUfootball)
  Dat <- Matches
  source("application functions.R")
  library(skellam)

  Dat <- Dat[Dat$date < as.Date("2020-03-01"),]
  Dat.sub <- Dat[Dat$League == Liga,]
  
  train <- Dat.sub[Dat.sub$SeasonFrom <= 2014,]
  test <- Dat.sub[Dat.sub$SeasonFrom > 2014,]
  
  n <- dim(test)[1]
  
  Res <- data.frame(rps = rep(NA, n), mllh = rep(NA, n),
                    cr = rep(NA, n), SE = rep(NA, n), AE = rep(NA, n),
                    SeasonFrom = rep(NA, n), matchday = rep(NA, n))
  n.bets <- 0
  gains <- numeric(0)
  repeat{
    # indep equations
    print(dim(test))
    eq1 <- Goals90Home ~ eloHome + MVHome.T + pHome + FormGoals3Home +
      PromotedHome + TitleholderHome + CupTitleholderHome
    eq2 <- Goals90Guest ~ eloGuest + MVGuest.T + pGuest + FormGoals3Guest +
      PromotedGuest + TitleholderGuest + CupTitleholderGuest
    
    ## fit:
    fitpois1 <- glm(eq1, data = train, family = "poisson")
    fitpois2 <- glm(eq2, data = train, family = "poisson")
    
    ## predict:
    md <- test$matchday[1]
    year <- test$SeasonFrom[1]
    ind <- test$matchday == md & test$SeasonFrom == year
    testhere <- test[ind,]
    
    pred.pois <- evaluate1(type = "pois", fitpois1, fitpois2, newdata = testhere)
    res.pois <- evaluate2(pred.pois, newdata = testhere)
    
    bet <- tipico(pred.pois[[1]], testhere)
    
    n.here <- length(res.pois$rps)
    index1 <- n-(dim(test)[1])+1  # Index on where to write the results
    index2 <- index1+n.here-1
    Res$rps[index1:index2] <- res.pois$rps
    Res$mllh[index1:index2] <- res.pois$mllh
    Res$cr[index1:index2] <- res.pois$cr
    Res$SE[index1:index2] <- res.pois$SE
    Res$AE[index1:index2] <- res.pois$AE
    Res$SeasonFrom[index1:index2] <- year
    Res$matchday[index1:index2] <- md
    
    n.bets <- n.bets + bet$n.bets
    gains <- c(gains, bet$gains)
    
    ## current matchday to training data
    train <- rbind(train, testhere)
    test <- test[!ind,]
    #rm(fitpois1, fitpois2, pred.pois, res.pois, testhere, ind, index1, index2,
    #   year, md)
    if(dim(test)[1] == 0)
      break
  }
  Res <- list(Res, n.bets, gains)
  save(file = paste0("Results/pois/Res_", Liga, ".rData"), Res)
}

