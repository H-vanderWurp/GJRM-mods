## Fitting of ordinal regression:

## deletet p.draw because collinearity.

library(MASS)

exe.ordinal <- function(Liga){
  require(EUfootball)
  Dat <- Matches
  source("application functions.R")

  Dat <- Dat[Dat$date < as.Date("2020-03-01"),]
  Dat$outcome <- NA
  Dat$outcome[Dat$Goals90Home > Dat$Goals90Guest] <- "1"
  Dat$outcome[Dat$Goals90Home == Dat$Goals90Guest] <- "X"
  Dat$outcome[Dat$Goals90Home < Dat$Goals90Guest] <- "2"
  Dat$outcome <- factor(Dat$outcome, levels = c("1", "X", "2"), ordered = TRUE)
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
    eq1 <- outcome ~ eloHome + eloGuest + MVHome.T + MVGuest.T + pHome + pGuest +
      FormGoals3Home + FormGoals3Guest + PromotedHome + PromotedGuest +
      TitleholderHome + TitleholderGuest + CupTitleholderHome + CupTitleholderGuest

    ## fit:
    fit <- polr(eq1, data = train, Hess = TRUE)
    
    ## predict:
    md <- test$matchday[1]
    year <- test$SeasonFrom[1]
    ind <- test$matchday == md & test$SeasonFrom == year
    testhere <- test[ind,]
    
    pred <- predict(fit, newdata = testhere, type = "probs")
    pred <- data.frame(Home = testhere$Home, p.home = as.numeric(pred[,1]), p.draw = as.numeric(pred[,2]),
                                p.guest = as.numeric(pred[,3]), Guest = testhere$Guest)
    res <- evaluate2(pred, newdata = testhere, ordinal = TRUE)
    
    bet <- tipico(pred, testhere)
    
    n.here <- length(res$rps)
    index1 <- n-(dim(test)[1])+1  # Index on where to write the results
    index2 <- index1+n.here-1
    Res$rps[index1:index2] <- res$rps
    Res$mllh[index1:index2] <- res$mllh
    Res$cr[index1:index2] <- res$cr
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
  save(file = paste0("Results/ordinal/Res_", Liga, ".rData"), Res)
}

