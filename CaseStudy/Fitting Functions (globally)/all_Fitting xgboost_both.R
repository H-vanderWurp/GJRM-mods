## Fitting of xgboost

exe.xgboost.both <- function(Liga){
  require(EUfootball)
  Dat <- Matches
  source("application functions.R")
  library(xgboost)

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
  
  n.bets <- 0
  gains <- numeric(0)
  
  ind1 <- (!is.na(train$Goals90Home)) & (!is.na(train$pHome)) & (!is.na(train$MVHome.T)) &
    (!is.na(train$pGuest)) & (!is.na(train$MVGuest.T))
  ind2 <- (!is.na(train$Goals90Guest)) & (!is.na(train$pGuest)) & (!is.na(train$MVGuest.T)) &
    (!is.na(train$pHome)) & (!is.na(train$MVHome.T))
  ind <- ind1 & ind2
  train <- train[ind, ]
  
  eta <- 0.2 
  gamma <- 5 
  nrounds <- 500

  repeat{
    # indep equations
    print(dim(test))
    
    label1 <- train$Goals90Home[(!is.na(train$Goals90Home)) & (!is.na(train$pHome)) & (!is.na(train$MVHome.T)) &
                                  (!is.na(train$pGuest)) & (!is.na(train$MVGuest.T))]
    label2 <- train$Goals90Guest[(!is.na(train$Goals90Guest)) & (!is.na(train$pGuest)) & (!is.na(train$MVGuest.T)) &
                                   (!is.na(train$pHome)) & (!is.na(train$MVHome.T))]
    train1 <- model.matrix(eq1, data = train[!is.na(train$Goals90Home), ])
    train1 <- list(data = train1, label = label1)
    train2 <- model.matrix(eq2, data = train[!is.na(train$Goals90Guest), ])
    train2 <- list(data = train2, label = label2)
    
    ## fit:
    fit1 <- xgboost(data = train1$data, label = train1$label,
                    nrounds = nrounds, verbose = 0, params = list(objective = "count:poisson",
                                                                eta = eta, gamma = gamma))
    fit2 <- xgboost(data = train2$data, label = train2$label,
                    nrounds = nrounds, verbose = 0, params = list(objective = "count:poisson",
                                                                eta = eta, gamma = gamma))
    
    ## predict:
    md <- test$matchday[1]
    year <- test$SeasonFrom[1]
    ind <- test$matchday == md & test$SeasonFrom == year
    testhere <- test[ind,]
    testhere1 <- model.matrix(eq1, testhere)
    testhere2 <- model.matrix(eq1, testhere)
    
    pred <- evaluate1(type = "pois", fit1, fit2, newdata = testhere, eq1 = eq1,
                      eq2 = eq2, xgboost = TRUE)
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
  save(file = paste0("Results (globally)/Res_xgboost_both.rData"), Res)
}

