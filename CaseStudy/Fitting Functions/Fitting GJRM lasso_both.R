## Fitting Cop, equal penalty

## year. Starting in 2015.

exe.cop.lasso.both <- function(Liga, Cop, s.year = 2015){
  require(EUfootball)
  Dat <- Matches
  source("../GJRM changes - 2023.R")
  source("../gjrm lasso wrapper.R")
  source("application functions.R")
  require(GJRM)
  
  Dat <- Dat[Dat$date < as.Date("2020-03-01"),]
  Dat.sub <- Dat[Dat$League == Liga,]
  
  train <- Dat.sub[Dat.sub$SeasonFrom <= s.year - 1,]
  test <- Dat.sub[Dat.sub$SeasonFrom > s.year - 1,]
  
  n <- dim(test)[1]
  
  ResAIC <- data.frame(rps = rep(NA, n), mllh = rep(NA, n),
                    cr = rep(NA, n), SE = rep(NA, n), AE = rep(NA, n),
                    SeasonFrom = rep(NA, n), matchday = rep(NA, n))
  ResBIC <- data.frame(rps = rep(NA, n), mllh = rep(NA, n),
                       cr = rep(NA, n), SE = rep(NA, n), AE = rep(NA, n),
                       SeasonFrom = rep(NA, n), matchday = rep(NA, n))
  n.betsAIC <- n.betsBIC <- 0
  gainsAIC <- gainsBIC <- numeric(0)
  errorcount <- 0
  
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
    a <- try(fitcop <- gjrm.lasso(data = list(train, formula = list(eq1, eq2, eq3)), Cop = Cop,
                   grid.l = 100, threshold = 0.01, carry.start.values = FALSE,
                   start.nu = 0.4, CV = FALSE, plot = FALSE), silent = TRUE)
    
    
    ## predict:
    md <- test$matchday[1]
    year <- test$SeasonFrom[1]
    ind <- test$matchday == md & test$SeasonFrom == year
    testhere <- test[ind,]
    
    
    if(class(a)[1] == "try-error"){
      errorcount <- errorcount + 1
      print(errorcount)
      n.here <- dim(testhere)[1]
      index1 <- n-(dim(test)[1])+1  # Index on where to write the results
      index2 <- index1+n.here-1
      ResAIC$rps[index1:index2] <- NA
      ResAIC$mllh[index1:index2] <- NA
      ResAIC$cr[index1:index2] <- NA
      ResAIC$SE[index1:index2] <- NA
      ResAIC$AE[index1:index2] <- NA
      ResAIC$SeasonFrom[index1:index2] <- year
      ResAIC$matchday[index1:index2] <- md
      ResBIC$rps[index1:index2] <- NA
      ResBIC$mllh[index1:index2] <- NA
      ResBIC$cr[index1:index2] <- NA
      ResBIC$SE[index1:index2] <- NA
      ResBIC$AE[index1:index2] <- NA
      ResBIC$SeasonFrom[index1:index2] <- year
      ResBIC$matchday[index1:index2] <- md
      
      n.betsAIC <- n.betsAIC + 0
      n.betsBIC <- n.betsBIC + 0
      gainsAIC <- c(gainsAIC, 0)
      gainsBIC <- c(gainsBIC, 0)
    }
    else{
      fitAIC <- fitcop$fit.aic
      fitBIC <- fitcop$fit.bic
      
      predAIC <- evaluate1(type = "cop", fit1 = fitAIC, newdata = testhere)
      resAIC <- evaluate2(predAIC, newdata = testhere)
      
      predBIC <- evaluate1(type = "cop", fit1 = fitBIC, newdata = testhere)
      resBIC <- evaluate2(predBIC, newdata = testhere)
      
      betAIC <- tipico(predAIC[[1]], testhere)
      betBIC <- tipico(predBIC[[1]], testhere)
      
      n.here <- length(resAIC$rps)
      index1 <- n-(dim(test)[1])+1  # Index on where to write the results
      index2 <- index1+n.here-1
      ResAIC$rps[index1:index2] <- resAIC$rps
      ResAIC$mllh[index1:index2] <- resAIC$mllh
      ResAIC$cr[index1:index2] <- resAIC$cr
      ResAIC$SE[index1:index2] <- resAIC$SE
      ResAIC$AE[index1:index2] <- resAIC$AE
      ResAIC$SeasonFrom[index1:index2] <- year
      ResAIC$matchday[index1:index2] <- md
      ResBIC$rps[index1:index2] <- resBIC$rps
      ResBIC$mllh[index1:index2] <- resBIC$mllh
      ResBIC$cr[index1:index2] <- resBIC$cr
      ResBIC$SE[index1:index2] <- resBIC$SE
      ResBIC$AE[index1:index2] <- resBIC$AE
      ResBIC$SeasonFrom[index1:index2] <- year
      ResBIC$matchday[index1:index2] <- md
      
      n.betsAIC <- n.betsAIC + betAIC$n.bets
      n.betsBIC <- n.betsBIC + betBIC$n.bets
      gainsAIC <- c(gainsAIC, betAIC$gains)
      gainsBIC <- c(gainsBIC, betBIC$gains)
    }
    
    ## current matchday to training data
    train <- rbind(train, testhere)
    test <- test[!ind,]
    #rm(fitpois1, fitpois2, pred.pois, res.pois, testhere, ind, index1, index2,
    #   year, md)
    if(dim(test)[1] == 0)
      break
    if(test$SeasonFrom[1] > s.year)
      break
  }
  Res <- list(ResAIC = ResAIC, ResBIC = ResBIC, 
              n.betsAIC = n.betsAIC, n.betsBIC = n.betsBIC, 
              gainsAIC = gainsAIC, gainsBIC = gainsBIC,
              errors = errorcount)
  save(file = paste0("Results/cop_lasso_both/Res_", Liga, s.year, "_", Cop, ".rData"), Res)
}


