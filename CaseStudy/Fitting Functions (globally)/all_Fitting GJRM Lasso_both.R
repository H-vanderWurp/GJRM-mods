## Fitting Cop

exe.cop.lasso.all.both <- function(Cop, date.ind){
  library(EUfootball)
  require(GJRM)
  source("../GJRM changes - 2023.R")
  source("application functions.R")
  source("../gjrm lasso wrapper.R")
  Dat <- Matches
  
  
  Dat <- Dat[Dat$date < as.Date("2020-03-01"),]
  
  ## sort by date (formerly sorted by league)
  
  Dat <- Dat[order(Dat$date),]
  
  ind <- unique(Dat$date)[date.ind]
  train <- Dat[Dat$date < ind,]
  test <- Dat[Dat$date == ind,]
  
  
  n <- dim(test)[1]
  
  Res <- data.frame(rps = rep(NA, n), mllh = rep(NA, n),
                    cr = rep(NA, n), SE = rep(NA, n), AE = rep(NA, n),
                    SeasonFrom = rep(NA, n), matchday = rep(NA, n))
  n.bets <- 0
  gains <- numeric(0)
  
  #repeat{
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
    fitcop <- gjrm.lasso(data = list(train, formula = list(eq1, eq2, eq3)), Cop = Cop,
                                  grid.l = 100, threshold = 0.01, carry.start.values = FALSE,
                                  start.nu = 0.4, CV = FALSE, plot = FALSE)
    
    ## predict (date instead of matchday)
    match.date <- test$date[1]
    year <- test$SeasonFrom[1]
    testhere <- test
    
    fitAIC <- fitcop$fit.aic
    fitBIC <- fitcop$fit.bic
    
    predAIC <- evaluate1(type = "cop", fit1 = fitAIC, newdata = testhere)
    resAIC <- evaluate2(predAIC, newdata = testhere)
    
    predBIC <- evaluate1(type = "cop", fit1 = fitBIC, newdata = testhere)
    resBIC <- evaluate2(predBIC, newdata = testhere)
    
    betAIC <- tipico(predAIC[[1]], testhere)
    betBIC <- tipico(predBIC[[1]], testhere)
    
    n.here <- length(resAIC$rps)
    ResAIC$rps <- resAIC$rps
    ResAIC$mllh <- resAIC$mllh
    ResAIC$cr <- resAIC$cr
    ResAIC$SE <- resAIC$SE
    ResAIC$AE <- resAIC$AE
    ResAIC$SeasonFrom <- year
    ResAIC$matchday <- testhere$matchday
    ResAIC$League <- testhere$Liga
    ResBIC$rps <- resBIC$rps
    ResBIC$mllh <- resBIC$mllh
    ResBIC$cr <- resBIC$cr
    ResBIC$SE<- resBIC$SE
    ResBIC$AE <- resBIC$AE
    ResBIC$SeasonFrom <- year
    ResBIC$matchday <- testhere$matchday
    
    n.betsAIC <- n.betsAIC + betAIC$n.bets
    n.betsBIC <- n.betsBIC + betBIC$n.bets
    gainsAIC <- c(gainsAIC, betAIC$gains)
    gainsBIC <- c(gainsBIC, betBIC$gains)
    
  #}
  Res <- list(ResAIC, ResBIC, n.betsAIC, n.betsBIC, gainsAIC, gainsBIC)
  save(file = paste0("Results (globally)/Cop_LASSO_", Cop, "_", date.ind, ".rData"), Res)
}

