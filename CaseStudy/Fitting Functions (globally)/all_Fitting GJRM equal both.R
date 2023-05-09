## Fitting Cop, equal penalty

exe.cop.equal.all.both <- function(Cop, date.ind){
  library(EUfootball)
  require(GJRM)
  source("../GJRM changes - 2023.R")
  source("application functions.R")
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
  
 # repeat{
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
                   BivD = Cop, margins = c("PO", "PO"), Model = "B",
                   linear.equal = c(FALSE, rep(TRUE, 14)))
    
    ## predict (date instead of matchday)
    match.date <- test$date[1]
    year <- test$SeasonFrom[1]
    testhere <- test
    
    pred <- evaluate1(type = "cop", fit1 = fitcop, newdata = testhere)
    res <- evaluate2(pred, newdata = testhere)
    
    bet <- tipico(pred[[1]], testhere)
    
    n.here <- length(res$rps)
    index1 <- n-(dim(test)[1])+1  # Index on where to write the results
    index2 <- index1+n.here-1
    Res$rps <- res$rps
    Res$mllh <- res$mllh
    Res$cr <- res$cr
    Res$SE <- res$SE
    Res$AE <- res$AE
    Res$SeasonFrom <- year
    Res$matchday <- testhere$matchday
    Res$League <- testhere$League
    
    n.bets <- n.bets + bet$n.bets
    gains <- c(gains, bet$gains)
    
  #}
  Res <- list(Res, n.bets, gains)
  save(file = paste0("Results (globally)/Cop_equal_", Cop,"_", date.ind, "_both", ".rData"), Res)
}

