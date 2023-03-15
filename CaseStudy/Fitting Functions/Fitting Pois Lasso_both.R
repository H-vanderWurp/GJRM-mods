## Fitting of independent Poisson:

exe.pois.lasso.both <- function(Liga){
  require(EUfootball)
  Dat <- Matches
  source("application functions.R")
  library(skellam)
  library(glmmLasso)
  library(glmnet)

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
    ## incomplete cases entfernen
    train <- train[complete.cases(train),]
    
    train1 <- model.matrix(eq1, data = train)[,-1]
    train2 <- model.matrix(eq2, data = train)[,-1]
    
    ## standardise:
    nu1 <- apply(train1, 2, mean)
    sigma1 <- apply(train1, 2, sd)
    nu2 <- apply(train2, 2, mean)
    sigma2 <- apply(train2, 2, sd)
    train1 <- scale(train1)
    train2 <- scale(train2)
    Y1 <- train$Goals90Home
    Y2 <- train$Goals90Guest
    
    ## CV for the LASSO
    lasso.fam <- "poisson"
    lasso.obj1 <- cv.glmnet(y = Y1, x = train1, family = lasso.fam)
    lasso.obj2 <- cv.glmnet(y = Y2, x = train2, family = lasso.fam)
    
    ## fit again on full data:
    fit1 <- glmnet(y = Y1, x = train1, alpha = 1, family = lasso.fam, lambda = lasso.obj1$lambda.min)
    fit2 <- glmnet(y = Y2, x = train2, alpha = 1, family = lasso.fam, lambda = lasso.obj2$lambda.min)
    
    
    ## predict:
    md <- test$matchday[1]
    year <- test$SeasonFrom[1]
    ind <- test$matchday == md & test$SeasonFrom == year
    testhere <- test[ind,]
    ind.in <- complete.cases(testhere)
    Home <- testhere$Home
    Guest <- testhere$Guest
    testhere.full <- testhere
    testhere <- testhere[ind.in,]
    
    testhere1 <- model.matrix(eq1, data = testhere)[,-1]
    testhere2 <- model.matrix(eq2, data = testhere)[,-1]
    testhere1 <- testhere1 - rep(nu1, each = dim(testhere1)[1])
    testhere2 <- testhere2 - rep(nu2, each = dim(testhere2)[1])
    testhere1 <- testhere1 / rep(sigma1, each = dim(testhere1)[1])
    testhere2 <- testhere2 / rep(sigma2, each = dim(testhere2)[1])
    
    pred <- evaluate1(type = "glmnet", fit1, fit2, newdata = testhere1, newdata2 = testhere2,
                      Home = Home, Guest = Guest, ind = ind.in, 
                      Y1 = testhere$Goals90Home, Y2 = testhere$Goals90Guest)
    res <- evaluate2(pred, newdata = testhere.full)
    
    bet <- tipico(pred[[1]], testhere.full)
    
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
    train <- rbind(train, testhere.full)
    test <- test[!ind,]
    #rm(fitpois1, fitpois2, pred.pois, res.pois, testhere, ind, index1, index2,
    #   year, md)
    if(dim(test)[1] == 0)
      break
  }
  Res <- list(Res, n.bets, gains)
  save(file = paste0("Results/pois_lasso_both/Res_", Liga, ".rData"), Res)
}

