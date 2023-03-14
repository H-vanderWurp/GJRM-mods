

load("datenWorldCup.rdata")
bets <- read.csv2("Betting results.csv", header = F)

source("Football functions.R")
source("helpers.R")

## coinflip all

ind.home <- c(1, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
              39, 41)
ind.guest <- c(2, 38, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
               35, 36, 40, 42)
set.seed(1)
for(i in 1:320){
  coin <- sample(1:2, 1)
  if(coin == 2){ # flip
    zw <- dat[i, ind.home]
    dat[i, ind.home] <- dat[i, ind.guest]
    dat[i, ind.guest] <- zw
    rm(zw)
  }
  if(i %in% 257:320){
    if(coin == 2){ # flip
      bets[i-256,] <- bets[i-256, c(2, 1, 5, 4, 3)]
    }
  }
}
mean(dat$Goals)
mean(dat$Goals.oppo)

## correct age
WMs <- c(2002, 2006, 2010, 2014, 2018)
dat$age.dev <- dat$age.dev.oppo <- NULL
for(i in WMs){
  t1 <- dat$age[dat$WM == i]
  t2 <- dat$age.oppo[dat$WM == i]
  m.age <- mean(c(t1[1:48], t2[1:48]))  # first 48 matches, because all teams occured 3 times.
  dat$age.dev[dat$WM == i] <- abs(dat$age[dat$WM == i] - m.age)
  dat$age.dev.oppo[dat$WM == i] <- abs(dat$age.oppo[dat$WM == i] - m.age)
  rm(m.age, t1, t2)
}

library(GJRM) # still version 0.2-3

## CV through tournaments

eq1 <- Goals ~ CL.players + UEFA.players + Nation.Coach + Age.Coach + Tenure.Coach + 
  Legionaires + max.teammates + sec.max.teammates + age.dev + Rank + GDP + 
  host + confed + continent + odds + Population + titleholder + Knockout
eq2 <- Goals.oppo ~ CL.players.oppo + UEFA.players.oppo + Nation.Coach.oppo + Age.Coach.oppo + 
  Tenure.Coach.oppo + Legionaires.oppo + max.teammates.oppo + sec.max.teammates.oppo + 
  age.dev.oppo + Rank.oppo + GDP.oppo + host.oppo + confed.oppo + continent.oppo  + 
  odds.oppo + Population.oppo + titleholder.oppo + Knockout
eq3 <- ~ 1
eqlist <- list(eq1, eq2, eq3)

#fitcop <- gjrm(formula = eqlist, data = dat, BivD = "N", margins = c("PO", "PO"), 
       #        Model = "B")
#pdf("../../Figures/Diagcheck.pdf", height = 5, width = 6)
#post.check(fitcop, main = "Residuals in margin 1", main2 = "Residuals in margin 2")
#dev.off()

myfun <- function(Cop){
  resG <- data.frame(matrix(ncol = 6, nrow=5))
  dimnames(resG)[[2]] <- c("rps", "llh", "cr", "MSE", "bet", "bet.n")
  for(i in 1:5){
    WM <- WMs[i]
    train <- dat[dat$WM != WM,]
    test <- dat[dat$WM == WM,]
    
    if(Cop == "indep"){
      fit1 <- glm(eq1, family = "poisson", data = train)
      fit2 <- glm(eq2, family = "poisson", data = train)
    }
    else
      fitcop <- gjrm(formula = eqlist, data = train, BivD = Cop, margins = c("PO", "PO"), 
                     Model = "B")
    if(WM == 2018){
      if(Cop == "indep")
        res <- eval.glm(fit1, fit2, test, bets = bets)
      else
        res <- evalWM(fit = fitcop, test = test, bets = bets)
    }
    else{
      if(Cop == "indep")
        res <- eval.glm(fit1, fit2, test, bets = NULL)
      else
        res <- evalWM(fitcop, test = test, bets = NULL)
    }
    resG$rps[i] <- res$rps
    resG$llh[i] <- res$llh
    resG$cr[i] <- res$cr
    resG$MSE[i] <-res$MSE
    resG$bet[i] <- as.numeric(res$bet[1] * res$bet[2])
    resG$bet.n[i] <- as.numeric(res$bet[2])
  }
  resG <- as.data.frame(t(apply(resG, 2, mean, na.rm = TRUE)))
  resG$Cop <- Cop
  return(resG)
}

#### now for equal penalty:

myfun.pen1 <- function(Cop){
  resG <- data.frame(matrix(ncol = 6, nrow=5))
  dimnames(resG)[[2]] <- c("rps", "llh", "cr", "MSE", "bet", "bet.n")
  for(i in 1:5){
    WM <- WMs[i]
    train <- dat[dat$WM != WM,]
    test <- dat[dat$WM == WM,]
    
    if(Cop == "indep"){
      train.p1 <- train[,c(1, 3:20, 37, 39, 41, 43)]
      train.p2 <- train[,c(2, 38, 4, 21:36, 37, 40, 42, 44)]
      names(train.p2) <- names(train.p1)
      train.here <- rbind(train.p1, train.p2)
      rm(train.p1, train.p2)
      fit1 <- glm(eq1, family = "poisson", data = train.here)
    }
    else
      fitcop <- gjrm(formula = eqlist, data = train, BivD = Cop, margins = c("PO", "PO"), 
                     Model = "B", linear.equal = rep(TRUE, 22))
    if(WM == 2018){
      if(Cop == "indep"){
        test.p1 <- test[,c(1, 3:20, 37, 39, 41, 43)]
        test.p2 <- test[,c(2, 38, 4, 21:36, 37, 40, 42, 44)]
        names(test.p2) <- names(test.p1)
        test.here <- rbind(test.p1, test.p2)
        rm(test.p1, test.p2) 
        res <- eval.glm.equal(fit1, test.here, bets = bets, testfull = test)
      }
      else
        res <- evalWM(fit = fitcop, test = test, bets = bets)
    }
    else{
      if(Cop == "indep"){
        test.p1 <- test[,c(1, 3:20, 37, 39, 41, 43)]
        test.p2 <- test[,c(2, 38, 4, 21:36, 37, 40, 42, 44)]
        names(test.p2) <- names(test.p1)
        test.here <- rbind(test.p1, test.p2)
        rm(test.p1, test.p2) 
        res <- eval.glm.equal(fit1, test.here, bets = NULL, testfull = test)
      }
      else
        res <- evalWM(fitcop, test = test, bets = NULL)
    }
    resG$rps[i] <- res$rps
    resG$llh[i] <- res$llh
    resG$cr[i] <- res$cr
    resG$MSE[i] <-res$MSE
    resG$bet[i] <- as.numeric(res$bet[1] * res$bet[2])
    resG$bet.n[i] <- as.numeric(res$bet[2])
  }
  resG <- as.data.frame(t(apply(resG, 2, mean, na.rm = TRUE)))
  resG$Cop <- Cop
  return(resG)
}
#myfun.pen1("indep")


######## now for penalty 2 ########

myfun.pen2 <- function(Cop, dat){
  resG <- data.frame(matrix(ncol = 6, nrow=5))
  dimnames(resG)[[2]] <- c("rps", "llh", "cr", "MSE", "bet", "bet.n")
  for(i in 1:5){
    WM <- WMs[i]
    train <- dat[dat$WM != WM,]
    test <- dat[dat$WM == WM,]
    
    if(Cop == "indep"){
      require(glmnet)
      train1 <- model.matrix(eq1, data = train)[,-1]
      train2 <- model.matrix(eq2, data = train)[,-1]
      
      ## standardise:
      nu1 <- apply(train1, 2, mean)
      sigma1 <- apply(train1, 2, sd)
      nu2 <- apply(train2, 2, mean)
      sigma2 <- apply(train2, 2, sd)
      train1 <- scale(train1)
      train2 <- scale(train2)
      Y1 <- train$Goals
      Y2 <- train$Goals.oppo
      
      ## CV for the LASSO
      lasso.fam <- "poisson"
      lasso.obj1 <- cv.glmnet(y = Y1, x = train1, family = lasso.fam)
      lasso.obj2 <- cv.glmnet(y = Y2, x = train2, family = lasso.fam)
      
      ## fit again on full data:
      fitpoislasso1 <- glmnet(y = Y1, x = train1, alpha = 1, family = lasso.fam, lambda = lasso.obj1$lambda.min)
      fitpoislasso2 <- glmnet(y = Y2, x = train2, alpha = 1, family = lasso.fam, lambda = lasso.obj2$lambda.min)
      
      ## standardise test data as well:
      testhere1 <- model.matrix(eq1, data = test)[,-1]
      testhere2 <- model.matrix(eq2, data = test)[,-1]
      testhere1 <- testhere1 - rep(nu1, each = dim(testhere1)[1])
      testhere2 <- testhere2 - rep(nu2, each = dim(testhere2)[1])
      testhere1 <- testhere1 / rep(sigma1, each = dim(testhere1)[1])
      testhere2 <- testhere2 / rep(sigma2, each = dim(testhere2)[1])
      train.p1 <- train[,c(1, 4:25)]
      train.p2 <- train[,c(2, 27:48)]
    #  names(train.p2) <- names(train.p1)
    #  train.here <- rbind(train.p1, train.p2)
    #  rm(train.p1, train.p2)
    #  fit1 <- glm(eq1, family = "poisson", data = train.here)
    }
    else
      fitcop <- gjrm.lasso(data = list(train, eqlist), Cop = Cop, plot = TRUE,
                           grid.l = 100, K = 10, CV = FALSE, threshold = 0.01,
                           carry.start.values = FALSE, LASSO.groups = list(c(14:17)))
    if(WM == 2018){
      if(Cop == "indep"){
        #test1 <- test[,c(1, 4:25, 50, 51)]
        #test2 <- test[,c(2, 27:48, 50, 51)]
        #names(test.p2) <- names(test.p1)
        #test.here <- rbind(test.p1, test.p2)
        #rm(test.p1, test.p2) 
        res <- eval.glm.lasso(fit1 = fitpoislasso1, fit2 = fitpoislasso2, test1 = testhere1, 
                              test2 = testhere2, bets = bets, test = test)
      }
      else
        res <- evalWM(fit = fitcop$fit.aic, test = test, bets = bets)
    }
    else{
      if(Cop == "indep"){
        #test.p1 <- test[,c(1, 4:25, 50, 51)]
        #test.p2 <- test[,c(2, 27:48, 50, 51)]
        #names(test.p2) <- names(test.p1)
        #test.here <- rbind(test.p1, test.p2)
        #rm(test.p1, test.p2) 
        res <- eval.glm.lasso(fit1 = fitpoislasso1, fit2 = fitpoislasso2, test1 = testhere1, 
                              test2 = testhere2, bets = NULL, test = test)
      }
      else
        res <- evalWM(fitcop$fit.aic, test = test, bets = NULL)
    }
    resG$rps[i] <- res$rps
    resG$llh[i] <- res$llh
    resG$cr[i] <- res$cr
    resG$MSE[i] <-res$MSE
    resG$bet[i] <- as.numeric(res$bet[1] * res$bet[2])
    resG$bet.n[i] <- as.numeric(res$bet[2])
  }
  resG <- as.data.frame(t(apply(resG, 2, mean, na.rm = TRUE)))
  resG$Cop <- Cop
  return(resG)
}

#myfun.pen2("F", dat = dat.n)
## to do: indep reparieren.
#myfun.pen2("indep", dat)



#eq1 <- Goals ~ 1 + CL.players + UEFA.players + Nation.Coach + Age.Coach + Tenure.Coach +
#  Legionaires + max.teammates + sec.max.teammates + age + Rank + GDP + host + confed + 
#  continent + odds + Population + Knockout + titleholder #+ elo
#eq2 <- Goals.oppo ~ 1 + CL.players.oppo + UEFA.players.oppo + Nation.Coach.oppo +
#  Age.Coach.oppo + Tenure.Coach.oppo + Legionaires.oppo + max.teammates.oppo + sec.max.teammates.oppo +
#  age.oppo + Rank.oppo + GDP.oppo + host.oppo + confed.oppo + continent.oppo + odds.oppo + 
#  Population.oppo + Knockout + titleholder.oppo #+ elo.oppo
#eq3 <- ~ 1
#eqlist <- list(eq1, eq2)

#m1 <- model.matrix(eq1, data = dat)
#m2 <- model.matrix(eq2, data = dat)

#eq1 <- Goals ~ 1 + CL.players + UEFA.players + Nation.Coach1 + Age.Coach + Tenure.Coach +
#  Legionaires + max.teammates + sec.max.teammates + age + Rank + GDP + host1 + confedCAF + confedCONCACAF + confedCONMEBOL +
#  confedUEFA + 
#  continent1 + odds + Population + KnockoutTRUE + titleholder
#eq2 <- Goals.oppo ~ 1 + CL.players.oppo + UEFA.players.oppo + Nation.Coach.oppo1 +
#  Age.Coach.oppo + Tenure.Coach.oppo + Legionaires.oppo + max.teammates.oppo + sec.max.teammates.oppo +
#  age.oppo + Rank.oppo + GDP.oppo + host.oppo1 + confed.oppoCAF + confed.oppoCONCACAF + confed.oppoCONMEBOL + 
#  confed.oppoUEFA + 
#  continent.oppo1 + odds.oppo + 
#  Population.oppo + KnockoutTRUE + titleholder.oppo
#eq3 <- ~ 1
#eqlist <- list(eq1, eq2, eq3)

#dat.n <- data.frame(Goals = dat$Goals, Goals.oppo = dat$Goals.oppo, m1, m2, WM = as.character(dat$WM),
#                    Team = dat$Team, Opponent = dat$Opponent)


## now with penalties combined:

myfun.penboth <- function(Cop, dat){
  resG <- data.frame(matrix(ncol = 6, nrow=5))
  dimnames(resG)[[2]] <- c("rps", "llh", "cr", "MSE", "bet", "bet.n")
  for(i in 1:5){
    WM <- WMs[i]
    train <- dat[dat$WM != WM,]
    test <- dat[dat$WM == WM,]
    
    if(Cop == "indep"){
      require(glmnet)
      train1 <- model.matrix(eq1, data = train)[,-1]
      train2 <- model.matrix(eq2, data = train)[,-1]
      
      ## standardise:
      nu1 <- apply(train1, 2, mean)
      sigma1 <- apply(train1, 2, sd)
      nu2 <- apply(train2, 2, mean)
      sigma2 <- apply(train2, 2, sd)
      train1 <- scale(train1)
      train2 <- scale(train2)
      Y1 <- train$Goals
      Y2 <- train$Goals.oppo
      
      ## CV for the LASSO
      lasso.fam <- "poisson"
      lasso.obj1 <- cv.glmnet(y = Y1, x = train1, family = lasso.fam)
      lasso.obj2 <- cv.glmnet(y = Y2, x = train2, family = lasso.fam)
      
      ## fit again on full data:
      fitpoislasso1 <- glmnet(y = Y1, x = train1, alpha = 1, family = lasso.fam, lambda = lasso.obj1$lambda.min)
      fitpoislasso2 <- glmnet(y = Y2, x = train2, alpha = 1, family = lasso.fam, lambda = lasso.obj2$lambda.min)
      
      ## standardise test data as well:
      testhere1 <- model.matrix(eq1, data = test)[,-1]
      testhere2 <- model.matrix(eq2, data = test)[,-1]
      testhere1 <- testhere1 - rep(nu1, each = dim(testhere1)[1])
      testhere2 <- testhere2 - rep(nu2, each = dim(testhere2)[1])
      testhere1 <- testhere1 / rep(sigma1, each = dim(testhere1)[1])
      testhere2 <- testhere2 / rep(sigma2, each = dim(testhere2)[1])
      train.p1 <- train[,c(1, 4:25)]
      train.p2 <- train[,c(2, 27:48)]
      names(train.p2) <- names(train.p1)
      train.here <- rbind(train.p1, train.p2)
      rm(train.p1, train.p2)
      fit1 <- glm(eq1, family = "poisson", data = train.here)
    }
    else
      fitcop <- gjrm.lasso(data = list(train, eqlist), Cop = Cop, plot = TRUE,
                           grid.l = 100, K = 10, CV = FALSE, threshold = 0.01,
                           carry.start.values = TRUE, LASSO.groups = list(c(14:17)),
                           xi = 1e9, linear.equal = rep(TRUE, 22))
    if(WM == 2018){
      if(Cop == "indep"){
        test.p1 <- test[,c(1, 4:25, 50, 51)]
        test.p2 <- test[,c(2, 27:48, 50, 51)]
        names(test.p2) <- names(test.p1)
        test.here <- rbind(test.p1, test.p2)
        rm(test.p1, test.p2) 
        res <- eval.glm.equal(fit1, test.here, bets = bets, testfull = test)
      }
      else
        res <- evalWM(fit = fitcop$fit.aic, test = test, bets = bets)
    }
    else{
      if(Cop == "indep"){
        test.p1 <- test[,c(1, 4:25, 50, 51)]
        test.p2 <- test[,c(2, 27:48, 50, 51)]
        names(test.p2) <- names(test.p1)
        test.here <- rbind(test.p1, test.p2)
        rm(test.p1, test.p2) 
        res <- eval.glm.equal(fit1, test.here, bets = NULL, testfull = test)
      }
      else
        res <- evalWM(fitcop$fit.aic, test = test, bets = NULL)
    }
    resG$rps[i] <- res$rps
    resG$llh[i] <- res$llh
    resG$cr[i] <- res$cr
    resG$MSE[i] <-res$MSE
    resG$bet[i] <- as.numeric(res$bet[1] * res$bet[2])
    resG$bet.n[i] <- as.numeric(res$bet[2])
  }
  resG <- as.data.frame(t(apply(resG, 2, mean, na.rm = TRUE)))
  resG$Cop <- Cop
  return(resG)
}



