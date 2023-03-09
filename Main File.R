library(trustOptim)
library(GJRM)   # old version control: 0.2-1  28.05.2019. Updated to 0.2-3 08.03.2023
library(ggplot2)
library(copula)
#library(VineCopula)
library(VC2copula)
library(parallel)
library(skellam)

source("supporting functions.R")
#source("Football functions.R")
source("../GJRM changes - 2023.R")

####################### Generating process #########################


##examples: 
V <- gen(Cop="C", beta=list(c(0.25,0.15, -0.2), c(-0.1, 0, 0.1)), n = 50000, theta=1,
         seed=sample(2^31,1))
Hendrik("N", beta=list(c(0.25,0.15, -0.2), c(-0.1, 0, 0.1)), n=500, theta=0)
DoSim("C90", truebeta=list(runif(4,-1,1),runif(4,-1,1)), -5, 250, 10)

## with example from above:
V <- gen(Cop="G", beta=list(c(0.25,0.15, -0.2), c(-0.1, 0, 0.1)), n = 50000, theta=1.5,
         seed=sample(2^31,1))
plot(table(V[,1], V[,2]))



#### Simulation in Chapter 2 ####
set.seed(1904)
A <- DoAllSims(250, times=100, tau=0.1, truebeta = list(c(0.5, 0.2, -0.2, 0), c(0.2, -0.1, 0.1, 0.5)))
save(A, file = "Chap2_Sim_erg_01.rData")

## Table 2.1
A$AICpicks
## Figure 2.1
pdf("Pen1Sim01.pdf", width = 8, height = 4)
ggplot(A$MSEErg, aes(x=true, y=MSE, fill=fit)) + geom_boxplot() + scale_fill_grey(start = 0.3, end = 0.95)
dev.off()

set.seed(1904)
A <- DoAllSims(250, times=100, tau=0.7, truebeta = list(c(0.5, 0.2, -0.2, 0), c(0.2, -0.1, 0.1, 0.5)))
save(A, file = "Chap2_Sim_erg_07.rData")
## Table 2.2
A$AICpicks
## Figure 2.2
pdf("Pen1Sim07.pdf", width = 8, height = 4)
ggplot(A$MSEErg, aes(x=true, y=MSE, fill=fit)) + geom_boxplot() + scale_fill_grey(start = 0.3, end = 0.95)
dev.off()

A <- DoAllSims(250, times=100, tau= -0.6, truebeta = list(c(0.5, 0.2, -0.2, 0), c(0.2, -0.1, 0.1, 0.5)))
## Figure 2.3
pdf("Pen1Sim-06.pdf", width = 8, height = 4)
ggplot(A$MSEErg, aes(x=true, y=MSE, fill=fit)) + geom_boxplot() + scale_fill_grey(start = 0.3, end = 0.95)
dev.off()



#pdf("SimStudy.pdf", width = 12, height = 8)
#set.seed(1904)
A <- DoAllSims(250, 100, theta = 1.5, thetaN = 0.2, truebeta = list(c(0.5, 0.2, -0.2),
                                                                    c(0.2, -0.1, 0.1)))  
data <- A[[2]]
data <- data[data$sqerr<0.5,]
library(ggplot2)
ggplot(data, aes(x=true, y=sqerr, fill=fit)) + geom_boxplot()
data <- data[data$sqerr<0.5,]
ggplot(data, aes(x=true, y=sqerr, fill=fit)) + geom_boxplot()
dev.off()


######### Compare Penalty vs standard GJRM ##########

## dataset
V <- gen(Cop="C", beta=list(c(0.25,0.15, -0.2, 0.5), c(-0.1, 0, 0.1, 0.5)), n = 500, theta=1,
         seed=sample(2^31,1)) 

eq1 <- V1 ~ 1 + V3 + V4 + V5
eq2 <- V2 ~ 1 + V6 + V7 + V8
eq3 <- ~ 1
eq_list <- list(eq1, eq2, eq3)


## Example for Sim2, single true Copula, fit all Copulas, Marra vs. Hendrik
Cops <- c("N","F","G","C","C90","J")
kend1 <- kendall(0.25)
kend2 <- kendall(-0.25)

A <- data.frame()
Cops1 <- c("N", "F", "G0", "C0", "J0")
Cops2 <- c("N", "F", "C90")
tau1w <- c(1, 2, 3, 4, 6)
tau2w <- c(1, 2, 5)
for (i in (1:5)) {
  print(Cops1[i])
  A <- rbind(A, Sim2(Cops1[i], theta=kend1[tau1w[i]], times=100, n=250, truebeta = c(0.25, 0.2, -0.35, 0)))
}
for (i in (1:3)) {
  print(Cops2[i])
  A <- rbind(A, Sim2(Cops2[i], theta=kend2[tau2w[i]], times=100, n=250, truebeta = c(0.25, 0.2, -0.35, 0)))
}

save(A, file="ErgSimEqual_31052019.rdata")

pdf("./Paper/Figures/SimMarravsMe.pdf", width = 6, height = 4.5)
ggplot(A, aes(x=true.and.fit, y=MSE, fill=version)) + geom_boxplot() + 
  geom_vline(xintercept = 5.5, linetype = 2) + ggtitle(bquote(paste(tau," = \u00B1", 0.25)))
dev.off()


################################################################################
########################## Let's start with Football ###########################
bet <- read.csv2("C:/Users/vanderwurp/Desktop/Arbeit/WM_Modellierungen/Paper/Daten/betting results.csv", header=F)
load("datenWorldCup.Rdata")  # named 'dat'.
head(dat)

wins1 <- dat2018$Goals > dat2018$Goals.oppo
draw <- dat2018$Goals == dat2018$Goals.oppo
wins2 <- dat2018$Goals < dat2018$Goals.oppo
trueresult <- data.frame(wins1 = wins1, draw = draw, wins2 = wins2)

eq1 <- Goals ~ 1 + CL.players + UEFA.players + Nation.Coach + Age.Coach + Tenure.Coach +
  Legionaires + max.teammates + sec.max.teammates + age + Rank + GDP + host + confed + 
  continent + odds + Population + Knockout + titleholder #+ elo
eq2 <- Goals.oppo ~ 1 + CL.players.oppo + UEFA.players.oppo + Nation.Coach.oppo +
  Age.Coach.oppo + Tenure.Coach.oppo + Legionaires.oppo + max.teammates.oppo + sec.max.teammates.oppo +
  age.oppo + Rank.oppo + GDP.oppo + host.oppo + confed.oppo + continent.oppo + odds.oppo + 
  Population.oppo + Knockout + titleholder.oppo #+ elo.oppo
eqlist <- list(eq1, eq2)

## fit model with 4 Worldcups and predict the last



##hv cross validation over all copulas and between Marra and Hendrik

##hv: does cv for single wc and single copula
FootballStudy <- function(WM, Cop, tau.bet = 1, kelly = FALSE) {
  print(paste("WM ",WM))
  i <- WM
  ind <- dat$WM==i
  d.train <- dat[!ind,]
  d.test <- dat[ind,]
  fitmarra <- gjrm(eqlist, data=d.train, BivD=Cop, margins=c("PO","PO"), Model="B",
                   gamlssfit=TRUE, linear.equal = FALSE)
  fithendrik <- gjrm(eqlist, data=d.train, BivD=Cop, margins=c("PO","PO"), Model="B",
                      gamlssfit=TRUE, linear.equal = TRUE)
  lambdamarra1 <- exp(predict(fitmarra, newdata=d.test, eq=1))
  lambdamarra2 <- exp(predict(fitmarra, newdata=d.test, eq=2))
  lambdahendrik1 <- exp(predict(fithendrik, newdata=d.test, eq=1))
  lambdahendrik2 <- exp(predict(fithendrik, newdata=d.test, eq=2))
  
  n.pred <- dim(d.test)[1]
  #marraGamePred <- c()
  #hendrikGamePred <- c()

  marraGamePred <- calc.probs(fitmarra, d.test, 20)
  hendrikGamePred <- calc.probs(fithendrik, d.test, 20)
  
  
  # for (j in 1:n.pred) {
  #   
  #   marraGamePred <- rbind(marraGamePred,
  #                          gameCopula(lambdamarra1[j], lambdamarra2[j], Cop, 
  #                                     summary(fitmarra)$theta, method="p"))
  #   hendrikGamePred <- rbind(hendrikGamePred,
  #                            gameCopula(lambdahendrik1[j], lambdahendrik2[j], Cop,
  #                                       summary(fithendrik)$theta, method="p"))
  # }
  
  ## calculate quality of those predictions
  zwm <- rps(marraGamePred, d.test)
  zwh <- rps(hendrikGamePred, d.test)
  rpsmarra <- mean(zwm$rps.vec)
  rpshendrik <- mean(zwh$rps.vec)
  llhmarra <- mean(zwm$likelihood)
  llhhendrik <- mean(zwh$likelihood)
  classratemarra <- mean(zwm$classpred==zwm$result)
  classratehendrik <- mean(zwh$classpred==zwh$result)
  MSEhendrik <- mean(sqrt((d.test$Goals - lambdahendrik1)^2 + (d.test$Goals.oppo - lambdahendrik2)^2))
  MSEmarra <- mean(sqrt((d.test$Goals - lambdamarra1)^2 + (d.test$Goals.oppo - lambdamarra2)^2))
  
  if(WM=="2018" & kelly==FALSE) {
    win.marra <- tipico(marraGamePred, tau.bet=tau.bet, kelly=FALSE, trueresult)$win.ratio
    win.hendrik <- tipico(hendrikGamePred, tau.bet=tau.bet, kelly=FALSE, trueresult)$win.ratio
  }
  if(WM!="2018") {
    win.marra <- win.hendrik <- NA
  }
  
  return(data.frame( wm = i, Copula = Cop, Version = c("Marra", "Hendrik"),
              rps = c(rpsmarra, rpshendrik),
              likelihood = c(llhmarra, llhhendrik),
              classrate = c(classratemarra, classratehendrik),
              betting = c(win.marra, win.hendrik),
              MSEgoals = c(MSEmarra, MSEhendrik)))
}

##hv: does cs for all wcs and single copula
FootballStudyAll <- function(Cop, kelly=FALSE, tau.bet=1){
  print(paste("Aktuelle Copula ist ", Cop))
  WMs <- c("2002", "2006", "2010", "2014", "2018")
  A <- lapply(WMs, FootballStudy, Cop=Cop, kelly=kelly, tau.bet=tau.bet)
  A <- rbind(A[[1]],A[[2]],A[[3]],A[[4]],A[[5]])
  return(A)
}

Cops <- c("N", "C0", "C90", "C180", "C270", "J0", "J90", "J180", "J270", "G0", 
  "G90", "G180", "G270", "F", "AMH", "FGM", "T", "PL")

##hv: error with "HO" Hougaard Copula. Maybe "HO" from gjrm and "khoudrajiBivCopula" from copula are different ones.
A <- lapply(Cops, FootballStudyAll)
B <- data.frame()
for (i in 1:18) {
  B <- rbind(B, A[[i]])
}
remove(A)


#Cops2 <- c("C0C90", "C0C270", 
#           "C180C90", "C180C270", "G0G90", "G0G270", "G180G90", "G180G270", "J0J90",
#           "J0J270", "J180J90", "J180J270")
#A <- lapply(Cops2, FootballStudyAll)
## error with C0C90, try it single
#gjrm(eqlist, data=dat, BivD="J0J90", margins=c("PO","PO"), Model="B", gamlssfit = TRUE)


#ggplot(B, aes(x=Copula, y=rps, fill=Version)) + geom_boxplot()
## means over wms
B1 <- aggregate(B$rps, list(B$Copula, B$Version), mean)
B2 <- aggregate(B$likelihood, list(B$Copula, B$Version), mean)
B3 <- aggregate(B$classrate, list(B$Copula, B$Version), mean)
B4 <- aggregate(B$MSEgoals, list(B$Copula, B$Version), mean)
Bg <- cbind(B1, B2$x, B3$x, B4$x)

bettingh <- B$betting[seq(10, 180, 10)]
bettingm <- B$betting[seq(9, 180, 10)]
Bg$betting <- c(bettingh, bettingm)

dimnames(Bg)[[2]] <- c("Copula", "Version", "rps", "likelihood", "classrate", "MSEGoals", "betting")
Bg$Version <- ifelse(Bg$Version=="Hendrik", "penalised", "unpenalised")
Bg$betting <- c(bettingh, bettingm)

Erg <- cbind(Bg[1:18,],Bg[19:36,])
save(Erg, file="Erg.rdata")

## umsortieren
Erg <- Erg[,-c(2, 8, 9)]
Erg <- Erg[,c(1,2,7,3,8,4,9,6,11,5,10)]
dimnames(Erg)[[2]] <- c("Copula", "rps", "rps", "llh", "llh", "cr", "cr", "bet", "bet", "MSEGoals", "MSEGoals")
print(xtable(Erg, digits=3), include.rownames=FALSE, digits=3)

## decide for winning copula. Use ranks
Erg <- Bg[1:18,]
indep <- func1()
Erg <- rbind(Erg, data.frame(Copula="indep", Version="penalised", rps=as.numeric(indep[2]),
                        likelihood=as.numeric(indep[4]), classrate=as.numeric(indep[6]),
                        MSEGoals=as.numeric(indep[10]), betting=as.numeric(indep[8])))
rankings <- data.frame(Copula=Erg$Copula, rps=rank(Erg$rps, ties.method="min"), 
                       llh=rank(-Erg$likelihood, ties.method="min"), 
           class=rank(-Erg$classrate, ties.method="min"), 
           bet=rank(-Erg$betting, ties.method="min"),
           MSE=rank(Erg$MSEGoals, ties.method="min"))
rankings$sum <- apply(rankings[,-1], 1, sum)
(rankings <- rankings[order(rankings$sum),])

## rankings cutten
print(xtable(cbind(rankings[1:10,], rbind(rankings[11:19,],NA))), include.rownames=FALSE)

ggplot(data=Bg, aes(x=Copula, y=rps)) + geom_bar(aes(fill = Version), position = "dodge", stat="identity")
ggplot(data=Bg, aes(x=Copula, y=likelihood)) + geom_bar(aes(fill = Version), position = "dodge", stat="identity")
ggplot(data=Bg, aes(x=Copula, y=betting)) + geom_bar(aes(fill = Version), position = "dodge", stat="identity")
ggplot(data=Bg, aes(x=Copula, y=MSEGoals)) + geom_bar(aes(fill = Version), position = "dodge", stat="identity")

########## Which Copula fits best? ###########

## top3: C0, T, PL
fitFGM <- gjrm(eqlist, data=dat, BivD="FGM", Model="B", margins=c("PO","PO"), linear.equal=T)
fitC90 <- gjrm(eqlist, data=dat, BivD="C90", Model="B", margins=c("PO","PO"), linear.equal=T)
fitG90 <- gjrm(eqlist, data=dat, BivD="G90", Model="B", margins=c("PO","PO"), linear.equal=T)

s <- summary(fitFGM)
0.5 * (s$tableP1[,2] + s$tableP2[,2])

pdf("./Paper/Figures/qqplots.pdf", width = 6, height = 6)
post.check(fitFGM)
dev.off()

## AIC ?ber alle:
aic <- c()
for (i in Cops) {
  aic <- c(aic, AIC(gjrm(eqlist, data=dat, BivD=i, Model="B", margins=c("PO","PO"),
                         linear.equal = TRUE)))
}
aic <- t(aic)
dimnames(aic)[[2]] <- Cops

## show coefficients

a <- fitAMH$coefficients
a[1:22]

## take a look at betting behavior: Kelly + values of tau.bet

## C0
dat2018 <- dat[dat$WM=="2018",]

fit.bet <- gjrm(eqlist, data=dat[dat$WM!="2018",], BivD="FGM", Model="B", 
                margins=c("PO","PO"), linear.equal=TRUE)

## Tipico Function see appendix
## Test it:
GamePred <- calc.probs(fit.bet, dat2018, 20)
tipico(GamePred, tau.bet=2, kelly=TRUE, trueresult)

## lets copy Andreas' plot:
## crunch some numbers:

tau.seq <- seq(1, 1.5, by = 0.001)

BETS1 <- sapply(tau.seq, tipico, GamePred=GamePred, kelly=FALSE, trueresult)
BETS2 <- sapply(tau.seq, tipico, GamePred=GamePred, kelly=TRUE, trueresult)
tau.seq <- tau.seq - 1

par(mfrow=c(1,1))
pdf("./Paper/Figures/Bettingresults.pdf", width = 6, height = 4.5)
plot(tau.seq, unlist(BETS2[1,]), type="l", col="grey", ylim=c(-0.15,1.6), ylab="Return ratio", xlab=expression(tau))
points(tau.seq, unlist(BETS1[1,]), type="l")
abline(h=0, lty=2, col="gray75")
abline(h=0.5, lty=2, col="gray75")
legend(x="topleft", bty="n", legend=c("Kelly", "constant"), lty=c(1,1), col=c("grey","black"))
dev.off()

pdf("./Paper/Figures/Bettingnumbers.pdf", width = 6, height = 4.5)
plot(tau.seq, unlist(BETS1[2,]), type="l", ylab="Number of bets placed", xlab=expression(tau))
abline(h=c(0, 10, 20, 30, 40, 50, 60), col="gray75", lty=2)
dev.off()





##### Old: Check what is still needed: ######

## compare with independent copula
library(skellam)

func1 <- function() {
  datind <- rbind(dat[, c(1,3,4,5:20,37,39,41)],
                  setNames(dat[, c(2,38,4, 21:36, 37, 40,42)], names(dat[, c(1,3,4,5:20,37,39,41)])))
  rpsergfree <- c()
  rpsergequl <- c()
  llhergfree <- c()
  llhergequl <- c()
  classifree <- c()
  classiequl <- c()
  MSEfree <- c()
  MSEequl <- c()
  
  for (i in c("2002", "2006", "2010", "2014", "2018")) {
    ## betas forced equal
    d.train <- datind[datind$WM!=i,]
    d.test <- datind[datind$WM==i,]
    fit <- glm(eq1, data=d.train, family="poisson")
    preds <- predict(fit, newdata=d.test)
    lambda1 <- exp(preds[1:64])
    lambda2 <- exp(preds[65:128])
    p.draw <- dskellam(0, lambda1=lambda1, lambda2=lambda2)
    p.win1 <- 1 - pskellam(0, lambda1=lambda1, lambda2=lambda2)
    p.win2 <- pskellam(-1, lambda1=lambda1, lambda2=lambda2)
    Tabeq <- data.frame(tag1 = d.test$Team[1:64],
                        win1 = p.win1,
                        draw = p.draw,
                        win2 = p.win2,
                        tag2 = d.test$Team[65:128])
    MSEequl <- c(MSEequl, mean(sqrt((d.test$Goals[1:64] - lambda1)^2 + (d.test$Goals[65:128] - lambda2)^2)))
    ## betas free
    fit1 <- glm(eq1, data=d.train[1:256,], family="poisson")
    fit2 <- glm(eq1, data=d.train[257:512,], family="poisson")
    lambda1 <- exp(predict(fit1, newdata=d.test[1:64,]))
    lambda2 <- exp(predict(fit2, newdata=d.test[65:128,]))
    p.draw <- dskellam(0, lambda1=lambda1, lambda2=lambda2)
    p.win1 <- 1 - pskellam(0, lambda1=lambda1, lambda2=lambda2)
    p.win2 <- pskellam(-1, lambda1=lambda1, lambda2=lambda2)
    Tabfree <- data.frame(tag1 = d.test$Team[1:64],
                          win1 = p.win1,
                          draw = p.draw,
                          win2 = p.win2,
                          tag2 = d.test$Team[65:128])
    
    zw1 <- rps(Tabeq, data.frame(Goals = d.test$Goals[1:64],
                          Goals.oppo = d.test$Goals[65:128]))
    zw2 <- rps(Tabfree, data.frame(Goals = d.test$Goals[1:64],
                            Goals.oppo = d.test$Goals[65:128]))
    rpsergequl <- c(rpsergequl, mean(unlist(zw1[1])))
    rpsergfree <- c(rpsergfree, mean(unlist(zw2[1])))
    llhergequl <- c(llhergequl, mean(unlist(zw1[3])))
    llhergfree <- c(llhergfree, mean(unlist(zw2[3])))
    classiequl <- c(classiequl, mean(unlist(zw1[2])==unlist(zw1[4])))
    classifree <- c(classifree, mean(unlist(zw2[2])==unlist(zw2[4])))
    MSEfree <- c(MSEfree,mean(sqrt((d.test$Goals[1:64] - lambda1)^2 + (d.test$Goals[65:128] - lambda2)^2)))
    
    if(i=="2018") {
      winfree <- tipico(Tabfree, trueresult = trueresult, tau.bet = 1, kelly = F)$win.ratio
      winequl <- tipico(Tabeq, trueresult = trueresult, tau.bet = 1, kelly=F)$win.ratio
    }
  }
  return(data.frame(rps.free = mean(rpsergfree),
                    rps.equal = mean(rpsergequl),
                    likelihood.free = mean(llhergfree),
                    likelihood.equal = mean(llhergequl),
                    classi.free = mean(classifree),
                    classi.equal = mean(classiequl),
                    bet.free = winfree,
                    bet.equal = winequl,
                    MSE.free = mean(MSEfree),
                    MSE.equal = mean(MSEequl)
                    ))
}
func1()
#    rps.free   rps.equal   likelihood.free   likelihood.equal
#  0.2114228    0.1982173       0.3899367         0.398137

# fit T Copula as a "best" result
fitT <- gjrm2(data=dat, eqlist, BivD="T", Model="B", margins=c("PO","PO"))


summary(fitAMH) # theta = 0.417, kendalls tau = 0.104
summary(fitC270) # theta ~~ 0, kendalls tau ~~ 0.

mean(exp(predict(fitAMH, eq=1))) # 1.292476
mean(exp(predict(fitAMH, eq=2))) # 1.14

plotcop("T", 0.154, 200)

library("plot3D")
library("plot3Drgl")


plotcop <- function(Cop, theta, n) {
  Copula <- tCopula(param = theta)
  myMVDC <- mvdc(Copula, margins=c("pois","pois"), paramMargins = list(
    list(lambda=1.317629), list(lambda=1.153186)))
  a <- rMvdc(n, myMVDC)
  plot(jitter(a))
}

myMVDC <- mvdc(tCopula(0.154), margins = c("pois","pois"), paramMargins = list(
  list(lambda=1.317629), list(lambda=1.153186)))
a <- as.matrix(table(as.data.frame(rMvdc(1000, myMVDC))))/1000

image.plot(a, col = rev(heat.colors(25)))

##hv: show how T-Copula looks here:
gameCopula(1.3, 1.2, "T", 0.154, method="p")

##hv: create Axis 0:6 for Goals


#hist3D(z=a, x=0:6, y=0:6)
#plotrgl()

plot(jitter(dat$Goals), jitter(dat$Goals.oppo))

load("footballpredictions.rdata")

## Compare RPS and likelihood from T Copula (best?) with RF and lasso from Andreas


andreas <- function(dat, eqlist) {
  eq_1 <- eqlist[[1]]
  eq_2 <- eqlist[[2]]
  rps.rf.k <- c()
  rps.lasso.k <- c()
  rps.lasso.strict.k <- c()
  truerate.rf.k <- c()
  truerate.lasso.k <- c()
  truerate.lasso.strict.k <- c()
  likelihood.rf.k <- c()
  likelihood.lasso.k <- c()
  likelihood.lasso.strict.k <- c()
  for(wm in c("2002","2006","2010","2014", "2018")){
    #cat("WM",wm,"\n")
    d.train <- subset(dat, WM!=wm)
    d.test <- subset(dat, WM==wm)
    
    dimnames(d.train)[[2]][c(2,21:37, 38, 40)] <- dimnames(d.train[,c(1,5:20, 37, 3, 39)])[[2]]
    d.train.rf <- d.train[,c(1,3,5:20, 37, 39)]
    d.train.rf <- rbind(d.train.rf, d.train[,c(2,21:37, 38, 40)])
    dimnames(d.test)[[2]][c(2,21:37, 38, 40)] <- dimnames(d.test[,c(1,5:20, 37, 3, 39)])[[2]]
    d.test.rf <- d.test[,c(1,3,5:20, 37, 39)]
    d.test.rf <- rbind(d.test.rf, d.test[,c(2,21:37, 38, 40)])
    
    w <- c(3,4,6,7,8,9,10,11,12,13,17,18,20)
    for (i in 1:dim(d.train)[[1]]) {        # Schleife fuer Differenzen
      d.train.rf[i, w] <- d.train.rf[i, w] - d.train.rf[i+dim(d.train)[[1]], w]
      d.train.rf[i+dim(d.train)[[1]],w] <- -d.train.rf[i, w]
    }
    for (i in 1:64) {        # Schleife fuer Differenzen
      d.test.rf[i, w] <- d.test.rf[i, w] - d.test.rf[i+64, w]
      d.test.rf[i+64,w] <- -d.test.rf[i, w]
    }
    
    set.seed(20190304) # Date
    
    rf <- party::cforest(eq_1, data=d.train.rf, controls = cforest_unbiased(mtry=max(floor((ncol(dat)-4)/3), 1)))
    p.rf <- predict(rf, newdata = d.test.rf)
    fitbiv1 <- glm(eq1, data=d.train.rf, family="poisson")
    fitbiv2 <- glm(eq2, data=d.train.rf, family="poisson")
    
    
    
    des.train <- model.matrix(eq_1, data = d.train.rf)
    des.test <- model.matrix(eq_1, data = d.test.rf)
    lasso <- cv.glmnet(des.train, d.train.rf$Goals, family="poisson")
    p.lasso <- exp(predict(lasso, des.test, s="lambda.min"))
    p.lasso.strict <- exp(predict(lasso, des.test))
    
    lambda1 <- p.rf[1:64]
    lambda2 <- p.rf[65:128]
    lambda1.lasso <- p.lasso[1:64]
    lambda2.lasso <- p.lasso[65:128]
    lambda1.lasso.strict <- p.lasso.strict[1:64]
    lambda2.lasso.strict <- p.lasso.strict[65:128]
    
    probs.rf <- c()
    probs.lasso <- c()
    probs.lasso.strict <- c()
    for (i in 1:length(lambda1)) {
      probs.rf <- rbind(probs.rf,game(lambda1[i], lambda2[i]))
      probs.lasso <- rbind(probs.lasso, game(lambda1.lasso[i], lambda2.lasso[i]))
      probs.lasso.strict <- rbind(probs.lasso.strict, game(lambda1.lasso.strict[i], lambda2.lasso.strict[i]))
    }
    Tab.rf <- data.frame(tag1 = d.test.rf$Team[1:64], win1=probs.rf$win1, draw=probs.rf$draw, 
                         win2 = probs.rf$win2, tag2=d.test.rf$Team[65:128])
    Tab.lasso <- data.frame(tag1 = d.test.rf$Team[1:64], win1=probs.lasso$win1, draw=probs.lasso$draw, 
                            win2 = probs.lasso$win2, tag2=d.test.rf$Team[65:128])  
    Tab.lasso.strict <- data.frame(tag1 = d.test$Team[1:64], win1=probs.lasso.strict$win1, draw=probs.lasso.strict$draw, 
                                   win2 = probs.lasso.strict$win2, tag2=d.test$Team[65:128])
    
    rps.erg.rf <- rps(Tab.rf, d.test)
    rps.erg.lasso <- rps(Tab.lasso, d.test)
    rps.erg.lasso.strict <- rps(Tab.lasso.strict, d.test)
    ##classification
    prog.rf <- max.col(Tab.rf[,2:4])
    prog.lasso <- max.col(Tab.lasso[,2:4])
    prog.lasso.strict <- max.col(Tab.lasso.strict[,2:4])
    truerate.rf <- length(prog.rf[prog.rf==rps.erg.rf[[2]]])/dim(d.test)[1]
    truerate.lasso <- length(prog.lasso[prog.lasso==rps.erg.lasso[[2]]])/dim(d.test)[1]
    truerate.lasso.strict <- length(prog.lasso.strict[prog.lasso.strict==rps.erg.lasso.strict[[2]]])/dim(d.test)[1]
    
    #cat("Likelihood", mean(rps.erg[[3]]), "Class. Rate", truerate,"%", "RPS", mean(rps.erg[[1]]), "\n")
    truerate.rf.k <- c(truerate.rf.k, truerate.rf)
    truerate.lasso.k <- c(truerate.lasso.k, truerate.lasso)
    truerate.lasso.strict.k <- c(truerate.lasso.strict.k, truerate.lasso.strict)
    rps.rf.k <- c(rps.rf.k,mean(rps.erg.rf[[1]]))
    rps.lasso.k <- c(rps.lasso.k, mean(rps.erg.lasso[[1]]))
    rps.lasso.strict.k <- c(rps.lasso.strict.k, mean(rps.erg.lasso.strict[[1]]))
    likelihood.rf.k <- c(likelihood.rf.k, mean(rps.erg.rf[[3]]))
    likelihood.lasso.k <- c(likelihood.lasso.k, mean(rps.erg.lasso[[3]]))
    likelihood.lasso.strict.k <- c(likelihood.lasso.strict.k, mean(rps.erg.lasso.strict[[3]]))
  }
  erg.rf <- cbind(likelihood.rf.k,truerate.rf.k, rps.rf.k)
  erg.lasso <- cbind(likelihood.lasso.k, truerate.lasso.k, rps.lasso.k)
  erg.lasso.strict <- cbind(likelihood.lasso.strict.k, truerate.lasso.strict.k, rps.lasso.strict.k)
  dimnames(erg.rf)[[2]] <- c("Likelihood","Class. Rate", "RPS")
  dimnames(erg.lasso)[[2]] <- c("Likelihood","Class. Rate", "RPS")
  dimnames(erg.lasso.strict)[[2]] <- c("Likelihood","Class. Rate", "RPS")
  erg.rf <- colMeans(erg.rf)
  erg.lasso <- colMeans(erg.lasso)
  erg.lasso.strict <- colMeans(erg.lasso.strict)
  rbind(erg.rf, erg.lasso, erg.lasso.strict)
}

andreas(dat, eqlist)


### Calculate betting results and compare them ####

bet <- read.csv2("C:/Users/vanderwurp/Desktop/Arbeit/WM_Modellierungen/Paper/Daten/betting results.csv", header=F)

## get predictions
datpre2018 <- dat[dat$WM!="2018",]
dat2018 <- dat[dat$WM=="2018",]

fit <- gjrm2(eqlist, data=datpre2018, BivD="T", Model="B", margins=c("PO","PO"))
fitalt <- gjrm(eqlist, data=datpre2018, BivD="T", Model="B", margins=c("PO","PO"))
pred1 <- exp(predict(fit, newdata=dat2018, eq=1))
pred2 <- exp(predict(fit, newdata=dat2018, eq=2))
pred1alt <-  exp(predict(fitalt, newdata=dat2018, eq=1))
pred2alt <- exp(predict(fitalt, newdata=dat2018, eq=1))

gamepred <- c()
for (i in 1:64) {
  gamepred <- rbind(gamepred, gameCopula(pred1[i], pred2[i], Copula="T", CopParam=as.numeric(summary(fit)$theta), method="p"))
}
gamepred <- data.frame(tag1 = dat2018$Team, win1 = gamepred$win1, 
                            draw = gamepred$draw, 
                            win2 = gamepred$win2, tag2=dat2018$Opponent)

expected <- gamepred[, 2:4] * bet[,3:5]

results <- data.frame(tag1 = dat2018$Team, win1 = expected$win1,
                      draw = expected$draw, win2 = expected$win2,
                      tag2 = dat2018$Opponent)

## these are the bets we take with 1 euro/dollar whatever and each's quota
quotes <- (results[,2:4] > 1) * bet[,3:5]

## true results

wins1 <- dat2018$Goals > dat2018$Goals.oppo
draw <- dat2018$Goals == dat2018$Goals.oppo
wins2 <- dat2018$Goals < dat2018$Goals.oppo
trueresult <- data.frame(wins1 = wins1, draw = draw, wins2 = wins2)

outcome <- trueresult * quotes
sum(outcome) # 87.29 euro won
length(quotes[quotes>0]) # 103 euro paid (bets placed)

### Look at AIC results ###

load("Erg.rdata")
xtable(Erg[[1]]$AICpicks)
xtable(Erg[[5]]$AICpicks)

Copula rps llh class bet MSE sum
C0   4   7    12   6   4  33
N   9   9     8   1   7  34
"T"   3   1    19  10   1  34
AMH   1   3     8  17   6  35
G180   5   5    17   6   3  36
"F"   2   2    12  11  10  37
J180   8   8    15   6   2  39
G0  10  10    12   5   5  42
J0  11  11     7   9   9  47
C270  13  14     4   4  15  50
PL   6   4    15  17   8  50
C180  12  12     3  15  11  53
C90  19  19     4   2  14  58
indep  16  13     2  14  13  58
FGM   7   6    17  17  12  59
G90  18  17     8   3  17  63
G270  15  16     1  16  16  64
J90  14  15     8  12  19  68
J270  17  18     4  12  18  69

