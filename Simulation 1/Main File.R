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
SingleSim("N", beta=list(c(0.25,0.15, -0.2), c(-0.1, 0, 0.1)), n=500, theta=0)
DoSim("N", truebeta=list(runif(4,-1,1),runif(4,-1,1)), truetheta = -0.5, 250, 10)

## with example from above:
V <- gen(Cop="G", beta=list(c(0.25,0.15, -0.2), c(-0.1, 0, 0.1)), n = 50000, theta=1.5,
         seed=sample(2^31,1))
plot(table(V[,1], V[,2]))



#### Simulation in Chapter 2 ####
set.seed(1904)
A <- DoAllSims(250, times=100, tau=0.1, truebeta = list(c(0.5, 0.2, -0.2, 0), c(0.2, -0.1, 0.1, 0.5)))
save(A, file = "Chap2_Sim_erg_01.rData")

## Table 2.1
A$AICpicks[c(1:3,5,4), c(1:4, 7, 5, 6)]
## Figure 2.1
pdf("Pen1Sim01.pdf", width = 8, height = 4)
ggplot(A$MSEErg, aes(x=true, y=MSE, fill=fit)) + geom_boxplot() + scale_fill_grey(start = 0.3, end = 0.95)
dev.off()

set.seed(1904)
A <- DoAllSims(250, times=100, tau=0.7, truebeta = list(c(0.5, 0.2, -0.2, 0), c(0.2, -0.1, 0.1, 0.5)))
save(A, file = "Chap2_Sim_erg_07.rData")
## Table 2.2
A$AICpicks[c(1:3,5,4), c(1:4, 7, 5, 6)]
## Figure 2.2
pdf("Pen1Sim07.pdf", width = 8, height = 4)
ggplot(A$MSEErg, aes(x=true, y=MSE, fill=fit)) + geom_boxplot() + scale_fill_grey(start = 0.3, end = 0.95)
dev.off()

set.seed(1904)
A <- DoAllSims(250, times=100, tau= -0.6, truebeta = list(c(0.5, 0.2, -0.2, 0), c(0.2, -0.1, 0.1, 0.5)))
save(A, file = "Chap2_Sim_erg_m06.rData")
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

save(A, file="ErgSimEqual_09032023.rdata")

pdf("SimMarravsMe.pdf", width = 8, height = 4)
ggplot(A, aes(x=true.and.fit, y=MSE, fill=version)) + geom_boxplot() + 
  geom_vline(xintercept = 5.5, linetype = 2) + #ggtitle(bquote(paste(tau," = \u00B1", 0.25))) +
  scale_fill_grey(start = 0.5, end = 0.95)
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
