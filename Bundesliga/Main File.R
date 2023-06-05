# Daten laden
#setwd("E:/Uni Arbeit/WM_Modellierungen/Bundesliga Prognose")
source("BuildData.R")
source("Functions.R")
rm(erg, erg2, ergG, ergH, i, indG, indH, Spiele, Verein)

## Pakete
library(GJRM) # use version 0.2-3
#source("GJRM changes (new 2020).R")
source("../GJRM changes - 2023.R")

library(xtable)
library(gridExtra)
dat <- dat[,-1]

## all formulas:

eq1p <- Score90Home ~ 1 + PreTabelleHome + PreTabelleGuest+ AufsteigerHome + 
  AufsteigerGuest+ MWHome + MWGuest + pHome + pGuest 
eq2p <- Score90Guest ~ 1 + PreTabelleGuest + PreTabelleHome + AufsteigerGuest + 
  AufsteigerHome + MWGuest + MWHome + pGuest + pHome
eq3p <- ~ 1
eqlist1to3 <- list(eq1p, eq2p, eq3p)

eq1p <- Score90Home ~ 1 + PreTabelleHome + PreTabelleGuest+ AufsteigerHome + 
  AufsteigerGuest+ MWHome + MWGuest + pHome + pGuest + TabelleHome + TabelleGuest +
  FormHome + FormGuest
eq2p <- Score90Guest ~ 1 + PreTabelleGuest + PreTabelleHome + AufsteigerGuest + 
  AufsteigerHome + MWGuest + MWHome + pGuest + pHome + TabelleGuest + TabelleHome +
  FormGuest + FormHome
eq3p <- ~ 1
eqlistfull <- list(eq1p, eq2p, eq3p)

eq1p <- Score90Home ~ 1 + PreTabelleHome + PreTabelleGuest+ AufsteigerHome + 
  AufsteigerGuest+ MWHome + MWGuest + pHome + pGuest + TabelleHome + TabelleGuest +
  FormHome + FormGuest + coronaHome + MWHomeCorona
eq2p <- Score90Guest ~ 1 + PreTabelleGuest + PreTabelleHome + AufsteigerGuest + 
  AufsteigerHome + MWGuest + MWHome + pGuest + pHome + TabelleGuest + TabelleHome +
  FormGuest + FormHome + coronaGuest + MWGuestCorona
eq3p <- ~ 1
eqlistcovid <- list(eq1p, eq2p, eq3p)

##Corona-Variable:
dat$coronaHome <- 0
dat$coronaGuest <- 0
dat$MWHomeCorona <- 0
dat$MWGuestCorona <- 0

dat$postponed <- 0
dat$tipWin <- NA
dat$tipDraw <- NA
dat$topLose <- NA
dat$tip_12 <- NA
dat$tip_1X <- NA
dat$tip_X2 <- NA

## neue Spieltage ergaenzen: (Spieltagsnummern nacheinander rotieren)
## ohne Corona:
for (i in 1:25) {
  #neu <- read.csv2("Spieltagsdaten/Spieltag6.csv")
  neu <- read.csv2(paste0("Spieltagsdaten/Spieltag",i,".csv"))
  sMW <- sum(c(neu$MarktwertHome, neu$MarktwertGuest))
  neu$MWHome <- neu$MarktwertHome / sMW
  neu$MWGuest <- neu$MarktwertGuest / sMW
  neu$AufsteigerHome <- as.logical(neu$AufsteigerHome)
  neu$AufsteigerGuest <- as.logical(neu$AufsteigerGuest)
  if(i < 20){
    neu$tip_12 <- NA
    neu$tip_1X <- NA
    neu$tip_X2 <- NA
  }

  neu$coronaHome <- 0
  neu$coronaGuest <- 0
  neu$MWHomeCorona <- 0
  neu$MWGuestCorona <- 0
  neu <- neu[,dimnames(dat)[[2]]]
  dat <- rbind(dat, neu)
}



for (i in 26:34) {
  #neu <- read.csv2("Spieltagsdaten/Spieltag6.csv")
  neu <- read.csv2(paste0("Spieltagsdaten/Spieltag",i,".csv"))
  sMW <- sum(c(neu$MarktwertHome, neu$MarktwertGuest))
  neu$MWHome <- neu$MarktwertHome / sMW
  neu$MWGuest <- neu$MarktwertGuest / sMW
  neu$AufsteigerHome <- as.logical(neu$AufsteigerHome)
  neu$AufsteigerGuest <- as.logical(neu$AufsteigerGuest)
  neu$coronaHome <- 1
  neu$coronaGuest <- -1
  neu$MWHomeCorona <- abs(neu$MWHome * neu$coronaHome)
  neu$MWGuestCorona <- abs(neu$MWGuest * neu$coronaGuest)
  neu <- neu[,dimnames(dat)[[2]]]

  dat <- rbind(dat, neu)
}

# Spieltag der Saison 20/21 laden:

for (i in 1:34) {
  neu <- read.csv2(paste0("Spieltagsdaten/Saison 20-21/Spieltag",i,".csv"))
  sMW <- sum(c(neu$MarktwertHome, neu$MarktwertGuest))
  neu$MWHome <- neu$MarktwertHome / sMW
  neu$MWGuest <- neu$MarktwertGuest / sMW
  neu$AufsteigerHome <- as.logical(neu$AufsteigerHome)
  neu$AufsteigerGuest <- as.logical(neu$AufsteigerGuest)

  if(i < 6){
    neu$coronaHome <- 0.5
    neu$coronaGuest <- -0.5
  }
  if(i >= 6){
    neu$coronaHome <- 1
    neu$coronaGuest <- -1
  }
 
  neu$MWHomeCorona <- abs(neu$MWHome * neu$coronaHome)
  neu$MWGuestCorona <- abs(neu$MWGuest * neu$coronaGuest)
  neu$postponed <- 0
  neu <- neu[,dimnames(dat)[[2]]]
  dat <- rbind(dat, neu)
}


dat$OddsDraw <- dat$OddsDraw / 100
dat$OddsGuest <- dat$OddsGuest / 100
dat$OddsHome <- dat$OddsHome / 100

p <- 1/ dat$OddsHome + 1 / dat$OddsDraw + 1 / dat$OddsGuest

## bereinigen um die Marge ist n?tig, da Marge nicht konstant.

dat$pHome <- 1 / (p * dat$OddsHome)
dat$pGuest <- 1  / (p * dat$OddsGuest)
dat$pDraw <- 1 / (p * dat$OddsDraw)

## Kovariablen Tabelle und Form machen erst ab dem 4. Spieltag Sinn. 
datBU <- dat


################################################################################
############################ Predictions #######################################
################################################################################



## Saison 19/20
## Note: matchday 20: Added 1X, 12, X2. Prior only bets on 1, X, 2.

## $postponed = helping variable, to identify cancelled/postponed matches.

## Note: Iterative = FALSE is definitely an option here and decreases runtime
## immensely. Results only slightly changed.
## Computation can be skipped. load results.rData instead, see below save.image(). 

#train <- dat[dat$SeasonFrom < 2019,]
#test.global <- dat[dat$SeasonFrom >= 2019,]

## Calculations can be skipped, results file provided. See below.

## Tables 2.5 and 2.6

res1 <- lapply(1:3, function(i) fun19(i, eqlist1to3, data = dat, true.count = 8, iterative = TRUE))
## pandemic from 26 onward, but learning till 29. 26-29 with averaged intercepts.
## included in fun19.
res2 <- lapply(4:29, function(i) {print(i); fun19(i, eqlistfull, data = dat, true.count = 12, iterative = TRUE)})
res3 <- lapply(30:34, function(i) {print(i); fun19(i, eqlistcovid, data = dat, true.count = 14, iterative = TRUE)})

bets1 <- rbind(res1[[1]]$bets.types, res1[[2]]$bets.types, res1[[3]]$bets.types)
bets2 <- NULL
for(i in 1:26){
  bets2 <- rbind(bets2, res2[[i]]$bets.types)
}
bets3 <- NULL
for(i in 1:5){
  bets3 <- rbind(bets3, res3[[i]]$bets.types)
}

### Saison 2020/2021
## matchdays 1-3 without covid, so eqlist1to3.
## 4-5 with everything, so eqlistfull.
## from 6 on eqlistcovid again.

res2.1 <- lapply(1:3, function(i) fun20(i, eqlist1to3, data = dat, true.count = 8, iterative = TRUE))
res2.2 <- lapply(4:5, function(i) fun20(i, eqlistfull, data = dat, true.count = 12, iterative = TRUE))
res2.3 <- lapply(6:34, function(i) fun20(i, eqlistcovid, data = dat, true.count = 14, iterative = TRUE))

bets2.1 <- rbind(res2.1[[1]]$bets.types, res2.1[[2]]$bets.types, res2.1[[3]]$bets.types)
bets2.2 <- NULL
for(i in 1:2){
  bets2.2 <- rbind(bets2.2, res2.2[[i]]$bets.types)
}
bets2.3 <- NULL
for(i in 1:29){
  bets2.3 <- rbind(bets2.3, res2.3[[i]]$bets.types)
}
#save.image("results.rData")

# load("results.rData")


## number of bets
sum(bets1) + sum(bets2) + sum(bets3)
sum(bets2.1) + sum(bets2.2) + sum(bets2.3)

## distribution of bets
colSums(rbind(bets1, bets2)[1:19,]) ## before 12, 1X, X2 were added.
colSums(rbind(bets1, bets2)[20:25,]) ## before pandemic
colSums(rbind(bets1, bets2)[26:29,]) ## start pandemic, averaging intercepts
colSums(rbind(bets1, bets2, bets3)[30:34,]) # rest of season, corona-variable
colSums(rbind(bets1, bets2, bets3))
colSums(rbind(bets2.1, bets2.2, bets2.3))

#fun19(3, eqlist1to3, data = dat, true.count = 8)

## observed gains (out of res1, res2, res3, res2.1, res2.2, res2.3)

gains <- c()
gains.kelly <- c()
for(i in 1:3){
  gains <- c(gains, res1[[i]]$gains)
  gains.kelly <- c(gains.kelly, res1[[i]]$gains.kelly)
}
for(i in 1:26){
  gains <- c(gains, res2[[i]]$gains)
  gains.kelly <- c(gains.kelly, res2[[i]]$gains.kelly)
}
for(i in 1:5){
  gains <- c(gains, res3[[i]]$gains)
  gains.kelly <- c(gains.kelly, res3[[i]]$gains.kelly)
}
  
sum(gains[1:19])
sum(gains[1:19]) / 99
sum(gains.kelly[1:19])
sum(gains.kelly[1:19]) / 99
sum(gains[20:25])
sum(gains[20:25]) / 36
sum(gains.kelly[20:25])
sum(gains.kelly[20:25]) / 36
sum(gains[26:29])
sum(gains[26:29]) / 29
sum(gains.kelly[26:29])
sum(gains.kelly[26:29]) / 29
sum(gains[30:34])
sum(gains[30:34]) / 45
sum(gains.kelly[30:34])
sum(gains.kelly[30:34]) / 45
sum(gains)
sum(gains.kelly)
sum(gains) / 209
sum(gains.kelly)/209
################################################
for(i in 1:3){
  gains <- c(gains, res2.1[[i]]$gains)
  gains.kelly <- c(gains.kelly, res2.1[[i]]$gains.kelly)
}
for(i in 1:2){
  gains <- c(gains, res2.2[[i]]$gains)
  gains.kelly <- c(gains.kelly, res2.2[[i]]$gains.kelly)
}
for(i in 1:29){
  gains <- c(gains, res2.3[[i]]$gains)
  gains.kelly <- c(gains.kelly, res2.3[[i]]$gains.kelly)
}
sum(gains[35:68]) / 252
sum(gains.kelly[35:68]) / 252

#fun20(20, eqlist = eqlistcovid, data = dat, true.count = 14, iterative = FALSE)

## final model
eqlist <- eqlist1to3
fit1 <- gjrm(eqlist, data=dat, BivD = "F", Model = "B", margins = c("PO","PO"),
            linear.equal = c(FALSE, rep(TRUE, 8)), iterative = FALSE)
## Table 2.7
xtable(cbind(coef(fit1)[1:9], coef(fit1)[10:18],
             exp(coef(fit1)[1:9]), exp(coef(fit1)[10:18])), digits = 4)
eqlist <- eqlistfull
fit2 <- gjrm(eqlist, data=dat, BivD = "F", Model = "B", margins = c("PO","PO"),
             linear.equal = c(FALSE, rep(TRUE, 12)))
## Table 2.8
xtable(cbind(coef(fit2)[1:13], coef(fit2)[14:26],
             exp(coef(fit2)[1:13]), exp(coef(fit2)[14:26])), digits = 4)

eqlist <- eqlistcovid
## Table 2.9
fit3 <- gjrm(eqlist, data=dat, BivD = "F", Model = "B", margins = c("PO","PO"),
             linear.equal = c(FALSE, rep(TRUE, 14)))
xtable(cbind(coef(fit3)[1:15], coef(fit3)[16:30],
             exp(coef(fit3)[1:15]), exp(coef(fit3)[16:30])), digits = 4)
