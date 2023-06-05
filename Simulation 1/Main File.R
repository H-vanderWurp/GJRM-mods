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
load(file = "Chap2_Sim_erg_01.rData")

## Table 2.1
A$AICpicks[c(1:3,5,4), c(1:4, 7, 5, 6)]
## Figure 2.1
pdf("Pen1Sim01.pdf", width = 8, height = 4)
ggplot(A$MSEErg, aes(x=true, y=MSE, fill=fit)) + geom_boxplot() + scale_fill_grey(start = 0.3, end = 0.95)
dev.off()

set.seed(1904)
A <- DoAllSims(250, times=100, tau=0.7, truebeta = list(c(0.5, 0.2, -0.2, 0), c(0.2, -0.1, 0.1, 0.5)))
save(A, file = "Chap2_Sim_erg_07.rData")
load(file = "Chap2_Sim_erg_07.rData")
## Table 2.2
A$AICpicks[c(1:3,5,4), c(1:4, 7, 5, 6)]
## Figure 2.2
pdf("Pen1Sim07.pdf", width = 8, height = 4)
ggplot(A$MSEErg, aes(x=true, y=MSE, fill=fit)) + geom_boxplot() + scale_fill_grey(start = 0.3, end = 0.95)
dev.off()

set.seed(1904)
A <- DoAllSims(250, times=100, tau= -0.6, truebeta = list(c(0.5, 0.2, -0.2, 0), c(0.2, -0.1, 0.1, 0.5)))
save(A, file = "Chap2_Sim_erg_m06.rData")
load(file = "Chap2_Sim_erg_m06.rData")
## Figure 2.3
pdf("Pen1Sim-06.pdf", width = 8, height = 4)
ggplot(A$MSEErg, aes(x=true, y=MSE, fill=fit)) + geom_boxplot() + scale_fill_grey(start = 0.3, end = 0.95)
dev.off()



######### Compare Penalty vs standard GJRM ##########

## dataset
set.seed(1904)
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

set.seed(1904)
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
load(file="ErgSimEqual_09032023.rdata")
## Figure 2.4
pdf("SimMarravsMe.pdf", width = 8, height = 4)
ggplot(A, aes(x=true.and.fit, y=MSE, fill=version)) + geom_boxplot() + 
  geom_vline(xintercept = 5.5, linetype = 2) + #ggtitle(bquote(paste(tau," = \u00B1", 0.25))) +
  scale_fill_grey(start = 0.5, end = 0.95)
dev.off()


