
Coplist <- c("F", "FGM", "N", "J0", "J90", "J180", "J270", "G0", "G90", "G180", "G270", 
             "C0", "C90", "C180", "C270", "AMH", "PL", "T", "indep")

library(parallel)
library(xtable)
source("PreScript.R")
source("Helpers.R")

## Figure 4.1
fitcop <- gjrm(formula = eqlist, data = dat, BivD = "N", margins = c("PO", "PO"), 
        Model = "B")
#pdf("../../Figures/Diagcheck.pdf", height = 5, width = 6)
post.check(fitcop, main = "Residuals in margin 1", main2 = "Residuals in margin 2")
#dev.off()


cl <- makeCluster(10)
clusterEvalQ(cl, source("PreScript.R"))
clusterEvalQ(cl, source("Helpers.R"))

res1 <- parLapply(cl = cl, X = Coplist[1:9], fun = myfun)
res1 <- do.call(rbind, res1)
res2 <- parLapply(cl = cl, X = Coplist[c(10:17, 19)], fun = myfun)
res2 <- do.call(rbind, res2)
res <- rbind(res1, res2)
stopCluster(cl)



print(xtable(res[,c(7,1:6)], digits = 3), include.rownames = FALSE)

RRPS <- rank(res$rps, ties.method = "min")
RLLH <- rank(-res$llh, ties.method = "min")
RCR <- rank(-res$cr, ties.method = "min")
RMSE <- rank(res$MSE, ties.method = "min")
Rgains <- rank(-res$bet, ties.method = "min")
Rges <- RRPS + RLLH + RCR + RMSE + Rgains

resn <- cbind(res, RRPS, RLLH, RCR, RMSE,  Rgains, Rges)
resn <- resn[order(resn$Rges),]

## Table 4.2
print(xtable(resn[,c(7,1:6, 8:13)], digits = c(0, 3, 3, 3, 3, 3, 2, 0, 0, 0, 0, 0, 0, 0)), 
      include.rownames = FALSE)

## fit F on all world cups

source("PreScript.R")

eq1 <- Goals ~ CL.players + UEFA.players + Nation.Coach + Age.Coach + Tenure.Coach + 
  Legionaires + max.teammates + sec.max.teammates + age.dev + Rank + GDP + 
  host + confed + continent + odds + Population + titleholder + Knockout
eq2 <- Goals.oppo ~ CL.players.oppo + UEFA.players.oppo + Nation.Coach.oppo + Age.Coach.oppo + 
  Tenure.Coach.oppo + Legionaires.oppo + max.teammates.oppo + sec.max.teammates.oppo + 
  age.dev.oppo + Rank.oppo + GDP.oppo + host.oppo + confed.oppo + continent.oppo  + 
  odds.oppo + Population.oppo + titleholder.oppo + Knockout
eq3 <- ~ 1
eqlist <- list(eq1, eq2, eq3)

fitJ90 <- gjrm(eqlist, data = dat, BivD = "J90", margins = c("PO", "PO"), Model = "biv")
## For Table 4.12:
print(xtable(cbind(coef(fitJ90)[1:22], coef(fitJ90)[23:44]), digits = 3))

fit <- gjrm(eqlist, data = dat, BivD = "F", margins = c("PO", "PO"), Model = "biv")
summary(fit)

## Figure 4.1, this time with F
post.check(fit)

## Table 4.3
print(xtable(cbind(coef(fit)[1:22], coef(fit)[23:44]), digits = 3))

## empiric correlation
cor(dat$Goals, dat$Goals.oppo, method = "kendall")

library(EUfootball)
cor(Matches$Goals90Home[Matches$League == "BPL"], Matches$Goals90Guest[Matches$League == "BPL"], method = "spearman")
ligen <- c("BPL", "BL", "LaLi", "Lig1", "SerA", "ED", "TurL")
sp <- sapply(ligen, function(liga) 
  cor(Matches$Goals90Home[Matches$League == liga], Matches$Goals90Guest[Matches$League == liga], method = "spearman", 
      use = "pairwise.complete.obs"))
bp <- sapply(ligen, function(liga) 
  cor(Matches$Goals90Home[Matches$League == liga], Matches$Goals90Guest[Matches$League == liga], method = "pearson", 
      use = "pairwise.complete.obs"))
ke <- sapply(ligen, function(liga) 
  cor(Matches$Goals90Home[Matches$League == liga], Matches$Goals90Guest[Matches$League == liga], method = "kendall", 
      use = "pairwise.complete.obs"))
cbind(sp, bp, ke)
