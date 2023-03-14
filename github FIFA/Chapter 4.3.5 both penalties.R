
Coplist <- c("F", "FGM", "N", "J0", "J90", "J180", "J270", "G0", "G90", "G180", "G270", 
             "C0", "C90", "C180", "C270", "AMH", "PL", "T", "indep")

library(parallel)
library(xtable)
source("PreScript.R")
source("Helpers.R")
source("../GJRM changes - 2023.R")
source("../gjrm lasso wrapper.R")


eq1 <- Goals ~ 1 + CL.players + UEFA.players + Nation.Coach + Age.Coach + Tenure.Coach +
  Legionaires + max.teammates + sec.max.teammates + age.dev + Rank + GDP + host + confed + 
  continent + odds + Population + Knockout + titleholder #+ elo
eq2 <- Goals.oppo ~ 1 + CL.players.oppo + UEFA.players.oppo + Nation.Coach.oppo +
  Age.Coach.oppo + Tenure.Coach.oppo + Legionaires.oppo + max.teammates.oppo + sec.max.teammates.oppo +
  age.dev.oppo + Rank.oppo + GDP.oppo + host.oppo + confed.oppo + continent.oppo + odds.oppo + 
  Population.oppo + Knockout + titleholder.oppo #+ elo.oppo
eq3 <- ~ 1
eqlist <- list(eq1, eq2)

m1 <- model.matrix(eq1, data = dat)
m2 <- model.matrix(eq2, data = dat)

eq1 <- Goals ~ 1 + CL.players + UEFA.players + Nation.Coach1 + Age.Coach + Tenure.Coach +
  Legionaires + max.teammates + sec.max.teammates + age.dev + Rank + GDP + host1 + confedCAF + confedCONCACAF + confedCONMEBOL +
  confedUEFA + 
  continent1 + odds + Population + KnockoutTRUE + titleholder
eq2 <- Goals.oppo ~ 1 + CL.players.oppo + UEFA.players.oppo + Nation.Coach.oppo1 +
  Age.Coach.oppo + Tenure.Coach.oppo + Legionaires.oppo + max.teammates.oppo + sec.max.teammates.oppo +
  age.dev.oppo + Rank.oppo + GDP.oppo + host.oppo1 + confed.oppoCAF + confed.oppoCONCACAF + confed.oppoCONMEBOL + 
  confed.oppoUEFA + 
  continent.oppo1 + odds.oppo + 
  Population.oppo + KnockoutTRUE + titleholder.oppo
eq3 <- ~ 1
eqlist <- list(eq1, eq2, eq3)

dat.n <- data.frame(Goals = dat$Goals, Goals.oppo = dat$Goals.oppo, m1, m2, WM = as.character(dat$WM),
                    Team = dat$Team, Opponent = dat$Opponent)


cl <- makeCluster(10)
clusterEvalQ(cl, source("PreScript.R"))
clusterEvalQ(cl, source("Helpers.R"))
clusterEvalQ(cl, source("gjrm lasso wrapper.R"))
clusterEvalQ(cl, source("GJRM changes.R"))
clusterExport(cl, list("dat.n", "eqlist"))

res1 <- parLapply(cl = cl, X = Coplist[1:9], fun = myfun.penboth, dat = dat.n)
res1 <- do.call(rbind, res1)
res2 <- parLapply(cl = cl, X = Coplist[c(10:17)], fun = myfun.penboth, dat = dat.n) #18 und ohne indep!
res2 <- do.call(rbind, res2)
res3 <- myfun.pen2("indep", dat.n)
res <- rbind(res1, res2, res3)
stopCluster(cl)

RRPS <- rank(res$rps, ties.method = "min")
RLLH <- rank(-res$llh, ties.method = "min")
RCR <- rank(-res$cr, ties.method = "min")
RMSE <- rank(res$MSE, ties.method = "min")
Rgains <- rank(-res$bet, ties.method = "min")
Rges <- RRPS + RLLH + RCR + RMSE + Rgains

resn <- cbind(res, RRPS, RLLH, RCR, RMSE,  Rgains, Rges)
resn <- resn[order(resn$Rges),]

print(xtable(resn[,c(7,1:5, 8:13)], digits = c(0, 3, 3, 3, 3, 3, 2, 0, 0, 0, 0, 0, 0)), 
      include.rownames = FALSE)


fitnocarry <- gjrm.lasso(data = list(dat.n, eqlist), Cop = "F", plot = TRUE,
                  grid.l = 100, K = 10, CV = FALSE, threshold = 0.01,
                  carry.start.values = FALSE, LASSO.groups = list(c(14:17)),
                  linear.equal = rep(TRUE, 22), xi = 1e9, start.nu = 6)
fitcarry <- gjrm.lasso(data = list(dat.n, eqlist), Cop = "F", plot = TRUE,
                         grid.l = 100, K = 10, CV = FALSE, threshold = 0.01,
                         carry.start.values = TRUE, LASSO.groups = list(c(14:17)),
                         linear.equal = rep(TRUE, 22), xi = 1e9, start.nu = 6)

## Figure 4.3
pdf("path2.pdf", height = 10, width = 10)
par(mfrow=c(2,2))
pathplot(fitcarry)
pathplot(fitnocarry)
dev.off()

fit <- gjrm.lasso(data = list(dat.n, eqlist), Cop = "PL", plot = TRUE,
                  grid.l = 100, K = 10, CV = TRUE, threshold = 0.01,
                  carry.start.values = TRUE, LASSO.groups = list(c(14:17)),
                  linear.equal = rep(TRUE, 22), xi = 1e9, start.nu = 1)
summary(fit$fit.exLLH)

## Figure 4.4
pdf("path_comb.pdf", height = 6, width = 10)
pathplot(fit)
dev.off()


## Table 4.9
x <- round(cbind(coef(fit$fit.exLLH)[1:22], coef(fit$fit.exLLH)[23:44]), digits = 6)
xtable(cbind(x[1:11,], rownames(x[12:22]), x[12:22,]), digits = 3)




