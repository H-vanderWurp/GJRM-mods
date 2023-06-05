Coplist <- c("F", "FGM", "N", "J0", "J90", "J180", "J270", "G0", "G90", "G180", "G270", 
             "C0", "C90", "C180", "C270", "AMH", "PL", "T", "indep")

library(parallel)
library(xtable)
source("PreScript.R")
source("Helpers.R")

source("../GJRM changes - 2023.R")

#myfun.pen1("F")
cl <- makeCluster(10)
clusterEvalQ(cl, source("PreScript.R"))
clusterEvalQ(cl, source("Helpers.R"))
clusterEvalQ(cl, source("../GJRM changes - 2023.R"))

res1 <- parLapply(cl = cl, X = Coplist[1:9], fun = myfun.pen1)
res1 <- do.call(rbind, res1)
res2 <- parLapply(cl = cl, X = Coplist[c(10:17, 19)], fun = myfun.pen1)
res2 <- do.call(rbind, res2)
res <- rbind(res1, res2)
stopCluster(cl)

RRPS <- rank(res$rps, ties.method = "min")
RLLH <- rank(-res$llh, ties.method = "min")
RCR <- rank(-res$cr, ties.method = "min")
RMSE <- rank(res$MSE, ties.method = "min")
Rgains <- rank(-res$bet, ties.method = "min")
Rges <- RRPS + RLLH + RCR + RMSE + Rgains

resn <- cbind(res, RRPS, RLLH, RCR, RMSE,  Rgains, Rges)
resn <- resn[order(resn$Rges),]

## Table 4.4
print(xtable(resn[,c(7,1:5, 8:13)], digits = c(0, 3, 3, 3, 3, 3, 2, 0, 0, 0, 0, 0, 0)), 
      include.rownames = FALSE)

####################################################################################

#eq1 <- Goals ~ 1 + CL.players + UEFA.players + Nation.Coach + Age.Coach + Tenure.Coach +
#  Legionaires + max.teammates + sec.max.teammates + age + Rank + GDP + host + confed + 
#  continent + odds + Population + Knockout + titleholder #+ elo
#eq2 <- Goals.oppo ~ 1 + CL.players.oppo + UEFA.players.oppo + Nation.Coach.oppo +
#  Age.Coach.oppo + Tenure.Coach.oppo + Legionaires.oppo + max.teammates.oppo + sec.max.teammates.oppo +
#  age.oppo + Rank.oppo + GDP.oppo + host.oppo + confed.oppo + continent.oppo + odds.oppo + 
#  Population.oppo + Knockout + titleholder.oppo #+ elo.oppo
#eq3 <- ~ 1
#eqlist <- list(eq1, eq2)

## re-fit equal, PL, all data

fit <- gjrm(formula = eqlist, data = dat, Model = "B", margins = c("PO", "PO"),
            linear.equal = rep(TRUE, 22), xi = 1e9, BivD = "PL")
summary(fit)
## Table 4.5
xtable(round(cbind(coef(fit)[1:22], coef(fit)[23:44]), digits = 3), digits = 3)
