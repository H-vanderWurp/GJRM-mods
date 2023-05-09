## abc bis hier. Rest prüfen + Tabellen A.1, A.2, A.3 im Anhang

## Table 5.5, simple gjrm fit
library("EUfootball")
library("GJRM")
source("../GJRM changes - 2023.R")
Dat <- Matches
Dat <- Dat[Dat$date < as.Date("2020-03-01"),]
Dat.sub <- Dat[Dat$League == "BPL",]
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
fitcop <- gjrm(formula = list(eq1, eq2, eq3), data = Dat.sub,
               BivD = "FGM", margins = c("PO", "PO"), Model = "B")
a <- summary(fitcop)$tableP1 
b <- summary(fitcop)$tableP2
X <- cbind(coef(fitcop)[1:15], a[,2], coef(fitcop)[16:30], b[,2])
colnames(X) <- c("beta1", "SE(beta1)", "beta2", "SE(beta2)")
round(X, 4)  ## Table 5.5

## Table 5.6 gjrm fit with lasso penalty
source("../../gjrm lasso wrapper.R")
## careful, may take a while, even without CV.
fitcop2 <- gjrm.lasso(data = list(Dat.sub, list(eq1,eq2,eq3)), grid.l = 100,
                      K = 10, linear.equal = NULL, LASSO.groups = NULL, CV = FALSE,
                      max.nu = NULL, threshold = 1e-03, CV.seed = 1904, xi = 1e9,
                      carry.start.values = TRUE, start.nu = 400, Cop = "F",
                      plot = TRUE)

fitcop3 <- gjrm.lasso(data = list(Dat.sub, list(eq1,eq2,eq3)), grid.l = 100,
                      K = 10, linear.equal = c(FALSE, rep(TRUE, 14)), LASSO.groups = NULL, CV = FALSE,
                      max.nu = NULL, threshold = 1e-03, CV.seed = 1904, xi = 1e9,
                      carry.start.values = TRUE, start.nu = 500, Cop = "F",
                      plot = TRUE)

X <- cbind(coef(fitcop2$fit.aic)[1:15],
           coef(fitcop2$fit.aic)[16:30], 
           coef(fitcop3$fit.aic)[1:15], 
           coef(fitcop3$fit.aic)[16:30])
colnames(X) <- c("beta1", "beta2", "beta1", "beta2")
round(X, 4)  ## Table 5.6

datBPL <- Dat[Dat$League == "BPL",]
datBL <- Dat[Dat$League == "BL",]
datLaLi <- Dat[Dat$League == "LaLi",]
datLig1 <- Dat[Dat$League == "Lig1",]
datSerA <- Dat[Dat$League == "SerA",]
datED <- Dat[Dat$League == "ED",]
datTurL <- Dat[Dat$League == "TurL",]

fitBPL <- gjrm.lasso(data = list(datBPL, list(eq1,eq2,eq3)), grid.l = 100,
                     K = 10, linear.equal = c(FALSE, rep(TRUE, 14)), LASSO.groups = NULL, CV = FALSE,
                     max.nu = NULL, threshold = 1e-03, CV.seed = 1904, xi = 1e9,
                     carry.start.values = TRUE, start.nu = 10, Cop = "F",
                     plot = TRUE)
fitBL <- gjrm.lasso(data = list(datBL, list(eq1,eq2,eq3)), grid.l = 100,
                    K = 10, linear.equal = c(FALSE, rep(TRUE, 14)), LASSO.groups = NULL, CV = FALSE,
                    max.nu = NULL, threshold = 1e-03, CV.seed = 1904, xi = 1e9,
                    carry.start.values = TRUE, start.nu = 10, Cop = "F",
                    plot = TRUE)
fitLaLi <- gjrm.lasso(data = list(datLaLi, list(eq1,eq2,eq3)), grid.l = 100,
                      K = 10, linear.equal = c(FALSE, rep(TRUE, 14)), LASSO.groups = NULL, CV = FALSE,
                      max.nu = NULL, threshold = 1e-03, CV.seed = 1904, xi = 1e9,
                      carry.start.values = TRUE, start.nu = 10, Cop = "F",
                      plot = TRUE)
fitLig1 <- gjrm.lasso(data = list(datLig1, list(eq1,eq2,eq3)), grid.l = 100,
                      K = 10, linear.equal = c(FALSE, rep(TRUE, 14)), LASSO.groups = NULL, CV = FALSE,
                      max.nu = NULL, threshold = 1e-03, CV.seed = 1904, xi = 1e9,
                      carry.start.values = TRUE, start.nu = 10, Cop = "F",
                      plot = TRUE)
fitSerA <- gjrm.lasso(data = list(datSerA, list(eq1,eq2,eq3)), grid.l = 100,
                      K = 10, linear.equal = c(FALSE, rep(TRUE, 14)), LASSO.groups = NULL, CV = FALSE,
                      max.nu = NULL, threshold = 1e-03, CV.seed = 1904, xi = 1e9,
                      carry.start.values = TRUE, start.nu = 10, Cop = "F",
                      plot = TRUE)
fitED <- gjrm.lasso(data = list(datED, list(eq1,eq2,eq3)), grid.l = 100,
                    K = 10, linear.equal = c(FALSE, rep(TRUE, 14)), LASSO.groups = NULL, CV = FALSE,
                    max.nu = NULL, threshold = 1e-03, CV.seed = 1904, xi = 1e9,
                    carry.start.values = TRUE, start.nu = 10, Cop = "F",
                    plot = TRUE)
fitTurL <- gjrm.lasso(data = list(datTurL, list(eq1,eq2,eq3)), grid.l = 100,
                      K = 10, linear.equal = c(FALSE, rep(TRUE, 14)), LASSO.groups = NULL, CV = FALSE,
                      max.nu = NULL, threshold = 1e-03, CV.seed = 1904, xi = 1e9,
                      carry.start.values = TRUE, start.nu = 10, Cop = "F",
                      plot = TRUE)


## Table 5.7 first part
round(cbind(coef(fitBPL$fit.aic)[1:15], coef(fitBPL$fit.aic)[16:30],
            coef(fitBL$fit.aic)[1:15], coef(fitBL$fit.aic)[16:30],
            coef(fitSerA$fit.aic)[1:15], coef(fitSerA$fit.aic)[16:30],
            coef(fitLaLi$fit.aic)[1:15], coef(fitLaLi$fit.aic)[16:30]), 2)

## Table 5.7 second part

round(cbind(coef(fitLig1$fit.aic)[1:15], coef(fitLig1$fit.aic)[16:30],
            coef(fitED$fit.aic)[1:15], coef(fitED$fit.aic)[16:30],
            coef(fitTurL$fit.aic)[1:15], coef(fitTurL$fit.aic)[16:30]), 2)

## kendall tau values
summary(fitBPL$fit.aic) # kendall 0.0329
summary(fitBL$fit.aic) # -0.016
summary(fitSerA$fit.aic) # 0.08
summary(fitLaLi$fit.aic) # 0.0451
summary(fitLig1$fit.aic) # 0.0209
summary(fitED$fit.aic) # -0.024
summary(fitTurL$fit.aic) # 0.0499

eqlist <- list(eq1, eq2, eq3)
## Table A.3
fitBPL <- gjrm(eqlist, data = datBPL, margins = c("PO", "PO"), BivD = "FGM", Model = "biv")
fitBL <- gjrm(eqlist, data = datBL, margins = c("PO", "PO"), BivD = "FGM", Model = "biv")
fitSerA <- gjrm(eqlist, data = datSerA, margins = c("PO", "PO"), BivD = "FGM", Model = "biv")
fitLaLi <- gjrm(eqlist, data = datLaLi, margins = c("PO", "PO"), BivD = "FGM", Model = "biv")
fitLig1 <- gjrm(eqlist, data = datLig1, margins = c("PO", "PO"), BivD = "FGM", Model = "biv")
fitED <- gjrm(eqlist, data = datED, margins = c("PO", "PO"), BivD = "FGM", Model = "biv")
fitTurL <- gjrm(eqlist, data = datTurL, margins = c("PO", "PO"), BivD = "FGM", Model = "biv")
summary(fitBPL)
summary(fitBL)
summary(fitSerA)
summary(fitLaLi)
summary(fitLig1)
summary(fitED)
summary(fitTurL)
