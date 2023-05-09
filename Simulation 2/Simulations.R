
library(GJRM)
source("../GJRM changes - 2023.R")
source("Misc functions.R")
source("../gjrm lasso wrapper.R")

## single Simulation example:

SingleSim(Cop = "F", theta = 2, seed = 1, n = 250, grid.l = 100, K = 10,
          threshold = 0.01)

## repeat this for copula classes and 100 each to reproduce simulation study.
## kendall() is helpful to calculate the values of theta for a given tau.


## Simulation performed on computational cluster of the statistics department
## in highly parallel setup.
## Each call created 2 results (lappy 1:2), using a different seed of date + n + 10*clusterID

erg <- lapply(1:2, function(n) SingleSim("C90", -0.2222222, 22092020 + n + 10*as.numeric(Sys.getenv("PBS_ARRAYID")), n = 250, 
                                         grid.l = 100, K = 10, threshold = 1e-02))
save(erg, file = paste0("C90.01.", Sys.getenv("PBS_ARRAYID"), ".Rdata"))

## To re-create everything, one would have to call the SingleSim above 100 times
## for each of the 32 settings (copula + theta). Parallelisation is recommended.

## theta values from:

sapply(c(-0.75, -0.5, -0.25, -0.1, 0.1, 0.25, 0.5, 0.75), function(t) kendall(t))




## Single Sim for visualisation purposes:

SingleSim(Cop = "N", theta = 0.1564322, seed = 19042023, n = 250, grid.l = 100, K = 10,
          threshold = 0.01)
data <- genData(Cop = "N", theta = 0.1564322 , seed = 19042023, n = 250)
fit <- gjrm.lasso(data, Cop = "N", grid.l = 100, K = 10, CV = TRUE, threshold = 0.01, plot = TRUE)

pdf("../../selection_paths.pdf", height = 9, width = 6)
par(mfrow = c(4,2))
plot(fit$grid, fit$exLLH, type = "l", xlab = expression(nu), ylab = "exLLH")
plot(1:100, rev(fit$exLLH), type = "l", xlab = expression("Index of" ~ nu), ylab = "exLLH")
plot(fit$grid, fit$aic, type = "l", xlab = expression(nu), ylab = "AIC")
plot(1:100, rev(fit$aic), type = "l", xlab = expression("Index of" ~ nu), ylab = "AIC")
plot(fit$grid, fit$bic, type = "l", xlab = expression(nu), ylab = "BIC")
plot(1:100, rev(fit$bic), type = "l", xlab = expression("Index of" ~ nu), ylab = "BIC")
plot(fit$grid, fit$dfs, type = "l", xlab = expression(nu), ylab = "dfs")
plot(1:100, rev(fit$dfs), type = "l", xlab = expression("Index of" ~ nu), ylab = "dfs")
dev.off()

## better in ggplot:
library(ggplot2)

x_labels <- c(expression(nu), expression("Index of" ~ nu))
labs <- data.frame(axis_title = x_labels, )

ggdat <- data.frame(target = c((fit$exLLH), (fit$exLLH), (fit$aic), (fit$aic), 
                               (fit$bic), (fit$bic), (fit$dfs), (fit$dfs)),
                    nu = rep(c(fit$grid, 100:1), 4),
                    ploty = rep(c("exLLH", "exLLH", "AIC", "AIC", "BIC", "BIC", "dfs", "dfs"), each = 100),
                    plotx = rep(c("nu", "Index~of~nu", "nu", "Index~of~nu", "nu", "Index~of~nu", "nu", "Index~of~nu"), each = 100))
ggdat$plotx <- factor(ggdat$plotx, levels = c("nu", "Index~of~nu"))

pdf("../../selection_paths.pdf", height = 7, width = 7)
ggplot(data = ggdat, aes(y = target, x = nu)) + geom_line() + facet_grid(ploty ~ plotx, scales = "free", switch = "both", labeller = label_parsed) +
  theme(strip.placement = "outside", strip.background = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())
dev.off()

## Pathplot als gg-Version
library(gridExtra)

## update with colors

pathplot <- function(fit, ...){
  par(mfrow = c(1,2))
  m <- fit$m
  grid.l <- dim(m)[2]
  p <- dim(m)[1]
  minim <- min(m[-c(1, (p/2)+1),])
  maxim <- max(m[-c(1, (p/2)+1),])
  plot(fit$grid, m[1,], type = "n", ylim = c(minim, maxim), xlab = expression(nu),
       ylab = expression(beta["i"]), ...)
  for(i in c(2:(p/2), ((p/2)+2):p)){
    col <- "gray"
    if(i %in% c(2:5, 27:30))
      col <- "black"
    points(fit$grid, m[i,], type = "l", col = col)
  }
  abline(v = fit$nus[1], lty = 2)
  #text(x = fit$nus[1] - max(fit$grid)*0.03, y = max(m)*0.9, labels = "BIC")
  abline(v = fit$nus[2], lty = 2)
  #text(x = fit$nus[2] - max(fit$grid)*0.03, y = max(m)*0.9, labels = "AIC")
  abline(v = fit$nus[3], lty = 2)
  #text(x = fit$nus[3] + max(fit$grid)*0.04, y = max(m)*0.9, labels = "exLLH")
  
  plot(1:grid.l, rev(m[1,]), type = "n", ylim = c(minim, maxim), xlab = expression("Index of"~~ nu),
       ylab = expression(beta["i"]), ...)
  for(i in c(2:(p/2), ((p/2)+2):p)){
    col <- "gray"
    if(i %in% c(2:5, 27:30))
      col <- "black"
    points(1:length(fit$grid), rev(m[i,]), type = "l", col = col)
  }
  abline(v = which(rev(fit$grid) == fit$nus[1]), lty = 2)
  #text(x = which(rev(fit$grid) == fit$nus[1]) - grid.l*0.03, y = max(m)*0.9, labels = "BIC")
  abline(v = which(rev(fit$grid) == fit$nus[2]), lty = 2)
  #text(x = which(rev(fit$grid) == fit$nus[2]) - grid.l*0.03, y = max(m)*0.9, labels = "AIC")
  abline(v = which(rev(fit$grid) == fit$nus[3]), lty = 2)
  #text(x = which(rev(fit$grid) == fit$nus[3]) + grid.l*0.03, y = max(m)*0.9, labels = "exLLH")
}

pdf("../../path_Sim.pdf", height = 6, width = 10) ## 
pathplot(fit)
dev.off()



