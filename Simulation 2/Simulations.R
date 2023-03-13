
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
                                         grid.l = 250, K = 10, threshold = 1e-02))
save(erg, file = paste0("C90.01.", Sys.getenv("PBS_ARRAYID"), ".Rdata"))

## To re-create everything, one would have to call the SingleSim above 100 times
## for each of the 32 settings (copula + theta). Parallelisation is recommended.

## theta values from:

sapply(c(-0.75, -0.5, -0.25, -0.1, 0.1, 0.25, 0.5, 0.75), function(t) kendall(t))




