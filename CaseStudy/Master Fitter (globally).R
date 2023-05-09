
source("Fitting Functions (globally)/all_Fitting Pois.R")
exe.pois.all()

source("Fitting Functions (globally)/all_Fitting Pois_both.R")
exe.pois.both.all()

source("Fitting Functions (globally)/all_Fitting Pois Lasso.R")
exe.pois.lasso.all()

source("Fitting Functions (globally)/all_Fitting Pois Lasso_both.R")
exe.pois.lasso.both.all()

## cop single

source("Fitting Functions (globally)/all_Fitting Cop.R") 
exe.cop.all(Cop = "F")
exe.cop.all(Cop = "FGM")
## calculated on cluster in paralel. However, can be calculated here.

source("Fitting Functions (globally)/all_Fitting Cop_both.R") 
exe.cop.all.both(Cop = "F")
exe.cop.all.both(Cop = "FGM")
## calculated on cluster in paralel. However, can be calculated here.

source("Fitting Functions (globally)/all_Fitting GJRM equal.R")
exe.cop.equal.all("F", date.ind = 915)
## about 30s for each call. Call for date.ind 914 - 1793 and copulae FGM and F 
## to reproduce everything. Calculated on cluster in paralel.

source("Fitting Functions (globally)/all_Fitting GJRM equal both.R")
exe.cop.equal.all.both("F", date.ind = 915)
## about 30s for each call. Call for date.ind 914 - 1793 and copulae FGM and F 
## to reproduce everything. Calculated on cluster in paralel.

source("Fitting Functions (globally)/all_Fitting GJRM Lasso.R")
exe.cop.lasso.all("F", date.ind = 915)
## about 30s for each call. Call for date.ind 914 - 1793 and copulae FGM and F 
## to reproduce everything. Calculated on cluster in paralel.

source("Fitting Functions (globally)/all_Fitting GJRM Lasso_both.R")
exe.cop.lasso.all.both("F", date.ind = 915)
## about 30s for each call. Call for date.ind 914 - 1793 and copulae FGM and F 
## to reproduce everything. Calculated on cluster in paralel.

source("Fitting Functions (globally)/all_Fitting GJRM fat.R")
exe.cop.fat.all(Cop = "F", date.ind = 915)
## about 30s for each call. Call for date.ind 914 - 1793 and copulae FGM and F 
## to reproduce everything. Calculated on cluster in paralel.

source("Fitting Functions (globally)/all_Fitting GJRM fat both.R")
exe.cop.fat.all.both(Cop = "F", date.ind = 915)
## about 30s for each call. Call for date.ind 914 - 1793 and copulae FGM and F 
## to reproduce everything. Calculated on cluster in paralel.


## RF 
source("Fitting Functions (globally)/all_Fitting RF.R") 
exe.rf(date.ind = 915)
## Call for date.ind 914 - 1793. Calculated in paralel on cluster.
source("Fitting Functions (globally)/all_Fitting RF_both.R")
exe.rf.both(date.ind = 915)
## Call for date.ind 914 - 1793. Calculated in paralel on cluster.


## xgboost is already using multiple cores, so single call here.
source("Fitting Functions (globally)/all_Fitting xgboost.R")
exe.xgboost()
## a few seconds per step (880 steps). about 1-2 hours in total.

source("Fitting Functions (globally)/all_Fitting xgboost_both.R")
exe.xgboost.both()
## a few seconds per step (880 steps). about 1-2 hours in total.
