## changes 29.11.2022:
# - deleted start.null (which was intercepts as means, rest zero, copula theta arbitrary)
# - replaced with NULL, which instead uses gjrms default starting values from mgcv.


## gjrm.lasso is a wrapper function for gjrm() that optimises in CV, AIC and BIC
## the choice of penalty strength nu. 
## - data is a list object with data[[1]] being the data frame and data[[2]] the
## formula equations. 
## - Cop Copula class. See function CopChooser.
## - plot indication if paths should be plotted. Needs to be true, otherwise
##   path values are not stored.
## - grid.l grid length
## - K for CV
## - linear.equal,  LASSO.groups: see gjrm changes
## - CV indicating if CV should be done.
## - max.nu optional, an upper limit for nu may be given.
## - threshold threshold to set coefficients to zero. Coefficients standardised
## - carry.start.values: logical. Deciding, if carried over
##   coefficients from last grid-step should be used. Usually is preferred
#    to achieve better path plots. FALSE meaning the intercepts are the mean responses
#    in their link function and all covariates' coefficients are zero.

gjrm.lasso <- function(data, Cop, plot = FALSE, grid.l = 100, K = 10, linear.equal = NULL,
                       LASSO.groups = NULL, CV = FALSE, max.nu = NULL, threshold = 1e-03, 
                       CV.seed = 1904, xi = 1e9, carry.start.values = TRUE,
                       start.nu = 0.01){
  require(GJRM)
  ## ML-estimation at start
  fitML <- gjrm(data[[2]], data = data[[1]], BivD = Cop, margins = c("PO", "PO"),
                Model = "B", linear.equal = linear.equal, LASSO = TRUE, start.values = NULL,
                LASSO.nu = 0, LASSO.groups = LASSO.groups, xi = xi)
  #print(fitML$fit$stan.coef)
  beta.ML <- fitML$fit$stan.coef
  
  ##hv: automatic search for upper grid limit
  ##hv: Big steps up
  continue <- TRUE
  nu <- start.nu
  #print("searching maximal \nu")
  nop1 <- fitML$VC$X1.d2
  nop2 <- fitML$VC$X2.d2
  nop3 <- fitML$VC$X3.d2
  ## fix starting values (intercepts = means, coefficients zero and copula parameter about 0.1)
  yn1 <- as.character(data[[2]][[1]][2])
  yn2 <- as.character(data[[2]][[2]][2])
  intercept1 <- log(mean(data[[1]][yn1][,1], na.rm = TRUE))
  intercept2 <- log(mean(data[[1]][yn2][,1], na.rm = TRUE))
  #ind1 <- which(names(beta.ML) == "(Intercept)")
  ind1 <- c(1, nop1+1, nop1+nop2+1)
  #ind2 <- which(names(beta.ML) == "theta.star")
  start.values <- rep(0, length(beta.ML))
  start.values[ind1] <- beta.ML[ind1]
  
  ## These start values are deprecated. The most recent approach uses the gam
  ## results within gjrm().

  rm(ind1, intercept1, intercept2)
  start.null <- start.values
  while(continue){
    fit <- gjrm(data[[2]], data = data[[1]], BivD = Cop, margins = c("PO", "PO"),
                Model = "B", linear.equal = linear.equal, LASSO = TRUE,
                LASSO.nu = nu, beta.ML = beta.ML, LASSO.groups = LASSO.groups,
                start.values = NULL, threshold = threshold, xi = xi)
    mycheck <- sum(fit$fit$stan.coef0 != 0)
    ## count coefficients != 0.
    continue <- mycheck > 3 
    #print(mycheck)
    if(continue){
      nu <- nu * 1.1
      #print(nu)
    }
  }
  
  nu.flat <- nu
  
  if(!is.null(max.nu))
    nu.flat <- max.nu
  
  fit <- gjrm(data[[2]], data = data[[1]], BivD = Cop, margins = c("PO", "PO"),
              Model = "B", linear.equal = linear.equal, LASSO = TRUE, 
              LASSO.nu = nu.flat, beta.ML = beta.ML, LASSO.groups = LASSO.groups,
              start.values = NULL, xi = xi, threshold = threshold)
  
  
  print(paste("nu.flat found at", nu.flat))
  fit.flat <- fit
  
  ## new grid: use polynomial grid, easier and more practical on scale.
  
  grid <- rev((0:(grid.l - 1))^4 / (grid.l - 1)^4 * nu.flat)
  
  ## cycle through all nu values:
  taus <- numeric(grid.l)
  if(plot){
    nop3 <- dim(fit$VC$X3)[2]
    m <- matrix(0, nrow=length(fit.flat$fit$stan.coef0)-nop3, ncol=length(grid))
    m[,1] <- fit.flat$fit$stan.coef0[-((length(fit.flat$fit$stan.coef0)-nop3+1):length(fit.flat$fit$stan.coef0))]
  }
  
  d <- length(grid)
  bic <- numeric(d)
  aic <- numeric(d)
  exLLH <- numeric(d)
  llh <- numeric(d)
  dfs <- numeric(d)
  print(paste0("Starting on grid from 0 to ", nu.flat))
  n <- fit.flat$n
  for(nu in grid){
    #print(which(grid == nu))
    ## AIC und BIC outside CV
    if(carry.start.values){
      if(nu != grid[[1]])
        start.values <- fitvoll$fit$stan.coef  # carrying over from last \nu value
      else
        start.values <- NULL  # default glm start values for first \nu value
      
      #browser()
      fitvoll <- gjrm(data[[2]], data=data[[1]], BivD=Cop, margins = c("PO","PO"), Model="B", 
                      linear.equal = linear.equal, LASSO = TRUE, LASSO.nu = nu,
                      start.values = start.values, beta.ML = beta.ML, LASSO.groups = LASSO.groups,
                      threshold = threshold, xi = xi)
      #print(fitvoll$fit$argument[45])
    }
    else   # case: do not carry over at all
      fitvoll <- gjrm(data[[2]], data=data[[1]], BivD=Cop, margins = c("PO","PO"), Model="B", 
                      linear.equal = linear.equal, LASSO = TRUE, LASSO.nu = nu,
                      start.values = NULL, beta.ML = beta.ML, LASSO.groups = LASSO.groups,
                      threshold = threshold, xi = xi)
    
    if(plot){
      m[,which(nu == grid)] <- fitvoll$fit$stan.coef0[-((length(fitvoll$fit$stan.coef0)-nop3+1):length(fitvoll$fit$stan.coef0))]
      taus[which(nu == grid)] <- fitvoll$fit$argument[45]
    }
    bic[which(nu == grid)] <- myBIC(fitvoll)
    aic[which(nu == grid)] <- myAIC(fitvoll)
    llh[which(nu == grid)] <- logLik(fitvoll)
    dfs[which(nu == grid)] <- sum(fitvoll$fit$stan.coef0 != 0) - 3 #abc verallgemeinern?
    ## K-fold CV that uses the external likelihood
    if(CV){
      set.seed(CV.seed)
      ind <- sample(rep(1:K, ceiling(n/K))[1:n])
      intern.exLLH <- numeric(K)
      for(k in 1:K){
        welche <- ind == k
        test <- data[[1]][welche,]
        train <- data[[1]][!welche,]
        
        ## Use start values from full fit. This might be considered cheating,
        ## but as the starting values are not deemed to be super important,
        ## we stick to this approach as it is computational easy.
        fit <- gjrm(data[[2]], data = train, BivD=Cop, margins = c("PO","PO"),
                    Model = "B", linear.equal = linear.equal, LASSO = TRUE, LASSO.nu = nu,
                    start.values = fitvoll$fit$stan.coef, beta.ML = beta.ML,
                    LASSO.groups = LASSO.groups, threshold = threshold, xi = xi)
        
        ## evaluate
        ps <- NULL
        ps$S.h <- 0
        VC2 <- fit$VC
        ## recontruct VC to be able to calculate external likelihood:
        welche1 <- dimnames(VC2$X1)[[2]]
        welche2 <- dimnames(VC2$X2)[[2]]
        welche3 <- dimnames(VC2$X3)[[2]]
        VC2$X1 <- cbind("(Intercept)" = 1, test[,dimnames(test)[[2]] %in% welche1 |
                                                  paste0(dimnames(test)[[2]], "1") %in% welche1 |
                                                  paste0(dimnames(test)[[2]], "TRUE") %in% welche1])
        VC2$X2 <- cbind("(Intercept)" = 1, test[,dimnames(test)[[2]] %in% welche2 |
                                                  paste0(dimnames(test)[[2]], "1") %in% welche2 |
                                                  paste0(dimnames(test)[[2]], "TRUE") %in% welche2])
        VC2$X3 <- cbind(1, test[,dimnames(test)[[2]] %in% welche3 |
                                  paste0(dimnames(test)[[2]], "1") %in% welche3 |
                                  paste0(dimnames(test)[[2]], "TRUE") %in% welche3])
        ## repairing the order manually:
        VC2$X2 <- VC2$X2[,dimnames(fit$VC$X2)[[2]]]
        VC2$X1 <- VC2$X1[,dimnames(fit$VC$X1)[[2]]]
        
        eq1 <- data[[2]][[1]]
        eq2 <- data[[2]][[2]]
        dim1 <- length(test[,as.character(eq1[2])])
        maxg <- max(c(test[,as.character(eq1[2])], test[,as.character(eq2[2])]))
        VC2$y1m <- matrix(NA, nrow = dim1,
                          ncol = maxg+1)
        VC2$y2m <- matrix(NA, nrow = dim1,
                          ncol = maxg+1)
        VC2$n <- dim1
        VC2$weights <- rep(1, dim1)
        
        zwischen1 <- data.matrix(VC2$X1)
        zwischen1[(VC2$X1 != zwischen1) & !is.na(VC2$X1)] <- zwischen1[(VC2$X1 != zwischen1) & !is.na(VC2$X1)] - 1
        VC2$X1 <- zwischen1
        zwischen2 <- data.matrix(VC2$X2)
        zwischen2[(VC2$X2 != zwischen2) & !is.na(VC2$X2)] <- zwischen2[(VC2$X2 != zwischen2) & !is.na(VC2$X2)] - 1
        VC2$X2 <- zwischen2
        VC2$X3[,-1] <- VC2$X3[,-1] -1
        rm(zwischen1, zwischen2)
        for(i in 1:dim1){
          VC2$y1m[i,1:(test[,as.character(eq1[2])][i]+1)] <- 0:(test[,as.character(eq1[2])][i])
          VC2$y2m[i,1:(test[,as.character(eq2[2])][i]+1)] <- 0:(test[,as.character(eq2[2])][i])
        }
        VC2$X3 <- as.matrix(VC2$X3)
        intern.exLLH[k] <- -fit$VC$func(fit$fit$argument,
                                        list(y1 = test[,as.character(eq1[2])],
                                             y2 = test[,as.character(eq2[2])]),
                                        VC = VC2, ps = ps)$value
        
      }
      ## CV with external likelihood done for this single nu.
      
      exLLH[which(nu == grid)] <- sum(intern.exLLH)
    }
    
  }
  nu.best.bic <- grid[which.min(bic)]
  nu.best.aic <- grid[which.min(aic)]
  if(CV)
    nu.best.exLLH <- grid[which.max(exLLH)]
  
  ## the best nu is found, re-fit from maximum nu to nu.opt:
  print("Now refitting for best nu")
  if(carry.start.values){
    for (nu in grid){
      if(!CV)
        nu.best.exLLH <- Inf
      if(nu <  min(c(nu.best.bic, nu.best.aic, nu.best.exLLH)))
        break
      #print(nu)
      if(nu == grid[1]) # without starting values
        start.values <- NULL
      else
        start.values <- fit$fit$stan.coef
      
      fit <- gjrm(data[[2]], data=data[[1]], BivD=Cop, margins = c("PO","PO"), Model="B", 
                  linear.equal = linear.equal, LASSO = TRUE, LASSO.nu = nu,
                  beta.ML = beta.ML, LASSO.groups = LASSO.groups,
                  start.values = start.values, threshold = threshold, xi = xi)
      if(nu == nu.best.bic){
        fit.bic <- fit
      }
      if(nu == nu.best.aic){
        fit.aic <- fit
      }
      if(CV){
        if(nu == nu.best.exLLH){
          fit.exLLH <- fit
        }
      }
    }
  }
  else{
    fit.bic <- gjrm(data[[2]], data=data[[1]], BivD=Cop, margins = c("PO","PO"), Model="B", 
                    linear.equal = linear.equal, LASSO = TRUE, LASSO.nu = nu.best.bic,
                    start.values = NULL, beta.ML = beta.ML, LASSO.groups = LASSO.groups,
                    threshold = threshold, xi = xi)
    fit.aic <- gjrm(data[[2]], data=data[[1]], BivD=Cop, margins = c("PO","PO"), Model="B", 
                    linear.equal = linear.equal, LASSO = TRUE, LASSO.nu = nu.best.aic,
                    start.values = NULL, beta.ML = beta.ML, LASSO.groups = LASSO.groups,
                    threshold = threshold, xi = xi)
    if(CV){
      fit.exLLH <- gjrm(data[[2]], data=data[[1]], BivD=Cop, margins = c("PO","PO"), Model="B", 
                        linear.equal = linear.equal, LASSO = TRUE, LASSO.nu = nu.best.exLLH,
                        start.values = NULL, beta.ML = beta.ML, LASSO.groups = LASSO.groups,
                        threshold = threshold, xi = xi)
    }
  }
  
  if(plot){
    fitplot <- fit.aic
    dimnames(m)[[1]] <- names(fitplot$coefficients[-c((length(fitplot$coefficients) - nop3 + 1):length(fitplot$coefficients))])
    plot(grid, m[1,], type = "n", ylim=c(min(m)-0.1,max(m)+0.1), xlab = expression(nu))
    invisible(sapply(c(1:(length(fitplot$coefficients) - nop3)), FUN=function(i)
      points(grid, m[i,], type = "l")
    ))
    
    abline(v = nu.best.bic, col = "red")
    abline(v = nu.best.aic, col = "blue")
    if(CV)    
      abline(v = nu.best.exLLH, col = "green")
    legend(x = "topright", lty = 1, col = c("red", "blue"),
           legend = c("BIC", "AIC"))
  }
  if(!exists("m"))
    m <- NULL
  
  if(CV)
    l <- list(fit.bic = fit.bic, fit.aic = fit.aic, fit.exLLH = fit.exLLH,
              nus = c(nu.best.bic, nu.best.aic, nu.best.exLLH),
              m = m, grid = grid,
              aic = aic,
              bic = bic,
              exLLH = exLLH,
              llh = llh,
              dfs = dfs,
              taus = taus)
  if(!CV)
    l <- list(fit.bic = fit.bic, fit.aic = fit.aic,
              nus = c(nu.best.bic, nu.best.aic),
              m = m, grid = grid,
              aic = aic,
              bic = bic,
              dfs = dfs,
              taus = taus)
  return(l)
}

## BIC calculation to make sure this works as intended.

myBIC <- function(fit) {
  VCneu <- fit$VC
  VCneu$X1 <- VCneu$X1backup
  VCneu$X2 <- VCneu$X2backup
  n <- dim(VCneu$X1)[2] + dim(VCneu$X2)[2] + dim(VCneu$X3)[2]
  ps <- list(S.h = matrix(0, nrow = n, ncol = n), qu.mag = NULL)
  as.numeric(log(fit$n) * (sum(!fit$coefficients == 0)) - 2 * -fit$VC$func(params = fit$coefficients,
                                                                           respvec = fit$respvec,
                                                                           VC = VCneu, ps = ps)$value)
}

## AIC calculation to make this works as intended.

myAIC <- function(fit) {
  VCneu <- fit$VC
  VCneu$X1 <- VCneu$X1backup
  VCneu$X2 <- VCneu$X2backup
  n <- dim(VCneu$X1)[2] + dim(VCneu$X2)[2] + dim(VCneu$X3)[2]
  ps <- list(S.h = matrix(0, nrow = n, ncol = n), qu.mag = NULL)
  as.numeric(2 * (sum(!fit$coefficients == 0)) - 2 * -fit$VC$func(params = fit$coefficients,
                                                                  respvec = fit$respvec,
                                                                  VC = VCneu, ps = ps)$value)
}