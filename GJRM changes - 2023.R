## Update 17.03.2023: Difcrit (convergence in iterations) now calculated without coefficients of theta.

## Update 21.03.2023: starting values via glm() in case of LASSO. row 674.

## Modified gjrm() function from the GJRM package. 

library(GJRM) # Version check: 0.2-3 from September 2020
library(R.utils)

## added arguments
# - linear.equal: optional. logical vector of the length of the first marginal coefficients.
#                 decides, what coefficients will be penalised to be equal.
# - start.values: optional. vector of coefficients used as starting vector.
# - LASSO:        optional. Logical. Indicates if LASSO penalisation is used.
# - LASSO.nu:     optional. LASSO-penaly strength. Only relevant when LASSO is true.
# - LASSO.groups: optional. List of vectors given the number of coefficients groups
#                 in the full coefficient vector. Each group is a vector in this list.
# - beta.ML:      optional. Vector of ML estimated coefficients. Needed with LASSO.
#                 note: Calculate with gjrm() with LASSO = TRUE and LASSO.nu = 0, so standardisation is active.
# - threshold:    optional. Smaller absolute coefficients will be set to zero in LASSO case.
# - xi:           optional. Penalty strength of equalisation penalty (see above).
# - iterative:    optional. TRUE/FALSE to indicate if estimation process should be iterative.
#                           Necessary for LASSO-type penalty, optional for equal penalty.
# - iter.weights: optional. Decides, if iterative weights should be used for the equal-penalty. \omega in paper.


gjrm <- function (formula, data = list(), weights = NULL, subset = NULL, 
                  BivD = "N", margins, Model, dof = 3, ordinal = FALSE, 
                  surv = FALSE, cens1 = NULL, cens2 = NULL, cens3 = NULL, dep.cens = FALSE, 
                  gamlssfit = FALSE, fp = FALSE, infl.fac = 1, rinit = 1, rmax = 100, 
                  iterlimsp = 50, tolsp = 1e-07, gc.l = FALSE, parscale, extra.regI = "t", 
                  k1.tvc = 0, k2.tvc = 0, knots = NULL, penCor = "unpen", 
                  sp.penCor = 3, Chol = FALSE, gamma = 1, w.alasso = NULL, 
                  drop.unused.levels = TRUE, ind.ord = FALSE, min.dn = 1e-40, 
                  min.pr = 1e-16, max.pr = 0.999999, linear.equal = NULL,
                  start.values = NULL, LASSO = FALSE, LASSO.nu = 0, 
                  LASSO.groups = NULL, beta.ML = NULL, threshold = 1e-03,
                  xi = 1e9, conv.crit = 0.01, iterative = TRUE, iter.weights = TRUE) 
{
  
  ##hv: addded: linear.equal. NULL or vector of false the length of the first
  ##hv: equation (including intercept) to not include penalty. Otherwise true 
  ##hv: indicates, what coefficients should be penalised. equation 1 and 2 have
  ##hv: to be ordered to correspond to this. Only coefficients in the same order
  ##hv: can be penalised.
  
  ##hv: start.values are possible to allow for smoother grid search for \nu.
  ##hv: Should only be used in wrapper function such as gjrm.lasso.
  
  ##hv: LASSO TRUE/FALSE indicates if the penalisation should be used.
  
  ##hv: Lasso.nu numeric, indicates the penalty strength. A grid search is advised.
  
  ##hv: LASSO.groups list. Consists of one or more vectors containing different
  ##hv: coefficient groups. Each vector contains the corresponding order numbers
  ##hv: belonging to the same group. Numbering includes the intercept.
  
  ##hv: beta.ML: ML estimations. Used for adaptive weighting approach. Given
  ##hv: as vector.
  
  ##hv: threshold: to set coefficients to exactly zero. Denoted e_lasso in 
  ##hv: submission.
  
  ##hv: iterative indicates, if the estimation process should be iterative.
  ##hv: mandatory for LASSO, but optional for equal-penalty. FALSE will save
  ##hv: computational costs.
  
  if(!iterative & LASSO & LASSO.nu > 0)
    stop("LASSO requires iterative = TRUE")
  if(is.null(LASSO.nu))
    LASSO.nu <- 0
  if(LASSO.nu > 0 & !(LASSO))
    stop("LASSO.nu chosen with LASSO inactive. Did you forget to set LASSO = TRUE?")
  if(LASSO & LASSO.nu > 0 & is.null(beta.ML))
    stop("ML estimation required for LASSO")
  if (missing(margins)) 
    stop("You must choose the margins' values.")
  if (missing(Model)) 
    stop("You must choose a Model type.")
  if (margins[1] == "PH" && surv == TRUE) 
    margins[1] <- "cloglog"
  if (margins[1] == "PO" && surv == TRUE) 
    margins[1] <- "logit"
  if (margins[2] == "PH" && surv == TRUE) 
    margins[2] <- "cloglog"
  if (margins[2] == "PO" && surv == TRUE) 
    margins[2] <- "logit"
  bl <- c("probit", "logit", "cloglog")
  if (surv == FALSE && ordinal == FALSE) {
    if ((margins[1] %in% bl && margins[2] %in% bl && is.na(margins[3])) || 
        (margins[1] %in% bl && !(margins[2] %in% bl) && Model == 
         "B" && is.na(margins[3]))) {
      L <- eval(substitute(SemiParBIV(formula, data, weights, 
                                      subset, Model, BivD, margins, dof, gamlssfit, 
                                      fp, hess = TRUE, infl.fac, rinit, rmax, iterlimsp, 
                                      tolsp, gc.l, parscale, extra.regI, intf = TRUE, 
                                      theta.fx = NULL, knots = knots, drop.unused.levels = drop.unused.levels, 
                                      min.dn = min.dn, min.pr = min.pr, max.pr = max.pr), 
                           list(weights = weights)))
    }
  }
  if (surv == FALSE && ordinal == TRUE) {
    if ((margins[1] %in% bl && margins[2] %in% bl && is.na(margins[3])) || 
        (margins[1] %in% bl && !(margins[2] %in% bl) && is.na(margins[3]))) {
      L <- eval(substitute(CopulaCLM(formula, data, weights, 
                                     subset, Model, BivD, margins, dof, gamlssfit, 
                                     fp, hess = TRUE, infl.fac, rinit, rmax, iterlimsp, 
                                     tolsp, gc.l, parscale, extra.regI, intf = TRUE, 
                                     theta.fx = NULL, knots = knots, drop.unused.levels = drop.unused.levels, 
                                     ind.ord = ind.ord, min.dn = min.dn, min.pr = min.pr, 
                                     max.pr = max.pr), list(weights = weights)))
    }
  }
  if (margins[1] %in% bl && !(margins[2] %in% bl) && surv == 
      FALSE && is.na(margins[3]) && Model == "BSS" && 
      ordinal == FALSE) {
    L <- eval(substitute(copulaSampleSel(formula, data, weights, 
                                         subset, BivD, margins, dof, fp, infl.fac, rinit, 
                                         rmax, iterlimsp, tolsp, gc.l, parscale, extra.regI, 
                                         knots, drop.unused.levels = drop.unused.levels, min.dn = min.dn, 
                                         min.pr = min.pr, max.pr = max.pr), list(weights = weights)))
  }
  if (!is.na(margins[3])) {
    if (margins[1] %in% bl && margins[2] %in% bl && margins[3] %in% 
        bl && surv == FALSE && ordinal == FALSE) {
      L <- eval(substitute(SemiParTRIV(formula, data, weights, 
                                       subset, Model, margins, penCor, sp.penCor, approx = FALSE, 
                                       Chol, infl.fac, gamma, w.alasso, rinit, rmax, 
                                       iterlimsp, tolsp, gc.l, parscale, extra.regI, 
                                       knots, drop.unused.levels = drop.unused.levels, 
                                       min.dn = min.dn, min.pr = min.pr, max.pr = max.pr), 
                           list(weights = weights)))
    }
  }
  if ((!(margins[1] %in% bl) || surv == TRUE) && ordinal == 
      FALSE) {
    robust <- FALSE
    t.c = 3
    sp <- qu.mag <- y1.y2 <- y1.cy2 <- cy1.y2 <- cy1.cy2 <- cy <- cy1 <- gamlss1 <- gamlss2 <- gam1 <- gam2 <- y1m <- y2m <- indexTeq1 <- indexTeq2 <- NULL
    i.rho <- log.sig2.2 <- log.nu.2 <- log.nu.1 <- log.sig2.1 <- dof.st <- NULL
    end <- X3.d2 <- X4.d2 <- X5.d2 <- X6.d2 <- X7.d2 <- X8.d2 <- l.sp3 <- l.sp4 <- l.sp5 <- l.sp6 <- l.sp7 <- l.sp8 <- 0
    sp1 <- sp2 <- NULL
    sp3 <- gp3 <- gam3 <- X3 <- sp4 <- gp4 <- gam4 <- X4 <- sp5 <- gp5 <- gam5 <- X5 <- NULL
    sp6 <- gp6 <- gam6 <- X6 <- sp7 <- gp7 <- gam7 <- X7 <- sp8 <- gp8 <- gam8 <- X8 <- NULL
    c11 <- c10 <- c01 <- c00 <- NA
    Sl.sf <- NULL
    sp.method <- "perf"
    Xd1 <- Xd2 <- mono.sm.pos1 <- mono.sm.pos2 <- mono.sm.pos <- NULL
    surv.flex <- FALSE
    Deq1 <- pos.pbeq1 <- Deq2 <- pos.pbeq2 <- list()
    BivD2 <- c("C0C90", "C0C270", "C180C90", 
               "C180C270", "J0J90", "J0J270", 
               "J180J90", "J180J270", "G0G90", 
               "G0G270", "G180G90", "G180G270")
    opc <- c("N", "C0", "C90", "C180", 
             "C270", "J0", "J90", "J180", 
             "J270", "G0", "G90", "G180", 
             "G270", "F", "AMH", "FGM", 
             "T", "PL", "HO")
    scc <- c("C0", "C180", "J0", "J180", 
             "G0", "G180", BivD2)
    sccn <- c("C90", "C270", "J90", "J270", 
              "G90", "G270")
    m2 <- c("N", "GU", "rGU", "LO", 
            "LN", "WEI", "iG", "GA", 
            "BE", "FISK", "GP", "GPII", 
            "GPo")
    m3 <- c("DAGUM", "SM", "TW")
    m1d <- c("PO", "ZTP")
    m2d <- c("NBI", "NBII", "PIG", "DGP", 
             "DGPII")
    m3d <- c("DEL", "SICHEL")
    ct <- data.frame(c(opc), c(1:14, 55, 56, 57, 60, 61))
    cta <- data.frame(c(opc), c(1, 3, 23, 13, 33, 6, 26, 
                                16, 36, 4, 24, 14, 34, 5, 55, 56, 2, 60, 61))
    if (BivD %in% BivD2) {
      if (BivD %in% BivD2[1:4]) 
        BivDt <- "C0"
      if (BivD %in% BivD2[5:12]) 
        BivDt <- "J0"
      nC <- ct[which(ct[, 1] == BivDt), 2]
      nCa <- cta[which(cta[, 1] == BivDt), 2]
    }
    if (!(BivD %in% BivD2)) {
      nC <- ct[which(ct[, 1] == BivD), 2]
      nCa <- cta[which(cta[, 1] == BivD), 2]
    }
    if (!is.list(formula)) 
      stop("You must specify a list of equations.")
    l.flist <- length(formula)
    form.check(formula, l.flist)
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    pred.varR <- pred.var(formula, l.flist)
    v1 <- pred.varR$v1
    v2 <- pred.varR$v2
    pred.n <- pred.varR$pred.n
    fake.formula <- paste(v1[1], "~", paste(pred.n, 
                                            collapse = " + "))
    environment(fake.formula) <- environment(formula[[1]])
    mf$formula <- fake.formula
    ##hv: add new arguments in here:
    mf$min.dn <- mf$min.pr <- mf$LASSO <- mf$threshold <- mf$beta.ML <- mf$iter.weights <- mf$start.values <- mf$LASSO.nu <- mf$LASSO.groups <- mf$linear.equal <- mf$max.pr <- mf$dep.cens <- mf$ordinal <- mf$Model <- mf$knots <- mf$k1.tvc <- mf$k2.tvc <- mf$surv <- mf$BivD <- mf$margins <- mf$fp <- mf$dof <- mf$infl.fac <- mf$rinit <- mf$rmax <- mf$iterlimsp <- mf$tolsp <- mf$gc.l <- mf$parscale <- mf$extra.regI <- mf$gamlssfit <- mf$xi <- mf$conv.crit <- mf$iterative <- NULL
    ##hv: END
    mf$drop.unused.levels <- drop.unused.levels
    mf[[1]] <- as.name("model.frame")
    data <- eval(mf, parent.frame())
    if (gc.l == TRUE) 
      gc()
    n <- dim(data)[1]
    if (!("(weights)" %in% names(data))) {
      weights <- rep(1, dim(data)[1])
      data$weights <- weights
      names(data)[length(names(data))] <- "(weights)"
    }
    else weights <- data[, "(weights)"]
    if (surv == TRUE && !("(cens1)" %in% names(data)) && 
        margins[1] %in% bl) 
      stop("You must provide the first binary censoring indicator.")
    if (surv == TRUE && !("(cens2)" %in% names(data)) && 
        margins[2] %in% bl) 
      stop("You must provide the second binary censoring indicator.")
    if (!("(cens1)" %in% names(data))) {
      cens1 <- rep(0, dim(data)[1])
      data$cens1 <- cens1
      names(data)[length(names(data))] <- "(cens1)"
    }
    else cens1 <- data[, "(cens1)"]
    if (!("(cens2)" %in% names(data))) {
      cens2 <- rep(0, dim(data)[1])
      data$cens2 <- cens2
      names(data)[length(names(data))] <- "(cens2)"
    }
    else cens2 <- data[, "(cens2)"]
    if (!("(cens3)" %in% names(data))) {
      cens3 <- rep(0, dim(data)[1])
      data$cens3 <- cens3
      names(data)[length(names(data))] <- "(cens3)"
    }
    else cens3 <- data[, "(cens3)"]
    M <- list(m1d = m1d, m2 = m2, m2d = m2d, m3 = m3, m3d = m3d, 
              BivD = BivD, bl = bl, robust = robust, opc = opc, 
              extra.regI = extra.regI, margins = margins, BivD2 = BivD2, 
              dof = dof, surv = surv, c1 = cens1, c2 = cens2, c3 = cens3, 
              dep.cens = dep.cens)
    M$K1 <- NULL
    pream.wm(formula, margins, M, l.flist)
    formula.eq1 <- formula[[1]]
    formula.eq2 <- formula[[2]]
    form.eq12R <- form.eq12(formula.eq1, data, v1, margins[1], 
                            m1d, m2d)
    formula.eq1 <- form.eq12R$formula.eq1
    formula.eq1r <- form.eq12R$formula.eq1r
    y1 <- form.eq12R$y1
    y1.test <- form.eq12R$y1.test
    y1m <- form.eq12R$y1m
    if (surv == FALSE) 
      gam1 <- eval(substitute(gam(formula.eq1, gamma = infl.fac, 
                                  weights = weights, data = data, knots = knots, 
                                  drop.unused.levels = drop.unused.levels), list(weights = weights)))
    if (surv == TRUE && margins[1] %in% c(m2, m3) && margins[2] %in% 
        bl) 
      gam1 <- eval(substitute(gam(formula.eq1, gamma = infl.fac, 
                                  weights = weights, data = data, knots = knots, 
                                  drop.unused.levels = drop.unused.levels), list(weights = weights)))
    else {
      if (surv == TRUE && !(margins[1] %in% bl)) 
        gam1 <- eval(substitute(gam(formula.eq1, gamma = infl.fac, 
                                    weights = weights * cens1, data = data, knots = knots, 
                                    drop.unused.levels = drop.unused.levels), list(weights = weights, 
                                                                                   cens1 = cens1)))
    }
    if (surv == TRUE && margins[1] %in% bl) {
      surv.flex <- TRUE
      f.eq1 <- form.eq12R$f.eq1
      data$urcfcphmwicu <- seq(-10, 10, length.out = dim(data)[1])
      tempb <- eval(substitute(gam(f.eq1, family = cox.ph(), 
                                   data = data, weights = cens1, drop.unused.levels = drop.unused.levels), 
                               list(cens1 = cens1)))
      data$Sh <- as.vector(mm(predict(tempb, type = "response"), 
                              min.pr = min.pr, max.pr = max.pr))
      cens11 <- ifelse(cens1 == 0, 1e-07, cens1)
      gam1 <- eval(substitute(scam(formula.eq1, gamma = infl.fac, 
                                   weights = weights * cens11, data = data), list(weights = weights, 
                                                                                  cens11 = cens11)))
      lsgam1 <- length(gam1$smooth)
      if (lsgam1 == 0) 
        stop("You must use at least a monotonic smooth function of time in the first equation.")
      clsm <- ggr <- NA
      for (i in 1:lsgam1) {
        clsm[i] <- class(gam1$smooth[[i]])[1]
      }
      if (sum(as.numeric(clsm[1] %in% c("mpi.smooth"))) == 
          0) 
        stop("You must have a monotonic smooth of time and it has to be the first to be included.")
      l.sp1 <- length(gam1$sp)
      if (l.sp1 != 0) 
        sp1 <- gam1$sp
      sp1[1] <- 1
      gam.call <- gam1$call
      gam.call$sp <- sp1
      gam1 <- eval(gam.call)
      j <- 1
      for (i in 1:lsgam1) {
        if (max(as.numeric(grepl(v1[1], gam1$smooth[[i]]$term))) != 
            0 && clsm[i] == "mpi.smooth") 
          mono.sm.pos1 <- c(mono.sm.pos1, c(gam1$smooth[[i]]$first.para:gam1$smooth[[i]]$last.para))
      }
      X1 <- predict(gam1, type = "lpmatrix")
      if (!is.null(indexTeq1) && k1.tvc != 0) {
        if (range(X1[, indexTeq1])[1] < 0) 
          stop("Check design matrix for smooth(s) of tvc term(s) in eq. 1.")
      }
      Xd1 <- Xdpred(gam1, data, v1[1])
      gam1$y <- data[, v1[1]]
      st.v1 <- c(gam1$coefficients)
      if (!is.null(indexTeq1)) {
        st.v1[mono.sm.pos1] <- exp(st.v1[mono.sm.pos1])
        while (range(Xd1 %*% st.v1)[1] < 0) st.v1[indexTeq1] <- 0.999 * 
            st.v1[indexTeq1]
        gam1$coefficients <- gam1$coefficients.t <- st.v1
        gam1$coefficients.t[mono.sm.pos1] <- exp(gam1$coefficients.t[mono.sm.pos1])
      }
    }
    gam1$formula <- formula.eq1r
    lsgam1 <- length(gam1$smooth)
    y1 <- y1.test
    if (margins[1] %in% c("LN")) 
      y1 <- log(y1)
    attr(data, "terms") <- NULL
    if (!(surv == TRUE && margins[1] %in% bl)) {
      names(gam1$model)[1] <- as.character(formula.eq1r[2])
      X1 <- predict(gam1, type = "lpmatrix")
      l.sp1 <- length(gam1$sp)
      sp1 <- gam1$sp
    }
    gp1 <- gam1$nsdf
    X1.d2 <- dim(X1)[2]
    form.eq12R <- form.eq12(formula.eq2, data, v2, margins[2], 
                            m1d, m2d)
    formula.eq2 <- form.eq12R$formula.eq1
    formula.eq2r <- form.eq12R$formula.eq1r
    y2 <- form.eq12R$y1
    y2.test <- form.eq12R$y1.test
    y2m <- form.eq12R$y1m
    if (surv == FALSE) 
      gam2 <- eval(substitute(gam(formula.eq2, gamma = infl.fac, 
                                  weights = weights, data = data, knots = knots, 
                                  drop.unused.levels = drop.unused.levels), list(weights = weights)))
    if (surv == TRUE && !(margins[2] %in% bl)) 
      gam2 <- eval(substitute(gam(formula.eq2, gamma = infl.fac, 
                                  weights = weights * cens2, data = data, knots = knots, 
                                  drop.unused.levels = drop.unused.levels), list(weights = weights, 
                                                                                 cens2 = cens2)))
    if (surv == TRUE && margins[2] %in% bl) {
      surv.flex <- TRUE
      f.eq2 <- form.eq12R$f.eq1
      data$urcfcphmwicu <- seq(-10, 10, length.out = dim(data)[1])
      tempb <- eval(substitute(gam(f.eq2, family = cox.ph(), 
                                   data = data, weights = cens2, drop.unused.levels = drop.unused.levels), 
                               list(cens2 = cens2)))
      data$Sh <- as.vector(mm(predict(tempb, type = "response"), 
                              min.pr = min.pr, max.pr = max.pr))
      cens22 <- ifelse(cens2 == 0, 1e-07, cens2)
      gam2 <- eval(substitute(scam(formula.eq2, gamma = infl.fac, 
                                   weights = weights * cens22, data = data), list(weights = weights, 
                                                                                  cens22 = cens22)))
      lsgam2 <- length(gam2$smooth)
      if (lsgam2 == 0) 
        stop("You must use at least a monotonic smooth function of time in the second equation.")
      clsm <- ggr <- NA
      for (i in 1:lsgam2) {
        clsm[i] <- class(gam2$smooth[[i]])[1]
      }
      if (sum(as.numeric(clsm[1] %in% c("mpi.smooth"))) == 
          0) 
        stop("You must have a monotonic smooth of time and it has to be the first to be included.")
      l.sp2 <- length(gam2$sp)
      if (l.sp2 != 0) 
        sp2 <- gam2$sp
      sp2[1] <- 1
      gam.call <- gam2$call
      gam.call$sp <- sp2
      gam2 <- eval(gam.call)
      j <- 1
      for (i in 1:lsgam2) {
        if (max(as.numeric(grepl(v2[1], gam2$smooth[[i]]$term))) != 
            0 && clsm[i] == "mpi.smooth") 
          mono.sm.pos2 <- c(mono.sm.pos2, c(gam2$smooth[[i]]$first.para:gam2$smooth[[i]]$last.para))
      }
      X2 <- predict(gam2, type = "lpmatrix")
      if (!is.null(indexTeq2) && k2.tvc != 0) {
        if (range(X2[, indexTeq2])[1] < 0) 
          stop("Check design matrix for smooth(s) of tvc term(s) in eq. 2.")
      }
      Xd2 <- Xdpred(gam2, data, v2[1])
      gam2$y <- data[, v2[1]]
      st.v2 <- c(gam2$coefficients)
      if (!is.null(indexTeq2)) {
        st.v2[mono.sm.pos2] <- exp(st.v2[mono.sm.pos2])
        while (range(Xd2 %*% st.v2)[1] < 0) st.v2[indexTeq2] <- 0.999 * 
            st.v2[indexTeq2]
        gam2$coefficients <- gam2$coefficients.t <- st.v2
        gam2$coefficients.t[mono.sm.pos2] <- exp(gam2$coefficients.t[mono.sm.pos2])
      }
    }
    gam2$formula <- formula.eq2r
    lsgam2 <- length(gam2$smooth)
    y2 <- y2.test
    if (margins[2] %in% c("LN")) 
      y2 <- log(y2)
    attr(data, "terms") <- NULL
    if (!(surv == TRUE && margins[2] %in% bl)) {
      names(gam2$model)[1] <- as.character(formula.eq2r[2])
      X2 <- predict(gam2, type = "lpmatrix")
      l.sp2 <- length(gam2$sp)
      sp2 <- gam2$sp
    }
    gp2 <- gam2$nsdf
    X2.d2 <- dim(X2)[2]
    res1 <- residuals(gam1)
    res2 <- residuals(gam2)
    ass.s <- cor(res1, res2, method = "kendall")
    ass.s <- sign(ass.s) * ifelse(abs(ass.s) > 0.9, 0.9, 
                                  abs(ass.s))
    i.rho <- ass.dp(ass.s, BivD, scc, sccn, nCa)
    dof.st <- log(dof - 2)
    names(dof.st) <- "dof.star"
    if (!(margins[1] %in% c(m1d, bl))) {
      start.snR <- startsn(margins[1], y1)
      log.sig2.1 <- start.snR$log.sig2.1
      names(log.sig2.1) <- "sigma1.star"
      if (margins[1] %in% c(m3)) {
        log.nu.1 <- start.snR$log.nu.1
        names(log.nu.1) <- "nu.1.star"
      }
    }
    if (!(margins[2] %in% c(m1d, bl))) {
      start.snR <- startsn(margins[2], y2)
      log.sig2.2 <- start.snR$log.sig2.1
      names(log.sig2.2) <- "sigma2.star"
      if (margins[2] %in% c(m3)) {
        log.nu.2 <- start.snR$log.nu.1
        names(log.nu.2) <- "nu.2.star"
      }
    }
    vo <- list(gam1 = gam1, gam2 = gam2, i.rho = i.rho, log.sig2.2 = log.sig2.2, 
               log.nu.2 = log.nu.2, log.nu.1 = log.nu.1, log.sig2.1 = log.sig2.1, 
               dof.st = dof.st, n = n, drop.unused.levels = drop.unused.levels)
    start.v <- overall.sv(margins, M, vo)
    if (l.flist > 2) {
      overall.svGR <- overall.svG(formula, data, ngc = 2, 
                                  margins, M, vo, gam1, gam2, knots = knots)
      start.v = overall.svGR$start.v
      X3 = overall.svGR$X3
      X4 = overall.svGR$X4
      X5 = overall.svGR$X5
      X6 = overall.svGR$X6
      X7 = overall.svGR$X7
      X8 = overall.svGR$X8
      X3.d2 = overall.svGR$X3.d2
      X4.d2 = overall.svGR$X4.d2
      X5.d2 = overall.svGR$X5.d2
      X6.d2 = overall.svGR$X6.d2
      X7.d2 = overall.svGR$X7.d2
      X8.d2 = overall.svGR$X8.d2
      gp3 = overall.svGR$gp3
      gp4 = overall.svGR$gp4
      gp5 = overall.svGR$gp5
      gp6 = overall.svGR$gp6
      gp7 = overall.svGR$gp7
      gp8 = overall.svGR$gp8
      gam3 = overall.svGR$gam3
      gam4 = overall.svGR$gam4
      gam5 = overall.svGR$gam5
      gam6 = overall.svGR$gam6
      gam7 = overall.svGR$gam7
      gam8 = overall.svGR$gam8
      l.sp3 = overall.svGR$l.sp3
      l.sp4 = overall.svGR$l.sp4
      l.sp5 = overall.svGR$l.sp5
      l.sp6 = overall.svGR$l.sp6
      l.sp7 = overall.svGR$l.sp7
      l.sp8 = overall.svGR$l.sp8
      sp3 = overall.svGR$sp3
      sp4 = overall.svGR$sp4
      sp5 = overall.svGR$sp5
      sp6 = overall.svGR$sp6
      sp7 = overall.svGR$sp7
      sp8 = overall.svGR$sp8
    }
    GAM <- list(gam1 = gam1, gam2 = gam2, gam3 = gam3, gam4 = gam4, 
                gam5 = gam5, gam6 = gam6, gam7 = gam7, gam8 = gam8)
    if ((l.sp1 != 0 || l.sp2 != 0 || l.sp3 != 0 || l.sp4 != 
         0 || l.sp5 != 0 || l.sp6 != 0 || l.sp7 != 0 || l.sp8 != 
         0) && fp == FALSE) {
      L.GAM <- list(l.gam1 = length(gam1$coefficients), 
                    l.gam2 = length(gam2$coefficients), l.gam3 = length(gam3$coefficients), 
                    l.gam4 = length(gam4$coefficients), l.gam5 = length(gam5$coefficients), 
                    l.gam6 = length(gam6$coefficients), l.gam7 = length(gam7$coefficients), 
                    l.gam8 = length(gam8$coefficients))
      L.SP <- list(l.sp1 = l.sp1, l.sp2 = l.sp2, l.sp3 = l.sp3, 
                   l.sp4 = l.sp4, l.sp5 = l.sp5, l.sp6 = l.sp6, 
                   l.sp7 = l.sp7, l.sp8 = l.sp8)
      sp <- c(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8)
      qu.mag <- S.m(GAM, L.SP, L.GAM)
    }
    if (missing(parscale)) 
      parscale <- 1
    respvec <- respvec2 <- respvec3 <- list(y1 = y1, y2 = y2, 
                                            y1.y2 = NULL, y1.cy2 = NULL, cy1.y2 = NULL, cy1.cy2 = NULL, 
                                            cy1 = NULL, cy = NULL, univ = 0)
    my.env <- new.env()
    my.env$signind <- 1
    lsgam3 <- length(gam3$smooth)
    lsgam4 <- length(gam4$smooth)
    lsgam5 <- length(gam5$smooth)
    lsgam6 <- length(gam6$smooth)
    lsgam7 <- length(gam7$smooth)
    lsgam8 <- length(gam8$smooth)
    if (surv == TRUE && dep.cens == FALSE) {
      if ((surv == TRUE && margins[1] %in% bl && margins[2] %in% 
           bl) || (surv == TRUE && margins[1] %in% m2 && 
                   margins[2] %in% m2)) {
        c11 <- cens1 * cens2
        c10 <- cens1 * (1 - cens2)
        c01 <- (1 - cens1) * cens2
        c00 <- (1 - cens1) * (1 - cens2)
      }
      if (surv == TRUE && margins[1] %in% c(m2, m3) && 
          margins[2] %in% bl) {
        c11 <- cens2
        c10 <- 1 - cens2
        c01 <- NULL
        c00 <- NULL
      }
    }
    if (surv == TRUE && dep.cens == TRUE) {
      c11 <- NULL
      c10 <- cens1
      c01 <- cens2
      c00 <- cens3
    }
    my.env$k1 <- k1.tvc
    my.env$k2 <- k2.tvc
    VC <- list(lsgam1 = lsgam1, indexTeq1 = indexTeq1, indexTeq2 = indexTeq2, 
               lsgam2 = lsgam2, Deq1 = Deq1, pos.pbeq1 = pos.pbeq1, 
               Deq2 = Deq2, pos.pbeq2 = pos.pbeq2, lsgam3 = lsgam3, 
               robust = FALSE, sp.fixed = NULL, lsgam4 = lsgam4, 
               Sl.sf = Sl.sf, sp.method = sp.method, lsgam5 = lsgam5, 
               K1 = NULL, lsgam6 = lsgam6, lsgam7 = lsgam7, lsgam8 = lsgam8, 
               X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5, X6 = X6, 
               X7 = X7, X8 = X8, X1.d2 = X1.d2, X2.d2 = X2.d2, X3.d2 = X3.d2, 
               X4.d2 = X4.d2, X5.d2 = X5.d2, X6.d2 = X6.d2, X7.d2 = X7.d2, 
               X8.d2 = X8.d2, gp1 = gp1, gp2 = gp2, gp3 = gp3, gp4 = gp4, 
               gp5 = gp5, gp6 = gp6, gp7 = gp7, gp8 = gp8, l.sp1 = l.sp1, 
               l.sp2 = l.sp2, l.sp3 = l.sp3, l.sp4 = l.sp4, l.sp5 = l.sp5, 
               l.sp6 = l.sp6, l.sp7 = l.sp7, l.sp8 = l.sp8, my.env = my.env, 
               infl.fac = infl.fac, weights = weights, fp = fp, 
               gamlssfit = gamlssfit, hess = NULL, Model = "CC", 
               univ.gamls = FALSE, end = end, BivD = BivD, nCa = nCa, 
               nC = nC, gc.l = gc.l, n = n, extra.regI = extra.regI, 
               parscale = parscale, margins = margins, Cont = "YES", 
               ccss = "no", m2 = m2, m3 = m3, m1d = m1d, m2d = m2d, 
               m3d = m3d, bl = bl, triv = FALSE, y1m = y1m, y2m = y2m, 
               tc = t.c, i.rho = i.rho, dof = dof, dof.st = dof.st, 
               BivD2 = BivD2, cta = cta, ct = ct, zerov = -10, c11 = c11, 
               c10 = c10, c01 = c01, c00 = c00, surv = surv, Xd1 = Xd1, 
               Xd2 = Xd2, mono.sm.pos1 = mono.sm.pos1, mono.sm.pos2 = mono.sm.pos2, 
               surv.flex = surv.flex, mono.sm.pos = mono.sm.pos, 
               gp2.inf = NULL, informative = "no", zero.tol = 0.01, 
               min.dn = min.dn, min.pr = min.pr, max.pr = max.pr)
    if (gc.l == TRUE) 
      gc()
    if (gamlssfit == TRUE) {
      form.gamlR <- form.gaml(formula, l.flist, M)
      surv1 <- surv2 <- surv
      if (surv == TRUE && margins[1] %in% c(m2, m3) && 
          margins[2] %in% bl) 
        surv1 <- FALSE
      gamlss1 <- eval(substitute(gamlss(form.gamlR$formula.gamlss1, 
                                        data = data, weights = weights, subset = subset, 
                                        margin = margins[1], surv = surv1, cens = cens1, 
                                        infl.fac = infl.fac, rinit = rinit, rmax = rmax, 
                                        iterlimsp = iterlimsp, tolsp = tolsp, gc.l = gc.l, 
                                        parscale = 1, extra.regI = extra.regI, k.tvc = k1.tvc, 
                                        drop.unused.levels = drop.unused.levels), list(weights = weights, 
                                                                                       cens1 = cens1)))
      gamlss2 <- eval(substitute(gamlss(form.gamlR$formula.gamlss2, 
                                        data = data, weights = weights, subset = subset, 
                                        margin = margins[2], surv = surv2, cens = cens2, 
                                        infl.fac = infl.fac, rinit = rinit, rmax = rmax, 
                                        iterlimsp = iterlimsp, tolsp = tolsp, gc.l = gc.l, 
                                        parscale = 1, extra.regI = extra.regI, k.tvc = k2.tvc, 
                                        drop.unused.levels = drop.unused.levels), list(weights = weights, 
                                                                                       cens2 = cens2)))
      SP <- list(sp1 = sp1, sp2 = sp2, sp3 = sp3, sp4 = sp4, 
                 sp5 = sp5, sp6 = sp6, sp7 = sp7, sp8 = sp8)
      gamls.upsvR <- gamls.upsv(gamlss1, gamlss2, margins, 
                                M, l.flist, nstv = names(start.v), VC, GAM, SP)
      sp <- gamls.upsvR$sp
      start.v <- gamls.upsvR$start.v
    }
    func.opt <- func.OPT(margins, M)
    VC$func <- func.opt
    
    ##hv: standardise covariates if LASSO penalty is active:
    if(LASSO){
      nop1 <- VC$gp1
      nop2 <- VC$gp2
      sds <- numeric(nop1 + nop2)
      names(sds) <- c(dimnames(VC$X1)[[2]], dimnames(VC$X2)[[2]])
      ##hv: empty sds vector created.
      ##hv: sds of simple covariates
      zw1 <- apply(VC$X1, 2, sd)
      zw2 <- apply(VC$X2, 2, sd)
      sds <- c(zw1, zw2)
      rm(zw1, zw2)
      ##hv: and now for covariates, whose coefficients are forced equal
      if(is.null(linear.equal))
        linear.equal <- rep(FALSE, nop1)
      for (i in 1:min(nop1, nop2)){
        #hv: 1:min(nop1,nop2) is enough, because coefficients that are forced equal
        #hv: have to be at the same numbered place in both marginal formula.
        if(linear.equal[i]) {
          sds[c(i, nop1+i)] <- sd(c(VC$X1[,i], VC$X2[,i]))
        }
      }
      ##hv: set the sds for the intercepts to 1. sds is later used to correct
      ##hv: the design matrices.
      sds[sds == 0] <- 1
      VC$X1backup <- VC$X1
      VC$X2backup <- VC$X2
      VC$X1 <- t(t(VC$X1) / sds[1:nop1])
      VC$X2 <- t(t(VC$X2) / sds[(nop1 + 1):(nop1 + nop2)])
      ##hv: Groups of dummy variables have to be treated separately. The values from
      ##hv: above can simply be overwritten for these columns.
      if(!is.null(LASSO.groups)){
        qrmatrices1 <- NULL
        qrmatrices2 <- NULL
        for (j in 1:length(LASSO.groups)){
          nj <- length(LASSO.groups[[j]])
          index <- LASSO.groups[[j]]
          #if(any(abs(apply(VC$X1backup[,index], 2, mean)) > 1e-05)) stop("Dummy coded variables need to be centered")
          
          zwblock <- blockstand(VC$X1backup[,index], list(c(1:nj)), NULL)
          VC$X1[,index] <- zwblock$x
          sds[index] <- 1
          qrmatrices1[[j]] <- zwblock[[2]][[1]]
          
          zwblock2 <- blockstand(VC$X2backup[,index], list(c(1:nj)), NULL)
          VC$X2[,index] <- zwblock2$x
          sds[index + nop1] <- 1
          qrmatrices2[[j]] <- zwblock[[2]][[1]]
          rm(zwblock, zwblock2)
          
          ##hv: If linear.equal is active on these groups:
          if(any(linear.equal[index])) {
            check <- sum(linear.equal[index])
            if(check != nj) 
              stop("Dummy coded variables belonging to a group must have linear.equal active or inactive, but not mixed")
            zw <- blockstand(rbind(VC$X1backup[, index], VC$X2backup[, index]),
                             list(c(1:nj)), NULL)
            n <- length(weights) ##hv: number of observations to split the matrix again
            VC$X1[,index] <- zw$x[1:n,]
            VC$X2[,index] <- zw$x[(n+1):(2*n),]
            qrmatrices1[[j]] <- qrmatrices2[[j]] <- zw[[2]][[1]]
            rm(check, zw)
          }
        }
      }
    }
    
    if(LASSO){
      
      ## 21.03.2023: Get new starting values
      y1 <- apply(VC$y1m, 1, function(x) max(x, na.rm = TRUE))
      y2 <- apply(VC$y2m, 1, function(x) max(x, na.rm = TRUE))
      X.here <- as.data.frame(VC$X1)
      glm1 <- glm(y1 ~ -1 + VC$X1, family = "poisson")
      glm2 <- glm(y2 ~ -1 + VC$X2, family = "poisson")
      
      nop1 <- VC$gp1
      nop2 <- VC$gp2
      nop3 <- VC$gp3
      
      start.glm <- c(coef(glm1), coef(glm2), start.v[(nop1+nop2+1):(length(start.v))])
      
      
      ##HV: new parts END
      
      ##hv: linear.equal added into SemiParBIV.fit
      ##hv: LASSO, LASSO.nu, LASSO.groups, beta.ML added into SemiParBIV.fit
      ##hv: starting values included (overwriting the calculated ones, if wished)
      if(!is.null(start.values))
        start.v <- start.values   # use start.values if given (in wrapper function i.e.)
      else
        start.v <- start.glm # glm start  values otherwise
    }
    
    SemiParFit <- SemiParBIV.fit(func.opt = func.opt, start.v = start.v, 
                                 rinit = rinit, rmax = rmax, iterlim = 100, iterlimsp = iterlimsp, 
                                 tolsp = tolsp, respvec = respvec, VC = VC, sp = sp, 
                                 qu.mag = qu.mag, linear.equal = linear.equal,
                                 LASSO = LASSO, LASSO.nu = LASSO.nu, LASSO.groups = LASSO.groups,
                                 beta.ML = beta.ML, threshold = threshold, xi = xi,
                                 conv.crit =  conv.crit, iterative = iterative, iter.weights = iter.weights)
    ##hv: arguments of SemiParBIV.fit added. END
    SemiParFit.p <- copulaReg.fit.post(SemiParFit = SemiParFit, 
                                       VC = VC, GAM)
    y1.m <- y1
    if (margins[1] == "LN") 
      y1.m <- exp(y1)
    y2.m <- y2
    if (margins[2] == "LN") 
      y2.m <- exp(y2)
    SemiParFit <- SemiParFit.p$SemiParFit
    if (gc.l == TRUE) 
      gc()
    cov.c(SemiParFit)
    gam1$call$data <- gam2$call$data <- gam3$call$data <- gam4$call$data <- gam5$call$data <- gam6$call$data <- gam7$call$data <- gam8$call$data <- cl$data
    
    #hv: part to set coefficients equal, that are forced to be equal. turned off for now.

    
    # if(any(linear.equal)){
    #   nop1 <- VC$gp1
    #   nop2 <- VC$gp2
    #   t1 <- SemiParFit$fit$argument[1:nop1]
    #   t2 <- SemiParFit$fit$argument[(nop1+1):(nop1+nop2)]
    #   t1[linear.equal] <- t2[linear.equal] <- 0.5 * (t1[linear.equal] + t2[linear.equal])
    #   SemiParFit$fit$argument[1:(nop1+nop2)] <- c(t1,t2)
    #   rm(t1, t2)
    # }
    
    ##hv: transformation back   ## update 22.03.2023: Added coefficients in third margin
    if(LASSO){
      SemiParFit$fit$argument <- SemiParFit$fit$argument / c(sds, 1, sds[names(SemiParFit$fit$argument)[(nop1+nop2+1):(nop1+nop2+nop3)]][-1])
      names(SemiParFit$fit$stan.coef) <- names(SemiParFit$fit$argument)
      #names(SemiParFit$fit$stan.coef)[length(SemiParFit$fit$stan.coef)] <- "theta.star"
      ##hv: transformation back for grouped lasso.
      ##hv: sds were set to 1 for groups. So add groups now:
      if(is.list(LASSO.groups)){
        for(j in length(LASSO.groups)){
          index <- LASSO.groups[[j]]
          SemiParFit$fit$argument[index] <- solve(qrmatrices1[[1]], SemiParFit$fit$argument[index])
          SemiParFit$fit$argument[index+nop1] <- solve(qrmatrices2[[1]], SemiParFit$fit$argument[index + nop1])
        }
      }
    }
    ##hv: END
    L <- list(fit = SemiParFit$fit, dataset = NULL, n = n, 
              gamlss1 = gamlss1, gamlss2 = gamlss2, formula = formula, 
              robust = FALSE, edf11 = SemiParFit.p$edf11, surv = surv, 
              gam1 = gam1, gam2 = gam2, gam3 = gam3, gam4 = gam4, 
              gam5 = gam5, gam6 = gam6, gam7 = gam7, gam8 = gam8, 
              coefficients = SemiParFit$fit$argument, coef.t = SemiParFit.p$coef.t, 
              iterlimsp = iterlimsp, weights = weights, cens1 = cens1, 
              cens2 = cens2, cens3 = cens3, sp = SemiParFit.p$sp, 
              iter.sp = SemiParFit$iter.sp, l.sp1 = l.sp1, l.sp2 = l.sp2, 
              l.sp3 = l.sp3, l.sp4 = l.sp4, l.sp5 = l.sp5, l.sp6 = l.sp6, 
              l.sp7 = l.sp7, l.sp8 = l.sp8, bl = bl, fp = fp, iter.if = SemiParFit$iter.if, 
              iter.inner = SemiParFit$iter.inner, theta = SemiParFit.p$theta, 
              theta.a = SemiParFit.p$theta.a, sigma21 = SemiParFit.p$sigma21, 
              sigma22 = SemiParFit.p$sigma22, sigma21.a = SemiParFit.p$sigma21.a, 
              sigma22.a = SemiParFit.p$sigma22.a, sigma1 = SemiParFit.p$sigma21, 
              sigma2 = SemiParFit.p$sigma22, sigma1.a = SemiParFit.p$sigma21.a, 
              sigma2.a = SemiParFit.p$sigma22.a, nu1 = SemiParFit.p$nu1, 
              nu2 = SemiParFit.p$nu2, nu1.a = SemiParFit.p$nu1.a, 
              nu2.a = SemiParFit.p$nu2.a, dof.a = SemiParFit.p$dof.a, 
              dof = SemiParFit.p$dof, X1 = X1, X2 = X2, X3 = X3, 
              X4 = X4, X5 = X5, X6 = X6, X7 = X7, X8 = X8, X1.d2 = X1.d2, 
              X2.d2 = X2.d2, X3.d2 = X3.d2, X4.d2 = X4.d2, X5.d2 = X5.d2, 
              X6.d2 = X6.d2, X7.d2 = X7.d2, X8.d2 = X8.d2, He = SemiParFit.p$He, 
              HeSh = SemiParFit.p$HeSh, Vb = SemiParFit.p$Vb, Ve = SemiParFit.p$Ve, 
              F = SemiParFit.p$F, F1 = SemiParFit.p$F1, t.edf = SemiParFit.p$t.edf, 
              edf = SemiParFit.p$edf, edf1 = SemiParFit.p$edf1, 
              edf2 = SemiParFit.p$edf2, edf3 = SemiParFit.p$edf3, 
              edf4 = SemiParFit.p$edf4, edf5 = SemiParFit.p$edf5, 
              edf6 = SemiParFit.p$edf6, edf7 = SemiParFit.p$edf7, 
              edf8 = SemiParFit.p$edf8, edf1.1 = SemiParFit.p$edf1.1, 
              edf1.2 = SemiParFit.p$edf1.2, edf1.3 = SemiParFit.p$edf1.3, 
              edf1.4 = SemiParFit.p$edf1.4, edf1.5 = SemiParFit.p$edf1.5, 
              edf1.6 = SemiParFit.p$edf1.6, edf1.7 = SemiParFit.p$edf1.7, 
              edf1.8 = SemiParFit.p$edf1.8, R = SemiParFit.p$R, 
              bs.mgfit = SemiParFit$bs.mgfit, conv.sp = SemiParFit$conv.sp, 
              wor.c = SemiParFit$wor.c, eta1 = SemiParFit$fit$eta1, 
              eta2 = SemiParFit$fit$eta2, etad = SemiParFit$fit$etad, 
              etas1 = SemiParFit$fit$etas1, etas2 = SemiParFit$fit$etas2, 
              y1 = y1.m, y2 = y2.m, BivD = BivD, margins = margins, 
              logLik = SemiParFit.p$logLik, nC = nC, respvec = respvec, 
              hess = TRUE, qu.mag = qu.mag, gp1 = gp1, gp2 = gp2, 
              gp3 = gp3, gp4 = gp4, gp5 = gp5, gp6 = gp6, gp7 = gp7, 
              gp8 = gp8, VC = VC, magpp = SemiParFit$magpp, gamlssfit = gamlssfit, 
              Cont = "YES", tau = SemiParFit.p$tau, tau.a = SemiParFit.p$tau.a, 
              l.flist = l.flist, v1 = v1, v2 = v2, triv = FALSE, 
              univar.gamlss = FALSE, BivD2 = BivD2, call = cl, 
              surv = surv, surv.flex = surv.flex, Vb.t = SemiParFit.p$Vb.t, 
              coef.t = SemiParFit.p$coef.t)
    if (BivD %in% BivD2) {
      L$teta1 <- SemiParFit$fit$teta1
      L$teta.ind1 <- SemiParFit$fit$teta.ind1
      L$teta2 <- SemiParFit$fit$teta2
      L$teta.ind2 <- SemiParFit$fit$teta.ind2
      L$Cop1 <- SemiParFit$fit$Cop1
      L$Cop2 <- SemiParFit$fit$Cop2
    }
    class(L) <- c("gjrm", "SemiParBIV")
  }
  L
}

environment(gjrm) <- asNamespace('GJRM')
reassignInPackage("gjrm", "GJRM", gjrm, keepOld = FALSE)


##hv: added linear.equal, LASSO, LASSO.nu, LASSO.groups, beta.ML and threshold
##hv: as above in gjrm().
SemiParBIV.fit <- function (func.opt, start.v, rinit, rmax, iterlim, iterlimsp, 
                            tolsp, respvec, VC, sp = NULL, qu.mag = NULL,
                            linear.equal = NULL, LASSO = FALSE, LASSO.nu = NULL,
                            LASSO.groups = NULL, beta.ML = NULL, threshold = 1e-03,
                            xi = 1e9, conv.crit = 0.01, iterative = TRUE, iter.weights = TRUE) 
{
  l.sp1 <- VC$l.sp1
  l.sp2 <- VC$l.sp2
  l.sp3 <- VC$l.sp3
  l.sp4 <- VC$l.sp4
  l.sp5 <- VC$l.sp5
  l.sp6 <- VC$l.sp6
  l.sp7 <- VC$l.sp7
  l.sp8 <- VC$l.sp8
  score.hist <- rp <- D <- L <- Sl.sfTemp <- St <- NULL
  l.splist <- list(l.sp1 = l.sp1, l.sp2 = l.sp2, l.sp3 = l.sp3, 
                   l.sp4 = l.sp4, l.sp5 = l.sp5, l.sp6 = l.sp6, l.sp7 = l.sp7, 
                   l.sp8 = l.sp8)
  if (!is.null(VC$sp.fixed)) 
    sp <- VC$sp.fixed
  ##hv: added is.null(linear.equal) below. If linear.equal is active, we
  ##hv: need pen() to create the penalty matrix S.
  if (((l.sp1 == 0 && l.sp2 == 0 && l.sp3 == 0 && l.sp4 == 0 && 
        l.sp5 == 0 && l.sp6 == 0 && l.sp7 == 0 && l.sp8 == 0) || 
       VC$fp == TRUE) & is.null(linear.equal)) 
    ps <- ps1 <- list(S.h = 0, S.h1 = 0, S.h2 = 0, qu.mag = NULL)
  else ps <- ps1 <- pen(qu.mag, sp, VC, univ = respvec$univ, 
                        l.splist, linear.equal, xi = xi)
  ##hv: To catch wrong calls with linear.equal while having splines.
  if (any(l.splist > 0) & any(linear.equal))
    stop("You specified splines and forced linear influences to be equal. This 
         is not supported at this point in time.")
  ##hv: END
  if (VC$triv == TRUE) {
    if (VC$penCor == "ridge") 
      qu.mag <- ps$qu.mag
    if (VC$penCor %in% c("lasso", "alasso")) 
      VC$sp <- sp
  }
  parsc <- rep(VC$parscale, length(start.v))
  sc <- TRUE
  
  ##hv: to implement the iterative penalty approach, wrap the fitting part:
  ##hv: new (longer part)
  #browser()
  if(any(linear.equal)){
    S.h.blank <- ps$S.h   ##hv: this is the S matrix with 1 and -1.
  }
  ##hv: number of parameters:
  nop1 <- VC$gp1
  nop2 <- VC$gp2
  nop3 <- VC$gp3
  
  if(LASSO & LASSO.nu > 0) {
    ##hv: Do not penalise intercepts:
    index <- c(1, nop1 + 1)
    K.blank <- diag(length(start.v))
    d.max <- nrow(K.blank)
    d <- nop1 + nop2 + 1 # start of third margin
    K.blank[d:d.max,d:d.max] <- 0 ##hv: no penalty on third margin (copula parameter)
    for(i in index){ #hv: no penalty for intercepts
      K.blank[i,i] <- 0
    }
  }
  difcrit <- 1
  while(difcrit > conv.crit){
    ##hv: penalty:
    #print(difcrit)
    ##hv: add penalty:
    params1 <- start.v[1:nop1]
    params2 <- start.v[(nop1 + 1):(nop1 + nop2)]
    limit <- min(nop1, nop2)
    
    if(any(linear.equal)) {
      pref <- numeric(limit)
      pref[linear.equal] <- abs(params1[1:limit] - params2[1:limit])[linear.equal]
      if(nop1 >= nop2)
        pref <- c(pref, rep(0, nop1 - nop2), pref, rep(0, nop3))
      if(nop1 < nop2)
        pref <- c(pref, pref, rep(0, nop2 - nop1), rep(0, nop3))
      if(!iter.weights)
        pref <- 1
      ps$S.h <- pref * S.h.blank
    }
    if(LASSO & LASSO.nu > 0){
      ##hv: with adaptive weights abs(beta.ML)
      K <- (1/(sqrt(start.v^2 + 1e-09) * abs(beta.ML))) * K.blank
      #K <- (1/(sqrt(start.v^2 + 1e-09))) * K.blank
      
      if(!is.null(LASSO.groups)){
        for (i in 1:length(LASSO.groups)){
          diag(K[LASSO.groups[[i]],LASSO.groups[[i]]]) <- 
            1/(sqrt(sum(start.v[LASSO.groups[[i]]]^2) + 1e-09)) * sqrt(length(LASSO.groups[[i]])) * (1 / abs(beta.ML[LASSO.groups[[i]]]))
          diag(K[nop1 + LASSO.groups[[i]], nop1 + LASSO.groups[[i]]]) <- 
            1/(sqrt(sum(start.v[nop1 + LASSO.groups[[i]]]^2) + 1e-09)) * sqrt(length(LASSO.groups[[i]])) * (1 / abs(beta.ML[LASSO.groups[[i]]]))
        }
      }
      #hv: remove entries for Intercepts
      K[c(1,nop1+1),c(1,nop1+1)] <- 0
      ps$S.h <- ps$S.h + LASSO.nu * K
    }
    ##hv: END
    fit <- fit1 <- try(trust(func.opt, start.v, rinit = rinit, 
                             rmax = rmax, parscale = parsc, respvec = respvec, VC = VC, 
                             ps = ps, blather = TRUE, iterlim = iterlim), silent = sc)
    if (class(fit) == "try-error" || is.null(fit$l)) {
      fit <- fit1 <- try(trust(func.opt, start.v, rinit = rinit, 
                               rmax = rmax, parscale = parsc, respvec = respvec, 
                               VC = VC, ps = ps, blather = TRUE, iterlim = iterlim/4), 
                         silent = sc)
      if (class(fit) == "try-error" || is.null(fit$l)) {
        fit <- fit1 <- try(trust(func.opt, start.v, rinit = rinit, 
                                 rmax = rmax, parscale = parsc, respvec = respvec, 
                                 VC = VC, ps = ps, blather = TRUE, iterlim = iterlim/10), 
                           silent = sc)
        if ((class(fit) == "try-error" || is.null(fit$l)) && 
            VC$gamlssfit == FALSE) 
          stop("It is not possible to fit the model.\nTry re-fitting the model and setting gamlssfit = TRUE if allowed.\nAlso, read the WARNINGS section of ?gjrm.")
        if ((class(fit) == "try-error" || is.null(fit$l)) && 
            VC$gamlssfit == TRUE) 
          stop("It is not possible to fit the model.\nRead the WARNINGS section of ?gjrm.")
      }
    }
    ##hv: new
    #browser()
    if(iterative & !(LASSO.nu == 0 & !any(linear.equal))){
      difcrit <- sum(abs(fit$argument[1:(nop1+nop2)] - start.v[1:(nop1+nop2)])) / sum(abs(start.v)[1:(nop1+nop2)])
      start.v <- fit$argument
      #print(difcrit)
    }
    else{
      difcrit <- 0 ##hv: Do not iterate multiple trust regions if linear.equal is not wished.
    }
  }
  #print(start.v[45])
  ##hv: END
  
  iter.if <- fit$iterations
  conv.sp <- iter.sp <- iter.inner <- bs.mgfit <- wor.c <- magpp <- NULL
  conv.sp <- TRUE
  if ((VC$fp == FALSE && is.null(VC$sp.fixed) && (l.sp1 != 
                                                  0 || l.sp2 != 0 || l.sp3 != 0 || l.sp4 != 0 || l.sp5 != 
                                                  0 || l.sp6 != 0 || l.sp7 != 0 || l.sp8 != 0))) {
    if (VC$sp.method == "perf") {
      stoprule.SP <- 1
      conv.sp <- TRUE
      iter.inner <- iter.sp <- 0
      while (stoprule.SP > tolsp) {
        fito <- fit$l
        o.ests <- c(fit$argument)
        spo <- sp
        wor.c <- working.comp(fit)
        if (VC$triv == TRUE && VC$penCor %in% c("lasso", 
                                                "alasso")) 
          qu.mag <- fit$qu.mag
        bs.mgfit <- try(magic(y = wor.c$Z, X = wor.c$X, 
                              sp = sp, S = qu.mag$Ss, off = qu.mag$off, rank = qu.mag$rank, 
                              gcv = FALSE, gamma = VC$infl.fac), silent = sc)
        if (class(bs.mgfit) == "try-error") {
          conv.sp <- FALSE
          break
        }
        if (any(is.na(bs.mgfit$sp)) == TRUE) {
          conv.sp <- FALSE
          break
        }
        sp <- bs.mgfit$sp
        if (!is.null(VC$sp.fixed)) 
          sp <- VC$sp.fixed
        iter.sp <- iter.sp + 1
        names(sp) <- names(spo)
        if (VC$triv == TRUE && VC$penCor %in% c("lasso", 
                                                "alasso")) 
          VC$sp <- sp
        ps <- pen(qu.mag, sp, VC, univ = respvec$univ, 
                  l.splist)
        fit <- try(trust(func.opt, o.ests, rinit = rinit, 
                         rmax = rmax, parscale = parsc, respvec = respvec, 
                         VC = VC, ps = ps, blather = TRUE, iterlim = iterlim), 
                   silent = sc)
        if (class(fit) == "try-error" || is.null(fit$l)) {
          conv.sp <- FALSE
          ps <- ps1
          fit <- try(trust(func.opt, c(fit1$argument), 
                           rinit = rinit, rmax = rmax, parscale = parsc, 
                           respvec = respvec, VC = VC, ps = ps, blather = TRUE, 
                           iterlim = iterlim), silent = sc)
          if ((class(fit) == "try-error" || is.null(fit$l)) && 
              VC$gamlssfit == FALSE) 
            stop("It is not possible to fit the model.\nTry re-fitting the model and setting gamlssfit = TRUE if allowed.\nAlso, read the WARNINGS section of ?gjrm.")
          if ((class(fit) == "try-error" || is.null(fit$l)) && 
              VC$gamlssfit == TRUE) 
            stop("It is not possible to fit the model.\nRead the WARNINGS section of ?gjrm.")
        }
        iter.inner <- iter.inner + fit$iterations
        if (iter.sp >= iterlimsp) {
          conv.sp <- FALSE
          break
        }
        stoprule.SP <- abs(fit$l - fito)/(0.1 + abs(fit$l))
      }
      if (VC$gc.l == TRUE) 
        gc()
      magpp <- magic.post.proc(wor.c$X, bs.mgfit)
    }
    if (VC$sp.method == "efs") {
      LDfun <- function(Hp, eigen.fix) {
        rank <- dim(Hp)[1]
        D <- diag(Hp)
        if (sum(!is.finite(D)) > 0) 
          stop("non finite values in Hessian")
        if (min(D) < 0) {
          Dthresh <- max(D) * sqrt(.Machine$double.eps)
          if (-min(D) < Dthresh) {
            indefinite <- FALSE
            D[D < Dthresh] <- Dthresh
          }
          else indefinite <- TRUE
        }
        else indefinite <- FALSE
        if (indefinite) {
          if (eigen.fix) {
            eh <- eigen(Hp, symmetric = TRUE)
            ev <- abs(eh$values)
            Hp <- eh$vectors %*% (ev * t(eh$vectors))
          }
          else {
            Ib <- diag(rank) * abs(min(D))
            Ip <- diag(rank) * abs(max(D) * .Machine$double.eps^0.5)
            Hp <- Hp + Ip + Ib
          }
          D <- rep(1, ncol(Hp))
          indefinite <- TRUE
        }
        else {
          D <- D^-0.5
          Hp <- D * t(D * Hp)
          Ip <- diag(rank) * .Machine$double.eps^0.5
        }
        L <- suppressWarnings(chol(Hp, pivot = TRUE))
        mult <- 1
        while (attr(L, "rank") < rank) {
          if (eigen.fix) {
            eh <- eigen(Hp, symmetric = TRUE)
            ev <- eh$values
            thresh <- max(min(ev[ev > 0]), max(ev) * 
                            1e-06) * mult
            mult <- mult * 10
            ev[ev < thresh] <- thresh
            Hp <- eh$vectors %*% (ev * t(eh$vectors))
            L <- suppressWarnings(chol(Hp, pivot = TRUE))
          }
          else {
            L <- suppressWarnings(chol(Hp + Ip, pivot = TRUE))
            Ip <- Ip * 100
          }
          indefinite <- TRUE
        }
        list(L = L, D = D)
      }
      stoprule.SP <- 1
      conv.sp <- TRUE
      iter.inner <- iter.sp <- 0
      controlEFS <- list(efs.lspmax = 15, eps = 1e-07, 
                         tol = 1e-06, tiny = .Machine$double.eps^0.5, 
                         efs.tol = 0.1)
      score.hist <- rep(0, 200)
      mult <- 1
      lsp <- log(sp)
      gamma <- 1
      Mp <- -1
      eigen.fix <- FALSE
      Sl.termMult <- getFromNamespace("Sl.termMult", 
                                      "mgcv")
      ldetS <- getFromNamespace("ldetS", "mgcv")
      Mp <- ncol(totalPenaltySpace(qu.mag$Ss, NULL, qu.mag$off, 
                                   length(fit$argument))$Z)
      Sl.sfTemp <- VC$Sl.sf
      for (i in 1:length(Sl.sfTemp)) Sl.sfTemp[[i]]$D <- solve(Sl.sfTemp[[i]]$D)
      for (iter in 1:200) {
        o.ests <- c(fit$argument)
        rp <- ldetS(VC$Sl.sf, rho = lsp, fixed = rep(FALSE, 
                                                     length(lsp)), np = length(fit$argument), root = TRUE)
        o.estsStar <- Sl.initial.repara(Sl.sfTemp, o.ests, 
                                        inverse = TRUE)
        Vb <- Sl.initial.repara(Sl.sfTemp, PDef(fit$hessian)$res.inv, 
                                inverse = TRUE)
        LD <- LDfun(PDef(Vb)$res.inv, eigen.fix)
        L <- LD$L
        D <- LD$D
        ipiv <- piv <- attr(L, "pivot")
        p <- length(piv)
        ipiv[piv] <- 1:p
        Vb <- crossprod(forwardsolve(t(L), diag(D, nrow = p)[piv, 
                                                             , drop = FALSE])[ipiv, , drop = FALSE])
        Vb <- Sl.repara(rp$rp, Vb, inverse = TRUE)
        SVb <- Sl.termMult(VC$Sl.sf, Vb)
        trVS <- rep(0, length(SVb))
        for (i in 1:length(SVb)) {
          ind <- attr(SVb[[i]], "ind")
          trVS[i] <- sum(diag(SVb[[i]][, ind]))
        }
        start <- Sl.repara(rp$rp, o.estsStar)
        Sb <- Sl.termMult(VC$Sl.sf, start, full = TRUE)
        bSb <- rep(0, length(Sb))
        for (i in 1:length(Sb)) bSb[i] <- sum(start * 
                                                Sb[[i]])
        S1 <- rp$ldet1
        a <- pmax(controlEFS$tiny, S1 * exp(-lsp) - trVS)
        r <- a/pmax(controlEFS$tiny, bSb)
        r[a == 0 & bSb == 0] <- 1
        r[!is.finite(r)] <- 1e+06
        lsp1 <- pmin(lsp + log(r) * mult, controlEFS$efs.lspmax)
        max.step <- max(abs(lsp1 - lsp))
        ldetHp <- 2 * sum(log(diag(L))) - 2 * sum(log(D))
        old.reml <- -as.numeric((-fit$l - drop(t(fit$argument) %*% 
                                                 fit$S.h %*% fit$argument)/2)/gamma + rp$ldetS/2 - 
                                  ldetHp/2 + Mp * (log(2 * pi)/2) - log(gamma)/2)
        sp1 <- exp(lsp1)
        names(sp1) <- names(lsp1)
        if (!is.null(VC$sp.fixed)) 
          sp1 <- VC$sp.fixed
        if (VC$triv == TRUE && VC$penCor %in% c("lasso", 
                                                "alasso")) 
          VC$sp <- sp1
        ps <- pen(qu.mag, sp1, VC, univ = respvec$univ, 
                  l.splist)
        fit <- try(trust(func.opt, o.ests, rinit = rinit, 
                         rmax = rmax, parscale = parsc, respvec = respvec, 
                         VC = VC, ps = ps, blather = TRUE, iterlim = iterlim), 
                   silent = sc)
        if (class(fit) == "try-error" || is.null(fit$l)) {
          conv.sp <- FALSE
          ps <- ps1
          fit <- try(trust(func.opt, c(fit1$argument), 
                           rinit = rinit, rmax = rmax, parscale = parsc, 
                           respvec = respvec, VC = VC, ps = ps, blather = TRUE, 
                           iterlim = iterlim), silent = sc)
          if ((class(fit) == "try-error" || is.null(fit$l)) && 
              VC$gamlssfit == FALSE) 
            stop("It is not possible to fit the model.\nTry re-fitting the model and setting gamlssfit = TRUE if allowed.\nAlso, read the WARNINGS section of ?gjrm.")
          if ((class(fit) == "try-error" || is.null(fit$l)) && 
              VC$gamlssfit == TRUE) 
            stop("It is not possible to fit the model.\nRead the WARNINGS section of ?gjrm.")
        }
        iter.inner <- iter.inner + fit$iterations
        rp <- ldetS(VC$Sl.sf, rho = lsp1, fixed = rep(FALSE, 
                                                      length(lsp1)), np = length(fit$argument), root = TRUE)
        Vb <- Sl.initial.repara(Sl.sfTemp, PDef(fit$hessian)$res.inv, 
                                inverse = TRUE)
        LD <- LDfun(PDef(Vb)$res.inv, eigen.fix)
        L <- LD$L
        D <- LD$D
        ldetHp <- 2 * sum(log(diag(L))) - 2 * sum(log(D))
        fit$REML <- -as.numeric((-fit$l - drop(t(fit$argument) %*% 
                                                 fit$S.h %*% fit$argument)/2)/gamma + rp$ldetS/2 - 
                                  ldetHp/2 + Mp * (log(2 * pi)/2) - log(gamma)/2)
        if (fit$REML <= old.reml) {
          if (max.step < 0.05) {
            lsp2 <- pmin(lsp + log(r) * mult * 2, 12)
            sp2 <- exp(lsp2)
            names(sp2) <- names(lsp2)
            if (!is.null(VC$sp.fixed)) 
              sp2 <- VC$sp.fixed
            if (VC$triv == TRUE && VC$penCor %in% c("lasso", 
                                                    "alasso")) 
              VC$sp <- sp2
            ps <- pen(qu.mag, sp2, VC, univ = respvec$univ, 
                      l.splist)
            fit2 <- try(trust(func.opt, o.ests, rinit = rinit, 
                              rmax = rmax, parscale = parsc, respvec = respvec, 
                              VC = VC, ps = ps, blather = TRUE, iterlim = iterlim), 
                        silent = sc)
            if (class(fit2) == "try-error" || is.null(fit2$l)) {
              conv.sp <- FALSE
              ps <- ps1
              fit2 <- try(trust(func.opt, c(fit1$argument), 
                                rinit = rinit, rmax = rmax, parscale = parsc, 
                                respvec = respvec, VC = VC, ps = ps, 
                                blather = TRUE, iterlim = iterlim), silent = sc)
              if ((class(fit2) == "try-error" || 
                   is.null(fit2$l)) && VC$gamlssfit == FALSE) 
                stop("It is not possible to fit the model.\nTry re-fitting the model and setting gamlssfit = TRUE if allowed.\nAlso, read the WARNINGS section of ?gjrm.")
              if ((class(fit2) == "try-error" || 
                   is.null(fit2$l)) && VC$gamlssfit == TRUE) 
                stop("It is not possible to fit the model.\nRead the WARNINGS section of ?gjrm.")
            }
            iter.inner <- iter.inner + fit2$iterations
            rp <- ldetS(VC$Sl.sf, rho = lsp2, fixed = rep(FALSE, 
                                                          length(lsp2)), np = length(fit$argument), 
                        root = TRUE)
            Vb <- Sl.initial.repara(Sl.sfTemp, PDef(fit2$hessian)$res.inv, 
                                    inverse = TRUE)
            LD <- LDfun(PDef(Vb)$res.inv, eigen.fix)
            L <- LD$L
            D <- LD$D
            ldetHp <- 2 * sum(log(diag(L))) - 2 * sum(log(D))
            fit2$REML <- -as.numeric((-fit2$l - drop(t(fit2$argument) %*% 
                                                       fit2$S.h %*% fit2$argument)/2)/gamma + 
                                       rp$ldetS/2 - ldetHp/2 + Mp * (log(2 * pi)/2) - 
                                       log(gamma)/2)
            if (fit2$REML < fit$REML) {
              fit <- fit2
              lsp <- lsp2
              mult <- mult * 2
            }
            else {
              lsp <- lsp1
            }
          }
          else lsp <- lsp1
        }
        else {
          while (fit$REML > old.reml && mult > 1) {
            mult <- mult/2
            lsp1 <- pmin(lsp + log(r) * mult, controlEFS$efs.lspmax)
            sp1 <- exp(lsp1)
            names(sp1) <- names(lsp1)
            if (!is.null(VC$sp.fixed)) 
              sp1 <- VC$sp.fixed
            if (VC$triv == TRUE && VC$penCor %in% c("lasso", 
                                                    "alasso")) 
              VC$sp <- sp1
            ps <- pen(qu.mag, sp1, VC, univ = respvec$univ, 
                      l.splist)
            fit <- try(trust(func.opt, o.ests, rinit = rinit, 
                             rmax = rmax, parscale = parsc, respvec = respvec, 
                             VC = VC, ps = ps, blather = TRUE, iterlim = iterlim), 
                       silent = sc)
            if (class(fit) == "try-error" || is.null(fit$l)) {
              conv.sp <- FALSE
              ps <- ps1
              fit <- try(trust(func.opt, c(fit1$argument), 
                               rinit = rinit, rmax = rmax, parscale = parsc, 
                               respvec = respvec, VC = VC, ps = ps, 
                               blather = TRUE, iterlim = iterlim), silent = sc)
              if ((class(fit) == "try-error" || 
                   is.null(fit$l)) && VC$gamlssfit == FALSE) 
                stop("It is not possible to fit the model.\nTry re-fitting the model and setting gamlssfit = TRUE if allowed.\nAlso, read the WARNINGS section of ?gjrm.")
              if ((class(fit) == "try-error" || 
                   is.null(fit$l)) && VC$gamlssfit == TRUE) 
                stop("It is not possible to fit the model.\nRead the WARNINGS section of ?gjrm.")
            }
            iter.inner <- iter.inner + fit$iterations
            rp <- ldetS(VC$Sl.sf, rho = lsp1, fixed = rep(FALSE, 
                                                          length(lsp1)), np = length(fit$argument), 
                        root = TRUE)
            Vb <- Sl.initial.repara(Sl.sfTemp, PDef(fit$hessian)$res.inv, 
                                    inverse = TRUE)
            LD <- LDfun(PDef(Vb)$res.inv, eigen.fix)
            L <- LD$L
            D <- LD$D
            ldetHp <- 2 * sum(log(diag(L))) - 2 * sum(log(D))
            fit$REML <- -as.numeric((-fit$l - drop(t(fit$argument) %*% 
                                                     fit$S.h %*% fit$argument)/2)/gamma + rp$ldetS/2 - 
                                      ldetHp/2 + Mp * (log(2 * pi)/2) - log(gamma)/2)
          }
          lsp <- lsp1
          if (mult < 1) 
            mult <- 1
        }
        score.hist[iter] <- fit$REML
        if (iter > 3 && max.step < 0.05 && max(abs(diff(score.hist[(iter - 
                                                                    3):iter]))) < controlEFS$efs.tol) 
          break
        if (iter == 1) 
          old.ll <- fit$l
        else {
          if (abs(old.ll - fit$l) < 100 * controlEFS$eps * 
              abs(fit$l)) 
            break
          old.ll <- fit$l
        }
      }
      sp <- exp(lsp)
      iter.sp <- iter
      if (iter > 200) 
        conv.sp <- FALSE
      else conv.sp <- TRUE
      if (VC$gc.l == TRUE) 
        gc()
      St <- crossprod(rp$E)
    }
  }
  else {
    wor.c <- working.comp(fit)
    bs.mgfit <- magic(wor.c$Z, wor.c$X, numeric(0), list(), 
                      numeric(0))
    magpp <- magic.post.proc(wor.c$X, bs.mgfit)
  }
  rm(fit1, ps1)
  
  ##hv: save raw estimates to carry them over:
  fit$stan.coef <- fit$argument
  
  ##hv: new longer part. Set coefficients below threshold to zero.
  if(LASSO.nu > 0){
    welche1 <- rep(FALSE, nop1 + nop2 + nop3)
    welche2 <- rep(FALSE, nop1 + nop2 + nop3)
    ##hv: to account for groups:
    if (is.list(LASSO.groups)){
      for(i in 1:length(LASSO.groups)){
        index <- LASSO.groups[[i]]
        
        if(all(linear.equal[index]))    ## groups and linear equal
        {
          welche1[index] <- TRUE
          welche1[index + nop1] <- TRUE

          if(all(abs(fit$argument[index]) < threshold) &
             all(abs(fit$argument[index + nop1]) < threshold)){
            fit$argument[c(index, index+nop1)] <- 0
          }
        }
        else    # groups without linear.equal
        {
          welche1[index] <- TRUE
          welche1[index + nop1] <- TRUE
          if(all(abs(fit$argument[index]) < threshold)){
            fit$argument[index] <- 0
          }
          if(all(abs(fit$argument[index + nop1]) < threshold)){
            fit$argument[index + nop1] <- 0
          }
        }
      }
    }
    
    ##hv: to account for linear.equal. Do not set to zero (yet) if linear.equal
    ##hv: is active on the coefficient.
    welche2[1:nop1][linear.equal] <- TRUE
    welche2[(nop1+1):(2*nop1)][linear.equal] <- TRUE
    
    ##hv: remove from welche2 those covariates, that have been handled above:
    welche2[welche1] <- FALSE
    ##hv: now to coefficient with linear.equal active:
    n.here <- length(fit$argument[welche2]) #hv: is always an even number
    indic <- (abs(fit$argument[welche2][1:(n.here/2)]) < threshold) & 
      (abs(fit$argument[welche2][(n.here/2 + 1):n.here]) < threshold)
    indic <- c(indic, indic)
    fit$argument[welche2][indic] <- 0
    
    ##hv: rest of variables, that are not linear.equal and not in groups.
    welche <- welche1 | welche2  ##hv: these ones are already done
    welche[names(fit$argument) == "(Intercept)"] <- TRUE ##hv: Intercepts are not set to zero
    speicher <- fit$argument[(length(fit$argument)-nop3+1):length(fit$argument)]
    fit$argument[(!welche) & abs(fit$argument) < threshold] <- 0
    fit$argument[(length(fit$argument)-nop3+1):length(fit$argument)] <- speicher
    ##hv: (quick solution to save the coefficients corresponding to eq3)
    
  }
  
  ##hv: save raw estimates with 0 sets to check for degrees of freedom
  fit$stan.coef0 <- fit$argument
  
  ##hv: END
  
  
  list(fit = fit, score.hist = score.hist, iter.if = iter.if, 
       sp.method = VC$sp.method, conv.sp = conv.sp, iter.sp = iter.sp, 
       iter.inner = iter.inner, bs.mgfit = bs.mgfit, wor.c = wor.c, 
       sp = sp, magpp = magpp, rp = rp$rp, Sl = VC$Sl.sf, D = D, 
       L = L, Sl.sfTemp = Sl.sfTemp, St = St)
}

environment(SemiParBIV.fit) <- asNamespace('GJRM')
reassignInPackage("SemiParBIV.fit", "GJRM", SemiParBIV.fit, keepOld = FALSE)


##hv: Added linear.equal as above and penalty \xi from van der Wurp et al. 2020.
##hv: may be tuned, but works well as soon as it is big enough.
##hv: The changes here in pen() are only applied, when linear.equal is active.
##hv: the LASSO-penalty structure was added in SemiParBIV.fit
pen <- function (qu.mag, sp, VC, univ, l.splist, linear.equal = NULL, xi = 1e9) 
{
  ma1 <- matrix(0, VC$gp1, VC$gp1)
  if (l.splist$l.sp1 == 0) 
    EQ1P <- adiag(ma1)
  if (l.splist$l.sp1 != 0) {
    ind <- 1:l.splist$l.sp1
    offtemp <- as.numeric(as.factor(qu.mag$off[ind]))
    S1 <- mapply("*", qu.mag$Ss[ind], sp[ind], SIMPLIFY = FALSE)
    if (length(unique(offtemp)) != length(offtemp)) 
      S1 <- SS(offtemp, S1)
    S1 <- do.call(adiag, lapply(S1, unlist))
    EQ1P <- adiag(ma1, S1)
  }
  if (is.null(VC$gp2.inf)) 
    ma2 <- matrix(0, VC$gp2, VC$gp2)
  else ma2 <- matrix(0, VC$gp2.inf, VC$gp2.inf)
  if (l.splist$l.sp2 == 0) 
    EQ2P <- adiag(ma2)
  if (l.splist$l.sp2 != 0) {
    ind <- (l.splist$l.sp1 + 1):(l.splist$l.sp1 + l.splist$l.sp2)
    offtemp <- as.numeric(as.factor(qu.mag$off[ind]))
    S2 <- mapply("*", qu.mag$Ss[ind], sp[ind], SIMPLIFY = FALSE)
    if (length(unique(offtemp)) != length(offtemp)) 
      S2 <- SS(offtemp, S2)
    S2 <- do.call(adiag, lapply(S2, unlist))
    EQ2P <- adiag(ma2, S2)
  }
  if (!is.null(VC$gp3)) {
    EQ4P <- EQ5P <- EQ6P <- EQ7P <- EQ8P <- NULL
    ma3 <- matrix(0, VC$gp3, VC$gp3)
    if (l.splist$l.sp3 == 0) 
      EQ3P <- adiag(ma3)
    if (l.splist$l.sp3 != 0) {
      ind <- (l.splist$l.sp1 + l.splist$l.sp2 + 1):(l.splist$l.sp1 + 
                                                      l.splist$l.sp2 + l.splist$l.sp3)
      offtemp <- as.numeric(as.factor(qu.mag$off[ind]))
      S3 <- mapply("*", qu.mag$Ss[ind], sp[ind], 
                   SIMPLIFY = FALSE)
      if (length(unique(offtemp)) != length(offtemp)) 
        S3 <- SS(offtemp, S3)
      S3 <- do.call(adiag, lapply(S3, unlist))
      EQ3P <- adiag(ma3, S3)
    }
    if (!is.null(VC$gp4)) {
      ma4 <- matrix(0, VC$gp4, VC$gp4)
      if (l.splist$l.sp4 == 0) 
        EQ4P <- adiag(ma4)
      if (l.splist$l.sp4 != 0) {
        ind <- (l.splist$l.sp1 + l.splist$l.sp2 + l.splist$l.sp3 + 
                  1):(l.splist$l.sp1 + l.splist$l.sp2 + l.splist$l.sp3 + 
                        l.splist$l.sp4)
        offtemp <- as.numeric(as.factor(qu.mag$off[ind]))
        S4 <- mapply("*", qu.mag$Ss[ind], sp[ind], 
                     SIMPLIFY = FALSE)
        if (length(unique(offtemp)) != length(offtemp)) 
          S4 <- SS(offtemp, S4)
        S4 <- do.call(adiag, lapply(S4, unlist))
        EQ4P <- adiag(ma4, S4)
      }
    }
    if (!is.null(VC$gp5)) {
      ma5 <- matrix(0, VC$gp5, VC$gp5)
      if (l.splist$l.sp5 == 0) 
        EQ5P <- adiag(ma5)
      if (l.splist$l.sp5 != 0) {
        ind <- (l.splist$l.sp1 + l.splist$l.sp2 + l.splist$l.sp3 + 
                  l.splist$l.sp4 + 1):(l.splist$l.sp1 + l.splist$l.sp2 + 
                                         l.splist$l.sp3 + l.splist$l.sp4 + l.splist$l.sp5)
        offtemp <- as.numeric(as.factor(qu.mag$off[ind]))
        S5 <- mapply("*", qu.mag$Ss[ind], sp[ind], 
                     SIMPLIFY = FALSE)
        if (length(unique(offtemp)) != length(offtemp)) 
          S5 <- SS(offtemp, S5)
        S5 <- do.call(adiag, lapply(S5, unlist))
        EQ5P <- adiag(ma5, S5)
      }
    }
    if (!is.null(VC$gp6)) {
      ma6 <- matrix(0, VC$gp6, VC$gp6)
      if (l.splist$l.sp6 == 0) 
        EQ6P <- adiag(ma6)
      if (l.splist$l.sp6 != 0) {
        ind <- (l.splist$l.sp1 + l.splist$l.sp2 + l.splist$l.sp3 + 
                  l.splist$l.sp4 + l.splist$l.sp5 + 1):(l.splist$l.sp1 + 
                                                          l.splist$l.sp2 + l.splist$l.sp3 + l.splist$l.sp4 + 
                                                          l.splist$l.sp5 + l.splist$l.sp6)
        offtemp <- as.numeric(as.factor(qu.mag$off[ind]))
        S6 <- mapply("*", qu.mag$Ss[ind], sp[ind], 
                     SIMPLIFY = FALSE)
        if (length(unique(offtemp)) != length(offtemp)) 
          S6 <- SS(offtemp, S6)
        S6 <- do.call(adiag, lapply(S6, unlist))
        EQ6P <- adiag(ma6, S6)
      }
    }
    if (!is.null(VC$gp7)) {
      ma7 <- matrix(0, VC$gp7, VC$gp7)
      if (l.splist$l.sp7 == 0) 
        EQ7P <- adiag(ma7)
      if (l.splist$l.sp7 != 0) {
        ind <- (l.splist$l.sp1 + l.splist$l.sp2 + l.splist$l.sp3 + 
                  l.splist$l.sp4 + l.splist$l.sp5 + l.splist$l.sp6 + 
                  1):(l.splist$l.sp1 + l.splist$l.sp2 + l.splist$l.sp3 + 
                        l.splist$l.sp4 + l.splist$l.sp5 + l.splist$l.sp6 + 
                        l.splist$l.sp7)
        offtemp <- as.numeric(as.factor(qu.mag$off[ind]))
        S7 <- mapply("*", qu.mag$Ss[ind], sp[ind], 
                     SIMPLIFY = FALSE)
        if (length(unique(offtemp)) != length(offtemp)) 
          S7 <- SS(offtemp, S7)
        S7 <- do.call(adiag, lapply(S7, unlist))
        EQ7P <- adiag(ma7, S7)
      }
    }
    if (!is.null(VC$gp8)) {
      ma8 <- matrix(0, VC$gp8, VC$gp8)
      if (l.splist$l.sp8 == 0) 
        EQ8P <- adiag(ma8)
      if (l.splist$l.sp8 != 0) {
        ind <- (l.splist$l.sp1 + l.splist$l.sp2 + l.splist$l.sp3 + 
                  l.splist$l.sp4 + l.splist$l.sp5 + l.splist$l.sp6 + 
                  l.splist$l.sp7 + 1):(l.splist$l.sp1 + l.splist$l.sp2 + 
                                         l.splist$l.sp3 + l.splist$l.sp4 + l.splist$l.sp5 + 
                                         l.splist$l.sp6 + l.splist$l.sp7 + l.splist$l.sp8)
        offtemp <- as.numeric(as.factor(qu.mag$off[ind]))
        S8 <- mapply("*", qu.mag$Ss[ind], sp[ind], 
                     SIMPLIFY = FALSE)
        if (length(unique(offtemp)) != length(offtemp)) 
          S8 <- SS(offtemp, S8)
        S8 <- do.call(adiag, lapply(S8, unlist))
        EQ8P <- adiag(ma8, S8)
      }
    }
  }
  else {
    if (VC$univ.gamls == FALSE) {
      if (VC$margins[1] %in% c(VC$m2, VC$m3) && VC$margins[2] %in% 
          c(VC$m2, VC$m3) && VC$BivD == "T") {
        if (VC$margins[1] %in% VC$m2 && VC$margins[2] %in% 
            VC$m2) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- 0
          EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% VC$m3 && VC$margins[2] %in% 
            VC$m3) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- 0
          EQ7P <- EQ8P <- 0
        }
        if (VC$margins[1] %in% VC$m2 && VC$margins[2] %in% 
            VC$m3) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- 0
          EQ7P <- 0
          EQ8P <- NULL
        }
        if (VC$margins[1] %in% VC$m3 && VC$margins[2] %in% 
            VC$m2) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- 0
          EQ7P <- 0
          EQ8P <- NULL
        }
      }
      else {
        if (VC$margins[1] %in% c(VC$bl) && VC$Model != 
            "BPO0") {
          EQ3P <- 0
          EQ4P <- NULL
          EQ5P <- NULL
          EQ6P <- EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% c(VC$bl, VC$m1d) && VC$margins[2] %in% 
            c(VC$bl, VC$m1d)) {
          EQ3P <- 0
          EQ4P <- NULL
          EQ5P <- NULL
          EQ6P <- EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% c(VC$bl, VC$m1d) && VC$margins[2] %in% 
            VC$m2d) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- NULL
          EQ6P <- EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% c(VC$bl, VC$m1d) && VC$margins[2] %in% 
            VC$m2) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- NULL
          EQ6P <- EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% c(VC$bl, VC$m1d) && VC$margins[2] %in% 
            VC$m3) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% c(VC$m2, VC$m2d) && VC$margins[2] %in% 
            c(VC$m2, VC$m2d)) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- NULL
          EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% c(VC$m2, VC$m2d) && VC$margins[2] %in% 
            c(VC$bl)) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- NULL
          EQ6P <- NULL
          EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% c(VC$m3) && VC$margins[2] %in% 
            c(VC$bl)) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- NULL
          EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% VC$m3 && VC$margins[2] %in% 
            VC$m3) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- 0
          EQ7P <- 0
          EQ8P <- NULL
        }
        if (VC$margins[1] %in% c(VC$m2, VC$m2d) && VC$margins[2] %in% 
            VC$m3) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- 0
          EQ7P <- EQ8P <- NULL
        }
        if (VC$margins[1] %in% VC$m3 && VC$margins[2] %in% 
            c(VC$m2, VC$m2d)) {
          EQ3P <- 0
          EQ4P <- 0
          EQ5P <- 0
          EQ6P <- 0
          EQ7P <- EQ8P <- NULL
        }
        if (VC$Model == "B" && !is.null(VC$theta.fx)) {
          EQ3P <- EQ4P <- EQ5P <- EQ6P <- EQ7P <- EQ8P <- NULL
        }
        if (VC$Model == "BPO0") {
          EQ3P <- EQ4P <- EQ5P <- EQ6P <- EQ7P <- EQ8P <- NULL
        }
      }
    }
  }
  if (VC$triv == FALSE) {
    if (univ == 0) 
      S.h <- adiag(EQ1P, EQ2P, EQ3P, EQ4P, EQ5P, EQ6P, 
                   EQ7P, EQ8P)
    if (univ == 2) {
      if (VC$margins[1] %in% c(VC$m1d, VC$bl)) 
        S.h <- adiag(EQ1P)
      if (VC$margins[1] %in% c(VC$bl) && !is.null(VC$gp2.inf)) 
        S.h <- adiag(EQ1P, EQ2P)
      if (VC$margins[1] %in% c(VC$m2, VC$m2d)) 
        S.h <- adiag(EQ1P, EQ2P)
      if (VC$margins[1] %in% VC$m3) 
        S.h <- adiag(EQ1P, EQ2P, EQ3P)
    }
  }
  if (VC$triv == TRUE) {
    S.h <- adiag(EQ1P, EQ2P, EQ3P)
    if (VC$penCor %in% c("unpen") && VC$l.flist == 
        3) 
      S.h <- adiag(S.h, matrix(0, 3, 3))
    if (VC$penCor %in% c("unpen") && VC$l.flist == 
        6) 
      S.h <- adiag(EQ1P, EQ2P, EQ3P, EQ4P, EQ5P, EQ6P)
    if (VC$penCor %in% c("ridge")) {
      A <- diag(c(1, 1, 1))
      if (VC$l.sp1 == 0 && VC$l.sp2 == 0 && VC$l.sp3 == 
          0) 
        qu.mag$Ss[[1]] <- A
      if (VC$l.sp1 != 0 || VC$l.sp2 != 0 || VC$l.sp3 != 
          0) 
        qu.mag$Ss[[length(qu.mag$Ss) + 1]] <- A
      S.h <- adiag(S.h, sp[length(sp)] * qu.mag$Ss[[length(qu.mag$Ss)]])
    }
  }
  #browser()
  ##hv: new from here. Include penalty for linear differences. Currently not
  ##hv: working with Splines.
  ##hv: reminder: linear.equal is assumed to have lengths nop1, number of coefficients
  ##hv: in first regression margin. Intercept included.
  if(any(linear.equal)) {
    #browser()
    nop <- sum(c(VC$gp1, VC$gp2, VC$gp3))
    nop1 <- VC$gp1
    nop2 <- VC$gp2
    nop3 <- VC$gp3
    A <- matrix(rep(0,nop^2), nrow=nop)
    ##hv: in case eq2 is longer than eq1:
    linear.equal2 <- c(linear.equal, rep(FALSE, nop2-nop1))
    diag(A)[c(linear.equal, linear.equal2, FALSE)] <- 1
    for (i in 1:nop1) {
      if(A[i,i] == 1) {
        A[i,i+nop1] <- -1
        A[i+nop1,i] <- -1
      }
    }
    A <- A * xi
    if(exists("S.h")) {S.h <- S.h + A}
    else {S.h <- A}
  }
  ##hv: new until here.
  
  list(S.h = S.h, qu.mag = qu.mag)
}

environment(pen) <- asNamespace("GJRM")
reassignInPackage("pen", "GJRM", pen, keepOld = FALSE)

