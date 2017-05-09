# https://github.com/wch/r-source/commit/7ada5f74f96dd8214e4bf0c43fda5757d348e74e

qr.lm <- function(x, ...) {
  if(is.null(r <- x$qr))
    stop("lm object does not have a proper 'qr' component.
         Rank zero or should not have used lm(.., qr=FALSE).")
  r
}

## code originally from John Maindonald 26Jul2000
custom_predict_lm <-
  function(object, newdata, model_mat)#,se.fit = FALSE, scale = NULL, df = Inf,
           #interval = "none",
           #level = .95,  type = c("response", "terms"),
           #terms = NULL, na.action = na.pass, pred.var = res.var/weights,
           #weights = 1, ...)
  {
    tt <- terms(object)
    if(!inherits(object, "lm"))
      warning("calling predict.lm(<fake-lm-object>) ...")
    # if(missing(newdata) || is.null(newdata)) {
    #   mm <- X <- model.matrix(object)
    #   mmDone <- TRUE
    #   offset <- object$offset
    # }
    # else {

    # MOVE INTO LAPPLY FUNCTION
    # Terms <- delete.response(tt)
    # m <- model.frame(Terms, newdata, na.action = na.action,
    #                  xlev = object$xlevels)
    # if(!is.null(cl <- attr(Terms, "dataClasses"))) .checkMFClasses(cl, m)
    # X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(model_mat))
    if (!is.null(off.num <- attr(tt, "offset")))
      for(i in off.num)
        offset <- offset + eval(attr(tt, "variables")[[i+1]], newdata)
    if (!is.null(object$call$offset))
      offset <- offset + eval(object$call$offset, newdata)
    # mmDone <- FALSE
    # }
    # n <- length(object$residuals) # NROW(qr(object)$qr)
    p <- object$rank
    p1 <- seq_len(p)
    piv <- if(p) qr.lm(object)$pivot[p1]
    if(p < ncol(model_mat) && !(missing(newdata) || is.null(newdata)))
      warning("prediction from a rank-deficient fit may be misleading")
    ### NB: Q[p1,] %*% model_mat[,piv] = R[p1,p1]
    beta <- object$coefficients
    predictor <- drop(model_mat[, piv, drop = FALSE] %*% beta[piv])
    if (!is.null(offset))
      predictor <- predictor + offset

    # interval <- match.arg(interval)
    # if (interval == "prediction") {
    #   if (missing(newdata))
    #     warning("predictions on current data refer to _future_ responses\n")
    #   if (missing(newdata) && missing(weights)) {
    #     w <-  weights.default(object)
    #     if (!is.null(w)) {
    #       weights <- w
    #       warning("assuming prediction variance inversely proportional to weights used for fitting\n")
    #     }
    #   }
    #   if (!missing(newdata) && missing(weights) && !is.null(object$weights) && missing(pred.var))
    #     warning("Assuming constant prediction variance even though model fit is weighted\n")
    #   if (inherits(weights, "formula")){
    #     if (length(weights) != 2L)
    #       stop("'weights' as formula should be one-sided")
    #     d <- if(missing(newdata) || is.null(newdata))
    #       model.frame(object)
    #     else
    #       newdata
    #     weights <- eval(weights[[2L]], d, environment(weights))
    #   }
    # }

    # type <- match.arg(type)
    # if(se.fit || interval != "none") {
    #   ## w is needed for interval = "confidence"
    #   w <- object$weights
    #   res.var <-
    #     if (is.null(scale)) {
    #       r <- object$residuals
    #       rss <- sum(if(is.null(w)) r^2 else r^2 * w)
    #       df <- object$df.residual
    #       rss/df
    #     } else scale^2
    #   if(type != "terms") {
    #     if(p > 0) {
    #       XRinv <-
    #         if(missing(newdata) && is.null(w))
    #           qr.Q(qr.lm(object))[, p1, drop = FALSE]
    #       else
    #         X[, piv] %*% qr.solve(qr.R(qr.lm(object))[p1, p1])
    #       #	NB:
    #       #	 qr.Q(qr.lm(object))[, p1, drop = FALSE] / sqrt(w)
    #       #	looks faster than the above, but it's slower, and doesn't handle zero
    #       #	weights properly
    #       #
    #       ip <- drop(XRinv^2 %*% rep(res.var, p))
    #     } else ip <- rep(0, n)
    #   }
    # }

    # if (type == "terms") { ## type == "terms" ------------
    #   if(!mmDone) {
    #     mm <- model.matrix(object)
    #     mmDone <- TRUE
    #   }
    #   aa <- attr(mm, "assign")
    #   ll <- attr(tt, "term.labels")
    #   hasintercept <- attr(tt, "intercept") > 0L
    #   if (hasintercept) ll <- c("(Intercept)", ll)
    #   aaa <- factor(aa, labels = ll)
    #   asgn <- split(order(aa), aaa)
    #   if (hasintercept) {
    #     asgn$"(Intercept)" <- NULL
    #     avx <- colMeans(mm)
    #     termsconst <- sum(avx[piv] * beta[piv])
    #   }
    #   nterms <- length(asgn)
    #   if(nterms > 0) {
    #     predictor <- matrix(ncol = nterms, nrow = NROW(X))
    #     dimnames(predictor) <- list(rownames(X), names(asgn))
    #
    #     if (se.fit || interval != "none") {
    #       ip <- matrix(ncol = nterms, nrow = NROW(X))
    #       dimnames(ip) <- list(rownames(X), names(asgn))
    #       Rinv <- qr.solve(qr.R(qr.lm(object))[p1, p1])
    #     }
    #     if(hasintercept)
    #       X <- sweep(X, 2L, avx, check.margin=FALSE)
    #     unpiv <- rep.int(0L, NCOL(X))
    #     unpiv[piv] <- p1
    #     ## Predicted values will be set to 0 for any term that
    #     ## corresponds to columns of the X-matrix that are
    #     ## completely aliased with earlier columns.
    #     for (i in seq.int(1L, nterms, length.out = nterms)) {
    #       iipiv <- asgn[[i]]      # Columns of X, ith term
    #       ii <- unpiv[iipiv]      # Corresponding rows of Rinv
    #       iipiv[ii == 0L] <- 0L
    #       predictor[, i] <-
    #         if(any(iipiv > 0L)) X[, iipiv, drop = FALSE] %*% beta[iipiv]
    #       else 0
    #       if (se.fit || interval != "none")
    #         ip[, i] <-
    #         if(any(iipiv > 0L))
    #           as.matrix(X[, iipiv, drop = FALSE] %*%
    #                       Rinv[ii, , drop = FALSE])^2 %*% rep.int(res.var, p)
    #       else 0
    #     }
    #     if (!is.null(terms)) {
    #       predictor <- predictor[, terms, drop = FALSE]
    #       if (se.fit)
    #         ip <- ip[, terms, drop = FALSE]
    #     }
    #   } else {                        # no terms
    #     predictor <- ip <- matrix(0, n, 0L)
    #   }
    #   attr(predictor, 'constant') <- if (hasintercept) termsconst else 0
    # }

    ### Now construct elements of the list that will be returned

    # if(interval != "none") {
    #   tfrac <- qt((1 - level)/2, df)
    #   hwid <- tfrac * switch(interval,
    #                          confidence = sqrt(ip),
    #                          prediction = sqrt(ip+pred.var)
    #   )
    #   if(type != "terms") {
    #     predictor <- cbind(predictor, predictor + hwid %o% c(1, -1))
    #     colnames(predictor) <- c("fit", "lwr", "upr")
    #   } else {
    #     if (!is.null(terms)) hwid <- hwid[, terms, drop = FALSE]
    #     lwr <- predictor + hwid
    #     upr <- predictor - hwid
    #   }
    # }
    # if(se.fit || interval != "none") {
    #   se <- sqrt(ip)
    #   if(type == "terms" && !is.null(terms) && !se.fit)
    #     se <- se[, terms, drop = FALSE]
    # }
    # if(missing(newdata) && !is.null(na.act <- object$na.action)) {
    #   predictor <- napredict(na.act, predictor)
    #   if(se.fit) se <- napredict(na.act, se)
    # }
    # if(type == "terms" && interval != "none") {
    #   if(missing(newdata) && !is.null(na.act)) {
    #     lwr <- napredict(na.act, lwr)
    #     upr <- napredict(na.act, upr)
    #   }
    #   list(fit = predictor, se.fit = se, lwr = lwr, upr = upr,
    #        df = df, residual.scale = sqrt(res.var))
    # } else if (se.fit)
    #   list(fit = predictor, se.fit = se,
    #        df = df, residual.scale = sqrt(res.var))
    # else
    predictor
  }
