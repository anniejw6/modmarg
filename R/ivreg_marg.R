#' Predicted Margins for `ivreg` objects from the \code{AER} package
#'
#' Obtains predicted margins and standard errors
#' of those predictions from a fitted \code{ivreg} model object.
#'
#' @inheritParams marg
#' @param ... additional parameters passed to \code{?marg}.
#'
#' @export
#' @examples
#' # From ?AER::ivreg
#'
#' # data
#' data("CigarettesSW", package = "AER")
#' CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
#' CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
#' CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
#'
#' # model
#' fm <- AER::ivreg(log(packs) ~ log(rprice) + log(rincome) |
#'                    log(rincome) + tdiff + I(tax/cpi),
#'                  data = CigarettesSW, subset = year == "1995")
#'
#' # Get margins for different levels of price/cpi
#' rprice_levs <- round(quantile(CigarettesSW$rprice))
#'
#' marg(fm, data = subset(CigarettesSW, year == "1995"),
#'      var_interest = 'rprice', at_var_interest = rprice_levs)
#'
marg.ivreg <- function(mod, var_interest,
                       data,
                       weights = NULL,
                       ...){

  .marg(mod = mod, var_interest = var_interest,
        data = data,
        weights = weights,
        ...)
}

pred_se.ivreg <- function(model,
                          deriv_func = function(x) rep.int(1, length(x)),
                          link_func = identity,
                          ...){

  .pred_se(model = model,
           deriv_func = function(x) rep.int(1, length(x)),
           link_func = identity,
           ...)

}

#' @importFrom stats get_all_vars
get_data.ivreg <- function(model, data, weights){

  # Store original number of rows
  nrow_orig <- nrow(data)

  # Grab only necessary variables
  data <- data[, all.vars(model$formula), drop = FALSE]

  # Drop to correct rows
  handle_missing(model, data, weights, nrow_orig)

}

# Method taken from unexported AER::vcov.ivreg method
# (https://github.com/cran/AER/blob/1530163a062bcd848cb38f5d0c8511583ed5599b/R/ivreg.R#L139-L140)
# and modified to match Stata dof correction
get_vcov.ivreg <- function(model){
  model$sigma^2 * model$cov.unscaled * model$df.residual / model$nobs
}

get_dof.ivreg <- function(model, ...){
  Inf
}

# Create covariate matrix given arbitrary data. Adapted from
# https://github.com/cran/AER/blob/1530163a062bcd848cb38f5d0c8511583ed5599b/R/ivreg.R#L45-L46
#' @importFrom stats delete.response model.frame na.pass
get_covar.ivreg <- function(model, data){

  # Format data according to formula (applies transformations,
  # adds as.factor() to names, etc)
  mf <- model.frame(
    delete.response(model$terms$full), data,
    na.action = na.pass, xlev = model$levels)

  # Convert data to model matrix (add intercept, break out factors)
  covar_matrix <- model.matrix(
    delete.response(model$terms$regressors), mf,
    contrasts = model$contrasts$regressors)

  # Return, dropping coefficients that weren't estimated
  covar_matrix[, !is.na(model$coefficients), drop = FALSE]
}

get_family.ivreg <- function(model){
  'gaussian'
}

has_weights.ivreg <- function(model){
  !is.null(model$weights)
}
