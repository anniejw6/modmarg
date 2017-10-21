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
#' data(margex)
#' margex$assign <- margex$treatment
#'
#' # Probability of getting treatment increases with age
#' margex$pr_treat <- plogis(
#'  scale(margex$age) + scale(margex$distance))
#'
#' # One-way non-compliance
#' margex$actual <- margex$assign
#' margex$actual[margex$assign == 1] <- rbinom(
#'   n = sum(margex$assign == 1), size = 1,
#'   prob = margex$pr_treat[margex$assign == 1]
#' )
#'
#' table(margex$assign, margex$actual)
#'
#' # Fit the model
#' mod <- AER::ivreg(
#'   y ~ as.factor(actual) + age + distance |
#'     as.factor(assign) + age + distance,
#'   data = margex)
#'
#' # Get margins for different levels of "actual" treatment
#' modmarg::marg(mod, data = margex, var_interest = 'actual')
#'
#'
marg.ivreg <- function(mod, var_interest,
                       data,
                       weights = mod$weights,
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
  data <- data[, all.vars(model$formula)]

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

#' Create covariate matrix given arbitrary data. Adapted from
#' https://github.com/cran/AER/blob/1530163a062bcd848cb38f5d0c8511583ed5599b/R/ivreg.R#L45-L46
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
