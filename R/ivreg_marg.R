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
#' # TODO: weights, clustering
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

  # Add weights
  if('_weights' %in% all.vars(model$formula))
    stop("You cannot use the name '_weights' in the model formula. ",
         "Please rename to another variable.")
  if(is.null(weights)) weights <- rep(1, nrow_orig)

  # Keep completes only
  miss <- rowSums(is.na(data)) > 0 | is.na(weights)
  weights <- weights[! miss]
  data <- data[! miss, , drop = FALSE]

  # Remove any booleans
  if(all(data$`T` == TRUE))
    data$`T` <- NULL
  if(all(data$`F` == FALSE))
    data$`F` <- NULL

  # Throw warning if rows were dropped
  if(nrow(data) != nrow_orig)
    warning(sprintf('Dropping %s rows due to missing data',
                    nrow_orig - nrow(data)))

  list(data = data,
       weights = weights)

}

# Method taken from unexported AER::vcov.ivreg method
# (https://github.com/cran/AER/blob/master/R/ivreg.R)
# and modified to match Stata dof correction
get_vcov.ivreg <- function(model){
  model$sigma^2 * model$cov.unscaled * model$df.residual / model$nobs
}

get_dof.ivreg <- function(model, ...){
  Inf
}

#' @importFrom stats delete.response model.frame na.pass
get_covar.ivreg <- function(model, data){

  mf <- model.frame(
    delete.response(model$terms$full), data,
    na.action = na.pass, xlev = model$levels)

  covar_matrix <- model.matrix(
    delete.response(model$terms$regressors), mf,
    contrasts = model$contrasts$regressors)

  covar_matrix[, !is.na(model$coefficients), drop = FALSE]
}

get_family.ivreg <- function(model){
  'gaussian'
}
