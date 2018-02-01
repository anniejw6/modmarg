#' Predicted Margins for `glm` objects
#'
#' Obtains predicted margins and standard errors
#' of those predictions from a fitted generalized linear model object.
#'
#' @inheritParams marg
#' @param ... additional parameters passed to \code{?marg}.
#'
#' @export
#' @examples
#' data(mtcars)
#' mod <- glm(vs ~ as.factor(gear) + mpg, data = mtcars, family = 'binomial')
#'
#' # Get the level of the outcome variable at different values of `gear`
#' marg(mod, var_interest = 'gear', type = 'levels')
#' # Get the effect of `gear` on the outcome value, holding values of `mpg`
#' # constant
#' marg(mod, var_interest = 'gear', type = 'effects',
#'      at = list(mpg = c(15, 21)))
#'
#' data(margex)
#' mod <- glm(outcome ~ as.factor(treatment) + distance,
#'        data = margex, family = 'binomial')
#' # Get the level of the outcome variable at different values of `treatment`
#' marg(mod, var_interest = 'treatment', type = 'levels', at = NULL)
#' # Get the effect of `treatment` on the outcome variable
#' marg(mod, var_interest = 'treatment', type = 'effects', at = NULL)
#' # Get the level of the outcome variable at different values of `distance`
#' marg(mod, var_interest = 'distance', type = 'levels',
#'           at = NULL, at_var_interest = c(10, 20, 30))
#'
#' # Using a custom variance-covariance matrix for clustered standard errors
#' # (also requires custom degrees of freedom for T statistic with OLS model),
#' # clustering on the "arm" variable
#'
#' data(margex)
#' data(cvcov)
#' # ?cvcov
#' v <- cvcov$ols$clust
#' d <- cvcov$ols$stata_dof
#' mod <- glm(outcome ~ treatment + distance,
#'            data = margex, family = 'binomial')
#' marg(mod, var_interest = 'treatment', type = 'levels',
#'           vcov_mat = v, dof = d)
#'
#' # Using weights
#' data(margex)
#' mm <- glm(y ~ as.factor(treatment) + age, data = margex, family = 'gaussian',
#'           weights = distance)
#' z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels')[[1]]
#' z2 <- marg(mod = mm, var_interest = 'treatment', type = 'effects')[[1]]
#'
marg.glm <- function(mod, var_interest,
                     data = mod$data[names(mod$prior.weights), ],
                     weights = mod$prior.weights,
                     ...){

  if(all(weights == 1)) weights <- NULL

  .marg(mod = mod, var_interest = var_interest,
        data = data,
        weights = weights,
        ...)
}

pred_se.glm <- function(model,
                        deriv_func = model$family$mu.eta,
                        link_func = model$family$linkinv, ...){

  .pred_se(model = model,
           deriv_func = model$family$mu.eta,
           link_func = model$family$linkinv, ...)

}

#' @importFrom stats get_all_vars
get_data.glm <- function(model, data, weights){

  # Store original number of rows
  nrow_orig <- nrow(data)

  # Grab only necessary variables
  data <- get_all_vars(model, data)

  # Drop to correct rows
  handle_missing(model, data, weights, nrow_orig)
}


get_vcov.glm <- function(model){
  vcov(model, complete = FALSE)
}

get_dof.glm <- function(model, ...){
  model$df.residual
}


get_covar.glm <- function(model, data){

  mm <- model.matrix(
    object = model$formula, data = data,
    contrasts.arg = model$contrasts,
    xlev = model$xlevels)

  mm[, !is.na(model$coefficients)]
}

get_family.glm <- function(model){
  model$family$family
}

has_weights.glm <- function(model){
  !all(model$prior.weights == 1)
}
