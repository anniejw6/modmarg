#' Predicted Margins for `estimatr::lm_robust` objects
#'
#' Obtains predicted margins and standard errors
#' of those predictions from a fitted generalized linear model object.
#'
#' @inheritParams marg
#' @param ... additional parameters passed to \code{?marg}.
#'
#' @export
#' @examples
#'
marg.lm_robust <- function(mod, var_interest,
                     data,
                     weights = mod$weights,
                     ...){

  stopifnot(length(data) > 0)

  .marg(mod = mod, var_interest = var_interest,
        data = data,
        weights = weights,
        ...)
}

pred_se.lm_robust <- function(model,
                        deriv_func = model$family$mu.eta,
                        link_func = model$family$linkinv, ...){

  family <- stats::gaussian()

  .pred_se(model = model,
           deriv_func = family$mu.eta,
           link_func = family$linkinv, ...)

}

#' @importFrom stats get_all_vars
get_data.lm_robust <- function(model, data, weights){

  # Store original number of rows
  nrow_orig <- nrow(data)

  # Grab only necessary variables
  data <- get_all_vars(model, data)

  # Drop to correct rows
  handle_missing(model, data, weights, nrow_orig)
}


get_vcov.lm_robust <- function(model){
  model$vcov
}

get_dof.lm_robust <- function(model, ...){
  model$df.residual
}


get_covar.lm_robust <- function(model, data){

  mm <- model.matrix(model, data = data)

  mm[, !is.na(model$term)]
}

get_family.lm_robust <- function(model){
  "gaussian"
}

has_weights.lm_robust <- function(model){
  model$weighted
}
