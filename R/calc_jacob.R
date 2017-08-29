# Calculate the Jacobian matrix for predictive levels or effects
calc_jacob <- function(pred_values, covar_matrix, deriv_func,
                       weights = rep(1, nrow(covar_matrix)),
                       ...){

  # covar_matrix: numeric matrix of covariates
  # pred_values: numeric vector of predicted values
  # deriv_func: if levels, this should be the function for the derivative
  #             of the link function with respect to eta. if effects, this
  #             should be the second derivative of the above.

  stopifnot(is.numeric(pred_values), is.matrix(covar_matrix),
            is.numeric(covar_matrix), is.function(deriv_func))

  # Calculate derivative of the predicted values
  x1 <- do.call(deriv_func, list(pred_values))

  as.numeric(crossprod(x1 * weights, covar_matrix) / sum(weights))

}

# Calculate the standard error of predictive levels
calc_pred_se <- function(vcov_model, jac){

  # vcov_model: variance-covariance matrix of the model
  # jac: jacobian matrix of the predictions

  # returns vector of standard errors

  stopifnot( (is.matrix(jac) | is.numeric(jac)),
             is.matrix(vcov_model), is.numeric(vcov_model))

  if(!is.matrix(jac)) jac <- matrix(jac, ncol = length(jac))

  sqrt(diag(jac %*% vcov_model %*% t(jac)))

}

