#' Calculate the Jacobian matrix for predictive levels or effects
#'
#' @param covar_matrix numeric matrix of covariates
#' @param pred_values numeric vector of predicted values
#' @param deriv_func if levels, this should be the function for the derivative
#' of the link function with respect to eta. if effects, this should be
#' the second derivative of the above.
#'
#' @return numeric vector row of jacobian
#' @export
#'
#' @examples
#'
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')
#'
#' new_df <- at_transform(df = mtcars, var_name = 'gear', value = 3)
#'
#' calc_jacob(
#'   pred_values = predict(
#'     mm, new_df),
#'   covar_matrix = model.matrix(
#'     object = mm$formula, data = new_df, contrasts.arg = mm$contrasts,
#'     xlev = mm$xlevels)[, !is.na(coef(mm))],
#'   deriv_func = mm$family$mu.eta)
#'
calc_jacob <- function(pred_values, covar_matrix, deriv_func){

  stopifnot(is.numeric(pred_values), is.matrix(covar_matrix),
            is.numeric(covar_matrix), is.function(deriv_func))

  # caluclate derivative of the predicted values
  x1 <- do.call(deriv_func, list(pred_values))

  as.numeric(
    crossprod(x1, covar_matrix)/nrow(covar_matrix)
  )

}
