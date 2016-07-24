#' Calculate the Jacobian matrix for predictive levels
#'
#' @param covar_matrix numeric matrix of covariates
#' @param pred_values numeric vector of predicted values
#' @param link_deriv function for the derivative of the link function with respect to eta
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
#' binom_family <- make.link('logit')
#' ld_fun <- binom_family$mu.eta
#'
#' x3 <- predict_modelmat(model = mm,
#' transformed_df = at_transform(df = mtcars, var_name = 'gear', value = 3)
#' )
#'
#' x4 <- predict_modelmat(model = mm,
#' transformed_df = at_transform(df = mtcars, var_name = 'gear', value = 4)
#' )
#'
#' x5 <- predict_modelmat(model = mm,
#' transformed_df = at_transform(df = mtcars, var_name = 'gear', value = 5)
#' )
#'
#' jacob_level(x3$pred, x3$covar, ld_fun)
#' jacob_level(x4$pred, x4$covar, ld_fun)
#' jacob_level(x5$pred, x5$covar, ld_fun)
#'
jacob_level <- function(pred_values, covar_matrix, link_deriv){

  stopifnot(is.numeric(pred_values), is.matrix(covar_matrix),
            is.numeric(covar_matrix), is.function(link_deriv))

  # caluclate derivative of the predicted values
  x1 <- do.call(link_deriv, list(pred_values))

  as.numeric(
    crossprod(x1, covar_matrix)/nrow(covar_matrix)
  )

}
