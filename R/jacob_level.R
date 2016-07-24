#' Calculate the Jacobian matrix for predictive levels
#'
#' @param covar_matrix numeric matrix of covariates
#' @param pred_values numeric vector of predicted values
#' @param inv_link_deriv function for the derivative of the inverse link function with respect to eta
#'
#' @return numeric vector row of jacobian
#' @export
#'
#' @examples
#'
#' # Run model
#' data(mtcars)
#' mtcars$carb <- factor(mtcars$carb)
#' mm <- glm(vs ~ carb + mpg * disp, mtcars, family = 'binomial')
#'
#' # Create transformed covariates
#' covar <- transform(mm$model, carb = 1)
#' covar$`mpg:disp` <- covar$mpg * covar$disp
#'
#' # Generate predictions
#' z <- predict(mm, data = covar)
#'
#' # Specify derivative of link function
#' binom_family <- make.link('logit')
#' ld_fun <- binom_family$mu.eta
#'
#' jacob_level(pred_values = z, covar_matrix = covar, inv_link_deriv = ld_fun)
jacob_level <- function(pred_values, covar_matrix, inv_link_deriv){

  x1 <- do.call(inv_link_deriv, list(pred_values)) # derivative of the predicted valeus

  as.numeric(
    crossprod(x1, as.matrix(covar_matrix))/nrow(covar_matrix)
  )

}
