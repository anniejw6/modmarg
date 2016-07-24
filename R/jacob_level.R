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
#' df3 <- transform(mm$data, gear = factor(3, levels = levels(mtcars$gear)))
#' df4 <- transform(mm$data, gear = factor(4, levels = levels(mtcars$gear)))
#' df5 <- transform(mm$data, gear = factor(5, levels = levels(mtcars$gear)))
#'
#' covar3 <- model.matrix(mm$formula, df3)
#' covar4 <- model.matrix(mm$formula, df4)
#' covar5 <- model.matrix(mm$formula, df5)
#'
#' # Note type != response
#' p3 <- predict(mm, newdata = df3)
#' p4 <- predict(mm, newdata = df4)
#' p5 <- predict(mm, newdata = df5)
#'
#' jacob_level(p3, as.matrix(covar3), ld_fun)
#' jacob_level(p4, as.matrix(covar4), ld_fun)
#' jacob_level(p5, as.matrix(covar5), ld_fun)
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
