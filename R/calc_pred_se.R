#' Calculate standard errors for predictive margins
#'
#' @param vcov_model variance-covariance matrix of the model
#' @param jacobian jacobian matrix of the predictions
#'
#' @return vector of standard errors
#' @export
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')
#'
#' binom_family <- make.link('logit')
#' ld_fun <- binom_family$mu.eta
#'
#' df3 <- transform(mm$data, gear = factor(3, levels = levels(mtcars$gear)))
#' covar3 <- model.matrix(mm$formula, df3)
#' p3 <- predict(mm, newdata = df3)
#'
#' z <-jacob_level(p3, as.matrix(covar3), ld_fun)
#'
#' calc_pred_se(vcov(mm), z)
#' calc_pred_se(vcov(mm), rbind(z, z))
calc_pred_se <- function(vcov_model, jac){

  stopifnot( (is.matrix(jac) | is.numeric(jac)),
             is.matrix(vcov_model), is.numeric(vcov_model))

  if(!is.matrix(jac)) jac <- matrix(jac, ncol = length(jac))

  sqrt(diag(jac %*% vcov_model %*% t(jac)))

}
