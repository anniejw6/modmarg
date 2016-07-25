#' Calculate jacobian for effects
#'
#' @param jacobian numeric matrix of jacobian for all levels
#' @param base_rn row number of the base level, defaults to 1
#'
#' @return numeric matrix, jacobian of difference
#' @export
#'
#' @examples
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
#' z <- rbind(
#' jacob_level(x3$pred, x3$covar, ld_fun),
#' jacob_level(x4$pred, x4$covar, ld_fun),
#' jacob_level(x5$pred, x5$covar, ld_fun)
#' )
#' jacob_effect(z, 1)
jacob_effect <- function(jacobian, base_rn = 1){

  stopifnot(is.numeric(jacobian), is.matrix(jacobian), base_rn <= nrow(jacobian))

  t(apply(jacobian, 1, function(x) x - jacobian[base_rn, ]))

}


#' Calculate predictions for effects
#'
#' @param pred numeric vector of predictions for all levels
#' @param base_rn row number of the base level, defaults to 1
#'
#' @return numeric vector, difference of predictions
#' @export
#'
#' @examples
#' pred_effect(c(1,3),1)
pred_effect <- function(pred, base_rn = 1){

  stopifnot(is.numeric(pred), is.vector(pred), base_rn <= length(pred))

  pred - pred[base_rn]

}
