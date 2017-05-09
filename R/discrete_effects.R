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
#' new_dfs <- lapply(3:5, function(x){
#'   at_transform(df = mtcars, var_name = 'gear', value = x)})
#' z <- do.call(
#'   rbind, lapply(new_dfs, function(x){
#'     calc_jacob(
#'       pred_values = predict(mm, x),
#'       covar_matrix = model.matrix(
#'         object = mm$formula, data = x, contrasts.arg = mm$contrasts,
#'         xlev = mm$xlevels)[, !is.na(coef(mm))],
#'       deriv_func = mm$family$mu.eta)
#'   }))
#'
#' discrete_effect_jacob(z, 1)
#'
discrete_effect_jacob <- function(jacobian, base_rn = 1){

  stopifnot(is.numeric(jacobian),
            is.matrix(jacobian),
            base_rn <= nrow(jacobian))

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
#' discrete_effect_pred(c(1,3),1)
discrete_effect_pred <- function(pred, base_rn = 1){

  stopifnot(is.numeric(pred), is.vector(pred), base_rn <= length(pred))

  pred - pred[base_rn]

}
