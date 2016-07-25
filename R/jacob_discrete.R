
#' Jacobian matrix to discrete
#'
#' @inheritParams jacob_level
#' @param base_rn row number of the base level, defaults to 1
#' @return jacobian matrix
#' @export
#' @examples
#' jacob_discrete(pred_values = z$pred, covar_matrix = z$covar,
#' link_deriv = mm$family$mu.eta, effects = T)
jacob_discrete <- function(pred_values, covar_matrix, link_deriv,
                           base_rn = 1, effects = F){

  z <- jacob_level(pred_values, covar_matrix, link_deriv)

  if(effects){
    z <- jacob_effect(z, base_rn)
  }

  z

}
