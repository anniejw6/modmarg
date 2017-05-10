# Calculate the jacobian for discrete effects
discrete_effect_jacob <- function(jacobian, base_rn){

  # jacobian: numeric matrix of jacobian for all levels
  # base_rn: row number of the base level
  #
  # returns numeric matrix, jacobian of difference

  stopifnot(is.numeric(jacobian),
            is.matrix(jacobian),
            base_rn <= nrow(jacobian))

  t(apply(jacobian, 1, function(x) x - jacobian[base_rn, ]))

}

# Calculate predictive effects for discrete variable
discrete_effect_pred <- function(pred, base_rn = 1){

  # pred: numeric vector of predictions for all levels
  # base_rn: row number of the base level, defaults to 1

  # returns: matrix of predictive effects

  stopifnot(is.numeric(pred), is.vector(pred), base_rn <= length(pred))

  pred - pred[base_rn]

}
