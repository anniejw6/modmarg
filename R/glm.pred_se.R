get_covar.glm <- function(model, data){

  mm <- model.matrix(
    object = model$formula, data = data,
    contrasts.arg = model$contrasts,
    xlev = model$xlevels)

  mm[, !is.na(model$coefficients)]
}

