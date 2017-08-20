# Main wrapper function to calculate margins and se
pred_se.ivreg <- function(df_levels, model, type, base_rn, vcov_mat, weights){


  res <- lapply(df_levels, function(x){
    # Predict function is expensive so just calling it once
    p <- predict(model, newdata = x)

    # Calculate mean values
    if(is.null(weights)){
      preds <- mean(p)
    } else {
      preds <- sum(p * weights)/sum(weights)
    }

    # Get covariate matrices
    mf <- model.frame(
      delete.response(model$terms$full), x,
      na.action = na.pass, xlev = model$levels)
    covar_matrix <- model.matrix(
      delete.response(model$terms$regressors), mf,
      contrasts = model$contrasts$regressors)[, !is.na(model$coefficients), drop = FALSE]

    list(
      # Calculate Jacobian
      jacobs = calc_jacob(
        pred_values = p,
        covar_matrix = covar_matrix,
        deriv_func = function(x) rep.int(1, length(x)),
        weights = weights),
      # Calculate predicted values
      preds = preds
    )
  })

  jacobs <- do.call(rbind, lapply(res, function(x){x[['jacobs']]}))
  preds <- vapply(res, function(x){ x[['preds']]}, numeric(1))

  if(type == 'effects') {
      jacobs <- discrete_effect_jacob(jacobs, base_rn)
      preds <- discrete_effect_pred(preds, base_rn)
  }

  list(
    labels = names(df_levels),
    pred_margins = preds,
    se = calc_pred_se(vcov_mat, jacobs)
  )

}


