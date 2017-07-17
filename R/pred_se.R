#' @importFrom stats coef predict model.matrix
# Main wrapper function to calculate margins and se
pred_se <- function(df_trans, var_interest, at_var_interest,
                    model, type, base_rn, vcov_mat, weights){

  stopifnot(is.data.frame(df_trans),
            is.character(var_interest),
            var_interest %in% names(df_trans),
            type %in% c('effects', 'levels'))

  # For one set of transformed covariates (not including the variable of
  # interest), calculate the predicted level and se for the
  # variable of interest.
  #
  # @param df_trans: data.frame, already transformed for variables not related
  #                  to the variable of interest
  # @param var_interest: character, the variable of interest
  # @param model: glm object
  # @param type: either effects or levels
  # @param base_rn: numeric, row number of the base level
  # @param vcov_mat: matrix, variance-covariance matrix
  # @param at_var_interest: vector, if type == 'levels', the values for the
  #                         variable of interest at which levels should be
  #                         calculated. ignored otherwise
  # @param weights: vector of weights, or NULL
  #
  # @return list of labels, predicted margins, and SE

  df_levels <- at_transforms(
    df_trans, gen_at_list(df_trans, var_interest, at_var_interest))

  res <- lapply(df_levels, function(x){
    # Predict function is expensive so just calling it once
    p <- predict(model, newdata = x)

    # Calculate mean values
    if(is.null(weights)){
      preds <- mean(model$family$linkinv(p))
    } else {
      preds <- sum(model$family$linkinv(p) * weights)/sum(weights)
    }

    list(
      # Calculate Jacobian
      jacobs = calc_jacob(
        pred_values = p,
        # create a model matrix only using coefficients in the model
        covar_matrix = model.matrix(
          object = model$formula, data = x,
          contrasts.arg = model$contrasts,
          xlev = model$xlevels)[, !is.na(coef(model))],
        deriv_func = model$family$mu.eta,
        weights = weights),
      # Calculate predicted values
      preds = preds
    )
  })

  jacobs <- do.call(rbind, lapply(res, function(x){x[['jacobs']]}))
  preds <- vapply(res, function(x){ x[['preds']]}, numeric(1))

  if(type == 'effects') {
    if(is.numeric(df_trans[[var_interest]]) &
       ! all(unique(df_trans[[var_interest]]) %in% c(0, 1)) &
       ! sprintf("as.character(%s)", var_interest) %in% names(model$model) &
       ! sprintf("as.factor(%s)", var_interest) %in% names(model$model)){
      stop('We do not support effects for continuous variables at this time.')
    } else {
      jacobs <- discrete_effect_jacob(jacobs, base_rn)
      preds <- discrete_effect_pred(preds, base_rn)
    }
  }

  list(
    labels = names(df_levels),
    pred_margins = preds,
    se = calc_pred_se(vcov_mat, jacobs)
  )

}


