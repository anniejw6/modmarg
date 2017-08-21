# # Main wrapper function to calculate margins and se
# pred_se.glm <- function(df_levels, model, type, base_rn, vcov_mat, weights){
#
#
#   res <- lapply(df_levels, function(x){
#     # Predict function is expensive so just calling it once
#     p <- predict(model, newdata = x)
#
#     # Calculate mean values
#     preds <- model$family$linkinv(p)
#     if(is.null(weights)){
#       preds <- mean(preds)
#     } else {
#       preds <- sum(preds * weights)/sum(weights)
#     }
#
#     # Get covariate matrix
#     covar_matrix <- model.matrix(
#       object = model$formula, data = x,
#       contrasts.arg = model$contrasts,
#       xlev = model$xlevels)[, !is.na(coef(model))]
#
#     list(
#       # Calculate Jacobian
#       jacobs = calc_jacob(
#         pred_values = p,
#         covar_matrix = covar_matrix,
#         deriv_func = model$family$mu.eta,
#         weights = weights),
#       # Calculate predicted values
#       preds = preds
#     )
#   })
#
#   jacobs <- do.call(rbind, lapply(res, function(x){x[['jacobs']]}))
#   preds <- vapply(res, function(x){ x[['preds']]}, numeric(1))
#
#   if(type == 'effects') {
#       jacobs <- discrete_effect_jacob(jacobs, base_rn)
#       preds <- discrete_effect_pred(preds, base_rn)
#   }
#
#   list(
#     labels = names(df_levels),
#     pred_margins = preds,
#     se = calc_pred_se(vcov_mat, jacobs)
#   )
#
# }


get_covar.glm <- function(model, data){

  mm <- model.matrix(
    object = model$formula, data = data,
    contrasts.arg = model$contrasts,
    xlev = model$xlevels)

  mm[, !is.na(model$coefficients)]
}

