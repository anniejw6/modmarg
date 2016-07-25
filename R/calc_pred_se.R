#' Calculate margins and se
#'
#' For one set of transformed covariates (not including the variable of interest),
#' calculate the predicted level and se for the variable of interest.
#'
#' @param df_trans data.frame, should already be transformed for variables not related to
#' the variable of interest
#' @param var_interest the variable of interest
#' @param model model
#' @param type either effects or levels, defaults to levels
#' @param base_rn row number of the base level, defaults to 1
#' @return dataframe of formatted output
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')
#' # apply at transformations
#' df <- at_transforms(mm$model, list("mpg" = c(15, 21)))
#' df <- df[[1]]
#' calc_jac_se(df, var_interest = 'gear', model = mm)
calc_pred_se <- function(df_trans, var_interest, model,
                         type = 'levels', base_rn = 1,
                         at_var_interest = NULL){

  stopifnot(is.data.frame(df_trans),
            is.character(var_interest),
            var_interest %in% names(df_trans),
            type %in% c('effects', 'levels'))

  df_levels <- at_transforms(df_trans, gen_at_list(df_trans, var_interest, at_var_interest))

  # Get predicted values and covariates
  cov_preds <- lapply(df_levels, function(x)
    predict_modelmat(model = model, transformed_df = x))

  # calculate predictions
  preds <- sapply(cov_preds, function(x){ mean(x$pred_resp) })

  # calculate jacobian
  jacobs <- do.call(rbind, lapply(cov_preds, function(x){
    jacob_level(
      pred_values = x$pred_link, covar_matrix = x$covar,
      link_deriv = model$family$mu.eta)
  }))

  if(type == 'effects'){
    jacobs <- jacob_effect(jacobs, base_rn)
    preds <- pred_effect(preds, base_rn)
  }
  # TODO: figure out whether you want levels or effects

  format_output( # maybe reformat this to var, value, at?
    margin_labels = names(cov_preds),
    pred_margins = preds,
    se = pred_se(vcov(model), jacobs)
  )

}


