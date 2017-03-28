#' Calculate margins and se for continuous variables
#'
#' For one set of transformed covariates (not including the variable of interest),
#' calculate the predicted level and se for the variable of interest.
#'
#' @param df_trans data.frame, should already be transformed for variables not related to
#' the variable of interest
#' @param var_interest the variable of interest
#' @param at_var_interest levels of variable of interest
#' @param model model
#' @param type either effects or levels, defaults to levels
#' @param vcov_mat variance-covariance matrix, defaults to NULL
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
#' continuous_wrap(df, var_interest = 'gear', model = mm)
continuous_wrap <- function(df_trans, var_interest, model,
                            at_var_interest = NULL,
                         type = 'levels',
                         vcov_mat = NULL){

  stopifnot(is.data.frame(df_trans),
            is.character(var_interest),
            var_interest %in% names(df_trans),
            type %in% c('effects', 'levels'))

  df_levels <- at_transforms(
    model_df = df_trans,
    at_list = gen_at_list(df_trans, var_interest, at_var_interest),
    mod = model)

  # Get predicted values and covariates
  cov_preds <- lapply(df_levels, function(x)
    predict_modelmat(model = model, transformed_df = x))

  # calculate predictions
  preds <- sapply(cov_preds, function(x){ mean(x$pred_resp) })
  
  # if covariates are dropped from the model, remove those columns from cov_preds
  for(i in 1:length(cov_preds)){
    cov_preds[[i]]$covar <- cov_preds[[i]]$covar[, !is.na(coef(model))]
  }

  # calculate jacobian
  jacobs <- do.call(rbind, lapply(cov_preds, function(x){
    if(type == 'levels'){
      jacob_level(
        pred_values = x$pred_link, covar_matrix = x$covar,
        link_deriv = model$family$mu.eta)
    } else {
      stop('We do not support effects for continuous variables at this time.')
    }

  }))

  format_output( # maybe reformat this to var, value, at?
    margin_labels = names(cov_preds),
    pred_margins = preds,
    se = calc_pred_se(vcov_mat, jacobs)
  )

}


