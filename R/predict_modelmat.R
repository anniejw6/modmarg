#' Generate predictions and model matrix
#'
#' @param model model object
#' @param transformed_df df after applying whatever transform you want
#' @param formula model formula, defaults to model$formula
#'
#' @return list of covariates (covar), predictions on the link function scale (pred_link),
#' and predictions on the response variable scale (pred_response), e.g., probabilities
#' @importFrom stats model.frame model.matrix predict
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')
#'
#' df3 <- at_transform(df = mtcars, var_name = 'gear', value = 3)
#' covar3 <- model.matrix(mm$formula, df3)
#'
#' z <- predict_modelmat(model = mm, transformed_df = df3)
#' all(z$covar == covar3)
#' all(z$pred_link == predict(mm, newdata = df3))
#' all(z$pred_resp == predict(mm, newdata = df3, type = 'response'))
predict_modelmat <- function(model, transformed_df,
                             formula = model$formula){

  list(
    covar =  model.matrix(formula, transformed_df,
                          contrasts.arg = model$contrasts,
                          xlev = model$xlevels),
    pred_link = predict(model, newdata = transformed_df),
    pred_resp = predict(model, newdata = transformed_df, type = 'response')
  )

}
