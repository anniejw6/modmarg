#' Generate predictions and model matrix
#'
#' @param model model object
#' @param transformed_df df after applying whatever transform you want
#' @param formula model formula, defaults to model$formula
#'
#' @return list of covariates (covar), predictions on the link function scale (pred_link),
#' and predictions on the response variable scale (pred_response), e.g., probabilities
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
#' expect_identical(z$covar, covar3)
#' expect_identical(z$pred_link, predict(mm, newdata = df3))
#' expect_identical(z$pred_resp, predict(mm, newdata = df3, type = 'response'))
predict_modelmat <- function(model, transformed_df,
                             formula = model$formula){

  m <- model.frame(formula, transformed_df, xlev = model$xlevels)

  list(
    covar =  model.matrix(formula, m, contrasts.arg = model$contrasts),
    pred_link = predict(model, newdata = transformed_df),
    pred_resp = predict(model, newdata = transformed_df, type = 'response')
  )

}
