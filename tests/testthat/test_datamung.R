library(modmarg)
context("Data Munging")

test_that("applying a single transformation", {

  data(mtcars)
  mtcars$gear <- as.factor(mtcars$gear)
  mod <- glm(cyl ~ gear + mpg + disp, data = mtcars)
  df_sm <- mtcars[, c('cyl', 'gear', 'mpg', 'disp')]

  expect_equivalent(
    at_transform(df = mtcars, mod = mod, var_name = 'gear', value = 3),
    transform(df_sm, gear = factor(3, levels = levels(mtcars$gear))))

  expect_equivalent(
    at_transform(df = mtcars, mod = mod,
                 var_name = 'mpg', value = 20),
    transform(df_sm, mpg = 20))

})


test_that("applying multiple transformations", {

  expect_equivalent(
    at_transforms(mtcars, at_list = list("gear" =  c(3,5)), mod = mod),
    list(transform(df_sm, gear = factor(3, levels = levels(mtcars$gear))),
         transform(df_sm, gear = factor(5, levels = levels(mtcars$gear))))
  )

  expect_equivalent(
    at_transforms(mtcars, list("mpg" = c(15, 21), "disp" = c(140, 180)),
                  mod = mod),
    list(transform(df_sm, mpg = 15, disp = 140),
         transform(df_sm, mpg = 21, disp = 140),
         transform(df_sm, mpg = 15, disp = 180),
         transform(df_sm, mpg = 21, disp = 180))
  )

  var_interest <- 'gear'
  values_var_interest <- unique(mtcars[[var_interest]])
  values_var_interest <- list(values_var_interest[order(values_var_interest)])
  names(values_var_interest) <- var_interest

  expect_equivalent(
    at_transforms(mtcars, values_var_interest, mod = mod),
    list(transform(df_sm, gear = factor(3, levels = levels(mtcars$gear))),
         transform(df_sm, gear = factor(4, levels = levels(mtcars$gear))),
         transform(df_sm, gear = factor(5, levels = levels(mtcars$gear))))
  )

})

test_that("Making sure we generate model matrix and predictions correctly", {

  data(mtcars)
  mtcars$gear <- factor(mtcars$gear)
  mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')

  df3 <- at_transform(df = mtcars, var_name = 'gear', value = 3)
  covar3 <- model.matrix(mm$formula, df3)

  z <- predict_modelmat(model = mm, transformed_df = df3)
  expect_identical(z$covar, covar3)
  expect_identical(z$pred_link, predict(mm, newdata = df3))
  expect_identical(z$pred_resp, predict(mm, newdata = df3, type = 'response'))

})

test_that("make sure we format output correctly", {

  z <- format_output(margin_labels = c('hello', 'goodbye'),
                     pred_margins = c(.0791146, .2600204),
                     se = c(.0069456, .0111772))

  expect_equal(z$`Lower CI (95%)`, c(.0655016, .2381135), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.0927277, .2819272), tolerance = 0.0001)
  expect_equal(z$P.Value, c(0, 0))
  expect_equal(z$Z.Value, c(11.39, 23.26), tolerance = 0.01)

  # TODO: add check in here for negative values!
})
