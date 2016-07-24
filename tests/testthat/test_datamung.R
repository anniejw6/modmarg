library(modmarg)
context("Data Munging")

test_that("applying a single transformation", {

  data(mtcars)
  mtcars$gear <- factor(mtcars$gear)

  expect_identical(apply_transform(df = mtcars, var_name = 'gear', value = 3),
                   transform(mtcars, gear = factor(3, levels = levels(mtcars$gear))))

  expect_identical(apply_transform(df = mtcars, var_name = 'mpg', value = 20),
                   transform(mtcars, mpg = 20))

})

test_that("Making sure we generate model matrix and predictions correctly", {

  data(mtcars)
  mtcars$gear <- factor(mtcars$gear)
  mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')

  df3 <- apply_transform(df = mtcars, var_name = 'gear', value = 3)
  covar3 <- model.matrix(mm$formula, df3)
  p3 <- predict(mm, newdata = df3)

  z <- predict_modelmat(model = mm, transformed_df = df3)
  expect_identical(z$covar, covar3)
  expect_identical(z$pred, p3)

})
