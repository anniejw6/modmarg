library(modmarg)
context("Data Munging")

test_that("applying a single transformation", {

  data(mtcars)
  mtcars$gear <- factor(mtcars$gear)

  expect_identical(at_transform(df = mtcars, var_name = 'gear', value = 3),
                   transform(mtcars, gear = factor(3, levels = levels(mtcars$gear))))

  expect_identical(at_transform(df = mtcars, var_name = 'mpg', value = 20),
                   transform(mtcars, mpg = 20))

})


test_that("applying multiple transformations", {

  data(mtcars)
  mtcars$gear <- factor(mtcars$gear)

  expect_identical(at_transforms(df = mtcars, var_name = 'gear', values = c(3,5)),
                   list(transform(mtcars, gear = factor(3, levels = levels(mtcars$gear))),
                        transform(mtcars, gear = factor(5, levels = levels(mtcars$gear))))
  )


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

test_that("make sure we format output correctly", {

  z <- format_output(margin_labels = c('hello', 'goodbye'),
                pred_margins = c(.0791146, .2600204),
                se = c(.0069456, .0111772))

  expect_equal(z$`Lower CI (95%)`, c(.0655016, .2381135), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.0927277, .2819272), tolerance = 0.0001)
  expect_equal(z$P.Value, c(0, 0))
  expect_equal(z$Z.Value, c(11.39, 23.26), tolerance = 0.01)

})

test_that("get means of a list correctly",{

  expect_equal(get_margins(list(1:3, 8:10)),
               c(2, 9))

})
