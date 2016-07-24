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
