library(modmarg)
context("Data Munging")

test_that("applying a single transformation", {

  data(mtcars)
  df <- mtcars
  df$gear <- factor(df$gear)

  d0 <- at_transform(var = df[['gear']], value = 3)
  d1 <- transform(df, gear = factor(3, levels = levels(df$gear)))

  expect_identical(d0, d1[['gear']])

  d0 <- at_transform(var = df[['mpg']], value = 20)
  d1 <- transform(df, mpg = 20)

  expect_identical(d0, d1$mpg)

})


test_that("applying multiple transformations", {

  data(mtcars)
  mtcars$gear <- factor(mtcars$gear)

  expect_equivalent(
    at_transforms(mtcars, at_list = list("gear" =  c(3,5))),
    list(transform(mtcars, gear = factor(3, levels = levels(mtcars$gear))),
         transform(mtcars, gear = factor(5, levels = levels(mtcars$gear))))
  )

  expect_equivalent(
    at_transforms(mtcars, list("mpg" = c(15, 21), "disp" = c(140, 180))),
    list(transform(mtcars, mpg = 15, disp = 140),
         transform(mtcars, mpg = 21, disp = 140),
         transform(mtcars, mpg = 15, disp = 180),
         transform(mtcars, mpg = 21, disp = 180))
  )

  var_interest <- 'gear'
  df_sm <- mtcars
  values_var_interest <- unique(df_sm[[var_interest]])
  values_var_interest <- list(values_var_interest[order(values_var_interest)])
  names(values_var_interest) <- var_interest

  expect_equivalent(
    at_transforms(df_sm, values_var_interest),
    list(transform(mtcars, gear = factor(3, levels = levels(mtcars$gear))),
         transform(mtcars, gear = factor(4, levels = levels(mtcars$gear))),
         transform(mtcars, gear = factor(5, levels = levels(mtcars$gear))))
  )

})


test_that("make sure we format output correctly", {

  z <- format_output(margin_labels = c('hello', 'goodbye'),
                     pred_margins = c(.0791146, .2600204),
                     se = c(.0069456, .0111772),
                     family = "gaussian",
                     dof = 20)

  expect_equal(z$`Lower CI (95%)`, c(0.06462633, 0.23670517),
               tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(0.09360287, 0.28333563),
               tolerance = 0.0001)
  expect_equal(z$P.Value, c(0, 0))
  expect_equal(z$Test.Stat, c(11.39, 23.26), tolerance = 0.01)

  z <- format_output(margin_labels = c('hello', 'goodbye'),
                     pred_margins = c(.0791146, .2600204),
                     se = c(.0069456, .0111772),
                     family = "gaussian",
                     dof = 20,
                     cofint = c(0.05, 0.95))

  expect_equal(z$`Lower CI (90%)`, c(0.0671354, 0.2407429),
               tolerance = 0.0001)
  expect_equal(z$`Upper CI (90%)`, c(0.0910938, 0.2792979),
               tolerance = 0.0001)
  expect_equal(z$P.Value, c(0, 0), tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(11.39061, 23.26346), tolerance = 0.0001)

})
