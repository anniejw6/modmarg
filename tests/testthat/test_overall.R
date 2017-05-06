library(modmarg)
context("Calculate everything correctly")

test_that("output is calculated correctly", {

  # http://www.stata.com/support/faqs/statistics/compute-standard-errors-with-margins/
  data(margex)
  margex$treatment <- factor(margex$treatment)
  mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
  z <- mod_marg2(mod, var_interest = 'treatment', type = 'levels', at = NULL)[[1]]

  expect_equal(z$Margin, c(.0791146, .2600204), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.0069456, .0111772), tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(11.39, 23.26), tolerance = 0.001)
  expect_equal(z$P.Value, c(0, 0), tolerance = 0.001)
  expect_equal(z$`Lower CI (95%)`, c(.0655016, .2381135), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.0927277, .2819272), tolerance = 0.0001)

  z <- mod_marg2(mod, var_interest = 'treatment', type = 'effects', at = NULL)[[1]]
  z <- z[2, ]
  expect_equal(z$Margin, c(.1809057), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.0131684), tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(13.74), tolerance = 0.001)
  expect_equal(z$P.Value, c(0), tolerance = 0.001)
  expect_equal(z$`Lower CI (95%)`, c(.1550961), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.2067153), tolerance = 0.0001)
})

test_that("interaction terms", {

  data(mtcars)
  mtcars$am <- factor(mtcars$am)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)

  ols1 <- glm(mpg ~ am * poly(disp, degree = 2, raw = TRUE) +
                cyl + hp + gear, data = mtcars)

  eff1 <- mod_marg2(
    mod = ols1, var_interest = 'am', type = 'effects',
    at = list('disp' = seq(70, 475, 5)))

  ols2 <- glm(mpg ~ am * disp + am * I(disp^2) +
                cyl + hp + gear, data = mtcars)

  eff2 <- mod_marg2(
    mod = ols2, var_interest = 'am', type = 'effects',
    at = list('disp' = seq(70, 475, 5)))

  expect_equal(eff1, eff2)
  expect_equal(eff1$`disp = 90`$Margin, c(0, 6.7650630), tolerance = 0.0001)
  expect_equal(eff1$`disp = 90`$Standard.Error, c(0, 2.509522), tolerance = 0.0001)
  expect_equal(eff1$`disp = 90`$P.Value, c(NaN, 0.0135366), tolerance = 0.0001)
  expect_equal(eff1$`disp = 425`$Margin, c(0, 9.7177810), tolerance = 0.0001)
  expect_equal(eff1$`disp = 425`$Standard.Error, c(0, 5.924503), tolerance = 0.0001)
  expect_equal(eff1$`disp = 425`$P.Value, c(NaN, 0.1158436), tolerance = 0.0001)

  # Missing values in factor for interaction
  mtcars$cyl[c(1,10,20,31)] <- NA
  ols3 <- glm(mpg ~ cyl * poly(disp, degree = 2, raw = TRUE) +
                hp + gear, data = mtcars)
  eff3 <- mod_marg2(
    mod = ols3, var_interest = 'cyl', type = 'effects',
    at = list('disp' = seq(70, 475, 5)))
  
  expect_equal(eff3$`disp = 90`$Margin, c(0, 1.475092, -26.624940), 
               tolerance = 0.0001)
  expect_equal(eff3$`disp = 90`$Standard.Error, c(0, 10.06496, 11.51925), 
               tolerance = 0.0001)
  expect_equal(eff3$`disp = 90`$P.Value, c(NaN, 0.8853123, 0.0344676), 
               tolerance = 0.0001)
  expect_equal(eff3$`disp = 425`$Margin, c(0, -164.3417, -205.5133), 
               tolerance = 0.0001)
  expect_equal(eff3$`disp = 425`$Standard.Error, c(0, 168.0473, 154.9123), 
               tolerance = 0.0001)
  expect_equal(eff3$`disp = 425`$P.Value, c(NaN, 0.3426556, 0.2032512), 
               tolerance = 0.0001)
  
})
