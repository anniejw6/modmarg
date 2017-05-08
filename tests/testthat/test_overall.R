library(modmarg)
context("Calculate everything correctly")

test_that("levels are calculated correctly despite different input types", {

  # http://www.stata.com/support/faqs/statistics/compute-standard-errors-with-margins/

  # Put as.factor in the equation
  data(margex)
  mod1 <- glm(outcome ~ as.character(treatment) + distance,
              data = margex, family = 'binomial')
  z1 <- mod_marg2(mod1, var_interest = 'treatment',
                 type = 'levels', at = NULL)[[1]]

  # Make character outside
  data(margex)
  margex$treatment <- as.character(margex$treatment)
  mod2 <- glm(outcome ~ treatment + distance,
              data = margex, family = 'binomial')
  z2 <- mod_marg2(mod2, var_interest = 'treatment',
                 type = 'levels', at = NULL)[[1]]

  # Make factor inside equation
  data(margex)
  mod3 <- glm(outcome ~ as.factor(treatment) + distance,
              data = margex, family = 'binomial')
  z3 <- mod_marg2(mod3, var_interest = 'treatment',
                  type = 'levels', at = NULL)[[1]]

  expect_equal(z1$Margin, z2$Margin, z3$Margin, c(.0791146, .2600204),
               tolerance = 0.0001)
  expect_equal(z1$Standard.Error, z2$Standard.Error, z3$Standard.Error,
               c(.0069456, .0111772), tolerance = 0.0001)
  expect_equal(z1$Test.Stat, z2$Test.Stat, z3$Test.Stat,
               c(11.39, 23.26), tolerance = 0.001)
  expect_equal(z1$P.Value, z2$P.Value, z3$P.Value,
               c(0, 0), tolerance = 0.001)
  expect_equal(z1$`Lower CI (95%)`, z2$`Lower CI (95%)`, z3$`Lower CI (95%)`,
               c(.0655016, .2381135), tolerance = 0.0001)
  expect_equal(z1$`Upper CI (95%)`, z2$`Upper CI (95%)`, z3$`Upper CI (95%)`,
               c(.0927277, .2819272), tolerance = 0.0001)

})


test_that("effects are calculated correctly despite different input types", {

  # Put as.factor in the equation
  data(margex)
  mod1 <- glm(outcome ~ as.character(treatment) + distance,
              data = margex, family = 'binomial')
  z1 <- mod_marg2(mod1, var_interest = 'treatment',
                  type = 'effects', at = NULL)[[1]]

  # Make character outside
  data(margex)
  margex$treatment <- as.character(margex$treatment)
  mod2 <- glm(outcome ~ treatment + distance,
              data = margex, family = 'binomial')
  z2 <- mod_marg2(mod2, var_interest = 'treatment',
                  type = 'effects', at = NULL)[[1]]

  # Make factor inside equation
  data(margex)
  mod3 <- glm(outcome ~ as.factor(treatment) + distance,
              data = margex, family = 'binomial')
  z3 <- mod_marg2(mod3, var_interest = 'treatment',
                  type = 'effects', at = NULL)[[1]]

  expect_equal(z1$Margin, z2$Margin, z3$Margin, c(.0791146, .2600204),
               tolerance = 0.0001)
  expect_equal(z1$Standard.Error, z2$Standard.Error, z3$Standard.Error,
               c(.0069456, .0111772), tolerance = 0.0001)
  expect_equal(z1$Test.Stat, z2$Test.Stat, z3$Test.Stat,
               c(11.39, 23.26), tolerance = 0.001)
  expect_equal(z1$P.Value, z2$P.Value, z3$P.Value,
               c(0, 0), tolerance = 0.001)
  expect_equal(z1$`Lower CI (95%)`, z2$`Lower CI (95%)`, z3$`Lower CI (95%)`,
               c(.0655016, .2381135), tolerance = 0.0001)
  expect_equal(z1$`Upper CI (95%)`, z2$`Upper CI (95%)`, z3$`Upper CI (95%)`,
               c(.0927277, .2819272), tolerance = 0.0001)

  expect_equal(z1$Margin, c(.1809057), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.0131684), tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(13.74), tolerance = 0.001)
  expect_equal(z$P.Value, c(0), tolerance = 0.001)
  expect_equal(z$`Lower CI (95%)`, c(.1550961), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.2067153), tolerance = 0.0001)



})

test_that("works correctly even when terms are dropped", {

  data(margex)
  margex$distance[1:5] <- NA

  mod <- glm(outcome ~ treatment + distance,
             data = margex, family = 'binomial')
  z <- mod_marg2(mod, 'treatment', 'levels', at = NULL)[[1]]

  expect_equal(z$Margin, c(0.07911049, 0.25890416), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0.006945149, 0.011181260),
               tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(11.39, 23.16), tolerance = 0.001)
  expect_equal(z$P.Value, c(0, 0), tolerance = 0.001)
  expect_equal(z$`Lower CI (95%)`, c(.0654982, .2369893), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.0927227,.280819), tolerance = 0.0001)

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
  expect_equal(eff1$`disp = 90`$Margin, c(0, 6.7650630),
               tolerance = 0.0001)
  expect_equal(eff1$`disp = 90`$Standard.Error, c(0, 2.509522),
               tolerance = 0.0001)
  expect_equal(eff1$`disp = 90`$P.Value, c(NaN, 0.0135366),
               tolerance = 0.0001)
  expect_equal(eff1$`disp = 425`$Margin, c(0, 9.7177810),
               tolerance = 0.0001)
  expect_equal(eff1$`disp = 425`$Standard.Error, c(0, 5.924503),
               tolerance = 0.0001)
  expect_equal(eff1$`disp = 425`$P.Value, c(NaN, 0.1158436),
               tolerance = 0.0001)

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


test_that("Effects and Levels of Continuous Covariates", {

  data(mtcars)
  mtcars$gear <- factor(mtcars$gear)

  # Stata Commands:
  # logit vs c.mpg##c.disp i.gear
  # margins, at(mpg = (15 21))

  mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')
  z <- mod_marg2(mod = mm, var_interest = 'mpg',
                 at_var_interest = c(15,21),
                 type = "levels")[[1]]

  expect_equal(z$Margin, c(.5722642, .4384398), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.0316163, .0174034), tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(18.10, 25.19), tolerance = 0.0001)
  expect_equal(z$P.Value, c(0, 0), tolerance = 0.0001)

  # Stata Commands
  # webuse margex
  # reg y c.age##c.distance i.sex
  # margins, at(age = (35 60) distance = (13 25))
  data(margex)
  margex$sex <- factor(margex$sex)
  mm <- glm(y ~ sex + age * distance, margex, family = 'gaussian')
  z <- mod_marg2(mod = mm, var_interest = 'age',
                 at_var_interest = c(35, 60),
                 at = list('distance' = c(13, 25)),
                 type = "levels")
  expect_equal(z$`distance = 13` $Margin, c(72.38828, 60.04454),
               tolerance = 0.0001)
  expect_equal(z$`distance = 13`$Standard.Error, c(0.4134559, 0.7986055),
               tolerance = 0.0001)
  expect_equal(z$`distance = 13`$Test.Stat, c(175.08, 75.19),
               tolerance = 0.0001)

  expect_equal(z$`distance = 25` $Margin, c(72.32523, 59.91826),
               tolerance = 0.0001)
  expect_equal(z$`distance = 25`$Standard.Error, c(0.4081671, 0.7874008),
               tolerance = 0.0001)
  expect_equal(z$`distance = 25`$Test.Stat, c(177.20, 76.10),
               tolerance = 0.0001)

  # Stata Commands
  # webuse margex
  # reg y c.age i.sex
  # margins, at(age = (35 60))
  mm <- glm(y ~ sex + age, margex, family = 'gaussian')
  z <- mod_marg2(mod = mm, var_interest = 'age',
                 at_var_interest = c(25, 55),
                 type = "levels")[[1]]

  expect_equal(z$Margin, c(77.19769, 62.06669), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.6200359, .6309828), tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(124.51, 98.37), tolerance = 0.0001)

})

test_that("mod_marg2 input is checked", {

  # var_interest shouldn't be a character
  data(margex)
  mm <- glm(y ~ sex + age, margex, family = 'gaussian')
  expect_error(mod_marg2(mod = mm, var_interest = 'sex'))

  # lm should fail
  margex$sex <- factor(margex$sex)
  ml <- lm(y ~ sex + age, margex)
  expect_error(mod_marg2(mod = lm, var_interest = 'sex'))

  # extrapolated values are troubling
  mm <- glm(y ~ sex + age, margex, family = 'gaussian')
  expect_warning(mod_marg2(mod = mm, var_interest = 'sex',
                           at = list(age = 100)))
})

