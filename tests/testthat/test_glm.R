library(modmarg)
context("Calculate everything correctly")

test_that("levels are calculated correctly despite different input types", {

  # http://www.stata.com/support/faqs/statistics/compute-standard-errors-with-margins/

  # Put as.character in the equation
  data(margex)
  mod1 <- glm(outcome ~ as.character(treatment) + distance,
              data = margex, family = 'binomial')
  z1 <- marg(mod1, var_interest = 'treatment',
                 type = 'levels', at = NULL)[[1]]

  # Make character outside
  data(margex)
  margex$treatment <- as.character(margex$treatment)
  mod2 <- glm(outcome ~ treatment + distance,
              data = margex, family = 'binomial')
  z2 <- marg(mod2, var_interest = 'treatment',
                 type = 'levels', at = NULL)[[1]]

  # Put as.factor inside equation
  data(margex)
  mod3 <- glm(outcome ~ as.factor(treatment) + distance,
              data = margex, family = 'binomial')
  z3 <- marg(mod3, var_interest = 'treatment',
                  type = 'levels', at = NULL)[[1]]

  expect_equal(z1$Margin, c(.0791146, .2600204), tolerance = 0.0001)
  expect_equal(z2$Margin, c(.0791146, .2600204), tolerance = 0.0001)
  expect_equal(z3$Margin, c(.0791146, .2600204), tolerance = 0.0001)

  expect_equal(z1$Standard.Error, c(.0069456, .0111772), tolerance = 0.0001)
  expect_equal(z2$Standard.Error, c(.0069456, .0111772), tolerance = 0.0001)
  expect_equal(z3$Standard.Error, c(.0069456, .0111772), tolerance = 0.0001)

  expect_equal(z1$Test.Stat, c(11.39, 23.26), tolerance = 0.001)
  expect_equal(z2$Test.Stat, c(11.39, 23.26), tolerance = 0.001)
  expect_equal(z3$Test.Stat, c(11.39, 23.26), tolerance = 0.001)

  expect_equal(z1$P.Value, c(0, 0), tolerance = 0.001)
  expect_equal(z2$P.Value, c(0, 0), tolerance = 0.001)
  expect_equal(z3$P.Value, c(0, 0), tolerance = 0.001)

  expect_equal(z1$`Lower CI (95%)`, c(.0655016, .2381135), tolerance = 0.0001)
  expect_equal(z2$`Lower CI (95%)`, c(.0655016, .2381135), tolerance = 0.0001)
  expect_equal(z3$`Lower CI (95%)`, c(.0655016, .2381135), tolerance = 0.0001)

  expect_equal(z1$`Upper CI (95%)`, c(.0927277, .2819272), tolerance = 0.0001)
  expect_equal(z2$`Upper CI (95%)`, c(.0927277, .2819272), tolerance = 0.0001)
  expect_equal(z3$`Upper CI (95%)`, c(.0927277, .2819272), tolerance = 0.0001)

})


test_that("effects are calculated correctly despite different input types", {

  # Put as.character in the equation
  data(margex)
  mod1 <- glm(outcome ~ as.character(treatment) + distance,
              data = margex, family = 'binomial')
  z1 <- marg(mod1, var_interest = 'treatment',
                  type = 'effects', at = NULL)[[1]]
  z1 <- z1[2, ]

  # Make character outside
  data(margex)
  margex$treatment <- as.character(margex$treatment)
  mod2 <- glm(outcome ~ treatment + distance,
              data = margex, family = 'binomial')
  z2 <- marg(mod2, var_interest = 'treatment',
                  type = 'effects', at = NULL)[[1]]
  z2 <- z2[2, ]

  # Put as.factor inside equation
  data(margex)
  mod3 <- glm(outcome ~ as.factor(treatment) + distance,
              data = margex, family = 'binomial')
  z3 <- marg(mod3, var_interest = 'treatment',
                  type = 'effects', at = NULL)[[1]]
  z3 <- z3[2, ]

  expect_equal(z1$Margin, c(.1809057), tolerance = 0.0001)
  expect_equal(z2$Margin, c(.1809057), tolerance = 0.0001)
  expect_equal(z3$Margin, c(.1809057), tolerance = 0.0001)

  expect_equal(z1$Standard.Error, c(.0131684), tolerance = 0.0001)
  expect_equal(z2$Standard.Error, c(.0131684), tolerance = 0.0001)
  expect_equal(z3$Standard.Error, c(.0131684), tolerance = 0.0001)

  expect_equal(z1$Test.Stat, c(13.74), tolerance = 0.001)
  expect_equal(z2$Test.Stat, c(13.74), tolerance = 0.001)
  expect_equal(z3$Test.Stat, c(13.74), tolerance = 0.001)

  expect_equal(z1$P.Value, c(0), tolerance = 0.001)
  expect_equal(z2$P.Value, c(0), tolerance = 0.001)
  expect_equal(z3$P.Value, c(0), tolerance = 0.001)

  expect_equal(z1$`Lower CI (95%)`, c(.1550961), tolerance = 0.0001)
  expect_equal(z2$`Lower CI (95%)`, c(.1550961), tolerance = 0.0001)
  expect_equal(z3$`Lower CI (95%)`, c(.1550961), tolerance = 0.0001)

  expect_equal(z1$`Upper CI (95%)`, c(.2067153), tolerance = 0.0001)
  expect_equal(z2$`Upper CI (95%)`, c(.2067153), tolerance = 0.0001)
  expect_equal(z3$`Upper CI (95%)`, c(.2067153), tolerance = 0.0001)

})

test_that("works correctly even when rows are dropped", {

  data(margex)
  margex$distance[1:5] <- NA

  mod <- glm(outcome ~ treatment + distance,
             data = margex, family = 'binomial')

  z <- marg(mod, var_interest = 'treatment', type = 'levels', at = NULL)[[1]]

  expect_equal(z$Margin, c(0.07911049, 0.25890416), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0.006945149, 0.011181260),
               tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(11.39, 23.16), tolerance = 0.001)
  expect_equal(z$P.Value, c(0, 0), tolerance = 0.001)
  expect_equal(z$`Lower CI (95%)`, c(.0654982, .2369893), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.0927227,.280819), tolerance = 0.0001)


  data(mtcars)
  mtcars$am <- factor(mtcars$am)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)
  mtcars$cyl[c(1, 10, 20, 31)] <- NA
  ols3 <- glm(mpg ~ cyl * poly(disp, degree = 2, raw = TRUE) +
                hp + gear, data = mtcars)
  eff3 <- marg(
    mod = ols3, var_interest = 'cyl', type = 'effects',
    at = list('disp' = seq(80, 470, 5)))

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

test_that("interaction terms", {

  data(mtcars)
  mtcars$am <- factor(mtcars$am)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)

  ols1 <- glm(mpg ~ am * poly(disp, degree = 2, raw = TRUE) +
                cyl + hp + gear, data = mtcars)

  eff1 <- marg(
    mod = ols1, var_interest = 'am', type = 'effects',
    at = list('disp' = seq(75, 470, 5)))


  ols2 <- glm(mpg ~ am * disp + am * I(disp^2) +
                cyl + hp + gear, data = mtcars)

  eff2 <- marg(
    mod = ols2, var_interest = 'am', type = 'effects',
    at = list('disp' = seq(75, 470, 5)))

  ols3 <- glm(mpg ~ am * poly(disp, degree = 2, raw = T) +
                cyl + hp + gear, data = mtcars)

  eff3 <- marg(
    mod = ols3, var_interest = 'am', type = 'effects',
    at = list('disp' = seq(75, 470, 5)))

  expect_equal(eff1, eff2)
  expect_equal(eff2, eff3)
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


})


test_that("Effects and Levels of Continuous Covariates", {

  data(mtcars)
  mtcars$gear <- factor(mtcars$gear)

  # Stata Commands:
  # logit vs c.mpg##c.disp i.gear
  # margins, at(mpg = (15 21))

  mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')
  z <- marg(mod = mm, var_interest = 'mpg',
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
  z <- marg(mod = mm, var_interest = 'age',
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
  z <- marg(mod = mm, var_interest = 'age',
                 at_var_interest = c(25, 55),
                 type = "levels")[[1]]

  expect_equal(z$Margin, c(77.19769, 62.06669), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.6200359, .6309828), tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(124.51, 98.37), tolerance = 0.0001)

})

test_that("Collinear cols treated right", {

  # Compare to stata command (load mtcars into stata after transformations)
  # stata
  # reg disp i.am##c.mpg
  # margins, dydx(am) at(mpg = (15(5)30))
  #-------------------------------------------------------------------"
  #    |            Delta-method"
  #    |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]"
  # ---+----------------------------------------------------------------"
  #2.am|"
  #_at |"
  # 1  |  -84.86612    36.3742    -2.33   0.027    -159.3753   -10.35695"
  # 2  |  -30.01541    28.0651    -1.07   0.294    -87.50416    27.47335"
  # 3  |    24.8353    37.5892     0.66   0.514    -52.16269    101.8333"
  # 4  |   79.68602   56.55951     1.41   0.170    -36.17088    195.5429"


  data(mtcars)
  mtcars$am <- factor(mtcars$am)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)
  mtcars$mpg_colin <- mtcars$mpg * 2 + 5

  ols1 <- glm(disp ~ am * mpg + mpg_colin, data = mtcars)

  eff1 <- marg(
    mod = ols1, var_interest = 'am', type = 'effects',
    at = list('mpg' = seq(15, 30, 5)))

  expect_equal(eff1$`mpg = 15`$Margin, c(0, -84.86612), tolerance = 0.0001)
  expect_equal(eff1$`mpg = 15`$Standard.Error, c(0, 36.3742),
               tolerance = 0.0001)
  expect_equal(eff1$`mpg = 15`$P.Value, c(NaN, 0.027), tolerance = 0.001)
  expect_equal(eff1$`mpg = 25`$Margin, c(0, 24.8353), tolerance = 0.0001)
  expect_equal(eff1$`mpg = 25`$Standard.Error, c(0, 37.5892),
               tolerance = 0.0001)
  expect_equal(eff1$`mpg = 25`$P.Value, c(NaN, 0.514), tolerance = 0.001)
})

test_that("marg subsets of data run properly", {

  # Stata Commands
  # webuse margex
  # reg y i.treatment##i.agegroup c.distance
  # margins i.treatment, by(agegroup)

  data(margex)
  mm <- glm(y ~ treatment * agegroup + distance, margex, family = 'gaussian')
  z1 <- marg(mod = mm, var_interest = 'treatment',
                  type = "levels",
                  data = subset(margex, agegroup == "20-29"))
  z2 <- marg(mod = mm, var_interest = 'treatment',
                  type = "levels",
                  data = subset(margex, agegroup == "30-39"))
  z3 <- marg(mod = mm, var_interest = 'treatment',
                  type = "levels",
                  data = subset(margex, agegroup == "40+"))

  expect_equal(z1[[1]]$Margin, c(68.67471, 83.40595), tolerance = 0.0001)
  expect_equal(z1[[1]]$Standard.Error, c(0.8908722, 1.3643423),
               tolerance = 0.0001)
  expect_equal(z2[[1]]$Margin, c(67.23932, 79.75863), tolerance = 0.0001)
  expect_equal(z2[[1]]$Standard.Error, c(1.004951, 1.164683),
               tolerance = 0.0001)
  expect_equal(z3[[1]]$Margin, c(58.87818, 71.36229), tolerance = 0.0001)
  expect_equal(z3[[1]]$Standard.Error, c(0.8580671, 0.6532233),
               tolerance = 0.0001)

  expect_error(marg(mod = mm, var_interest = 'treatment',
                         type = 'levels',
                         data = margex[, names(margex) != 'distance']))

})



test_that("marg input is checked", {

  # lm should fail
  margex$sex <- factor(margex$sex)
  ml <- lm(y ~ sex + age, margex)
  expect_error(marg(mod = ml, var_interest = 'sex'),
               "no applicable method for 'marg' applied to an object of class \"lm\"",
               fixed = T)

  # string formulas shouldn't work
  mm <- glm('y ~ sex + age', margex, family = 'gaussian')
  expect_error(marg(mod = mm, var_interest = 'sex'),
               regexp = "Estimate your model with a formula object, not a character string.")

  # extrapolated values are troubling
  mm <- glm(y ~ sex + age, margex, family = 'gaussian')
  expect_warning(marg(mod = mm, var_interest = 'sex',
                      at = list(age = 100)),
                 "Not all values in 'at' are in the range of 'age'",
                 fixed = TRUE)

  # extrapolated factors are broken
  mm <- glm(y ~ sex + agegroup, data = margex)
  expect_error(marg(mod = mm, var_interest = 'sex',
                    at = list(agegroup = '12')),
               "'12' is not a value in 'agegroup'",
               fixed = TRUE)

  # multiple extrapolated factors are broken
  mm <- glm(y ~ sex + agegroup, data = margex)
  expect_error(marg(mod = mm, var_interest = 'sex',
                    at = list(agegroup = c('12', '13'))),
               "'12, 13' is not a value in 'agegroup'",
               fixed = TRUE)
})


test_that("continuous effects not supported unless variable is binary",{

  data(margex)
  mod <- glm(y ~ sex + age, data = margex, family = 'gaussian')
  expect_error(marg(mod, var_interest = 'age', type = 'effects'))

  expect_true(is.numeric(margex$treatment))
  mod <- glm(y ~ treatment + age,
             data = margex, family = 'gaussian')
  z1 <- marg(mod, var_interest = 'treatment', type = 'effects')

  mod <- glm(y ~ as.factor(treatment) + age,
             data = margex, family = 'gaussian')
  z2 <- marg(mod, var_interest = 'treatment', type = 'effects')

  expect_equal(z1, z2)

})

test_that("Setting base level works", {

  data(margex)
  mm <- glm(y ~ as.factor(treatment) + age, margex, family = 'gaussian')

  # Setting base_rn without type = 'effects' does nothing (warning)
  expect_warning(marg(mod = mm, var_interest = 'treatment',
                           at = list(age = 50), base_rn = 2))

  # Actual check
  z1 <- marg(mod = mm, var_interest = 'treatment',
                  at = list(age = 50), base_rn = 1,
                  type = 'effects')[[1]]
  z2 <- marg(mod = mm, var_interest = 'treatment',
                  at = list(age = 50), base_rn = 2,
                  type = 'effects')[[1]]

  # stata
  # margins, dydx(treatment)
  #
  # Average marginal effects                          Number of obs   =       3000
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : treatment
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |   14.03271   .7777377    18.04   0.000     12.50775    15.55766
  # ------------------------------------------------------------------------------

  # Make sure effects flipped right
  expect_equal(z1$Margin, c(0, 14.03271), tolerance = 0.0001)
  expect_equal(rev(-1 * z2$Margin), c(0, 14.03271), tolerance = 0.0001)

  expect_equal(z1$Standard.Error, c(0, .7777377), tolerance = 0.0001)
  expect_equal(rev(z2$Standard.Error), c(0, .7777377), tolerance = 0.0001)

  expect_equal(z1$Test.Stat, c(NaN, 18.04), tolerance = 0.01)
  expect_equal(rev(-1 * z2$Test.Stat), c(NaN, 18.04), tolerance = 0.01)

  expect_equal(z1$P.Value, c(NaN, 0.000), tolerance = 0.001)
  expect_equal(rev(z2$P.Value), c(NaN, 0.000), tolerance = 0.001)

  expect_equal(z1$`Lower CI (95%)`, c(0, 12.50775), tolerance = 0.0001)
  expect_equal(rev(-1 * z2$`Upper CI (95%)`), c(0, 12.50775), tolerance = 0.0001)

  expect_equal(z1$`Upper CI (95%)`, c(0, 15.55766), tolerance = 0.0001)
  expect_equal(rev(-1 * z2$`Lower CI (95%)`), c(0, 15.55766), tolerance = 0.0001)

  # . reg y ib2.group##c.distance
  #
  #       Source |       SS       df       MS              Number of obs =    3000
  # -------------+------------------------------           F(  5,  2994) =    8.20
  #        Model |  18786.3692     5  3757.27384           Prob > F      =  0.0000
  #     Residual |  1372646.64  2994  458.465811           R-squared     =  0.0135
  # -------------+------------------------------           Adj R-squared =  0.0119
  #        Total |  1391433.01  2999  463.965657           Root MSE      =  21.412
  #
  # ----------------------------------------------------------------------------------
  #                y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -----------------+----------------------------------------------------------------
  #            group |
  #               1  |  -.3468524   .9366157    -0.37   0.711    -2.183328    1.489623
  #               3  |   5.082678   1.090568     4.66   0.000      2.94434    7.221016
  #                  |
  #         distance |  -.0056658   .0034517    -1.64   0.101    -.0124338    .0011023
  #                  |
  # group#c.distance |
  #               1  |  -.0013629   .0049395    -0.28   0.783     -.011048    .0083222
  #               3  |   -.002349   .0056076    -0.42   0.675    -.0133442    .0086462
  #                  |
  #            _cons |   69.10901   .6735036   102.61   0.000     67.78844    70.42959
  # ----------------------------------------------------------------------------------
  #
  # . margins, dydx(group) sformat(%7.6f) pformat(%5.4f)
  #
  # Average marginal effects                          Number of obs   =       3000
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.group 3.group
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #        group |
  #           1  |  -.4266991   .8902282 -0.479314   0.6318     -2.17222    1.318822
  #           3  |    4.94506   1.039902 4.755314   0.0000     2.906066    6.984055
  # ------------------------------------------------------------------------------
  #   Note: dy/dx for factor levels is the discrete change from the base level.

  data(margex)
  mm <- glm(y ~ as.factor(group) * distance, margex, family = 'gaussian')
  z <- marg(mm, 'group', type = 'effects', base_rn = 2)[[1]]
  expect_equal(z$Margin, c(-.4266991, 0, 4.94506), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.8902282, 0, 1.039902), tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(-0.479314, NaN, 4.755314), tolerance = 0.0001)
  expect_equal(z$P.Value, c(0.6318, NaN, 0), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(-2.17222, 0, 2.906066), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(1.318822, 0, 6.984055), tolerance = 0.0001)

})


test_that('using period works', {

  # Put as.character in the equation
  data(margex)
  margex <- margex[, c('outcome', 'treatment', 'distance')]
  mod1 <- glm(outcome ~ ., data = margex, family = 'binomial')
  z1 <- marg(mod1, var_interest = 'treatment',
             type = 'levels', at = NULL)[[1]]

  expect_equal(z1$Margin, c(.0791146, .2600204),
               tolerance = 0.0001)
  expect_equal(z1$Standard.Error,
               c(.0069456, .0111772), tolerance = 0.0001)
  expect_equal(z1$Test.Stat,
               c(11.39, 23.26), tolerance = 0.001)
  expect_equal(z1$P.Value,
               c(0, 0), tolerance = 0.001)
  expect_equal(z1$`Lower CI (95%)`,
               c(.0655016, .2381135), tolerance = 0.0001)
  expect_equal(z1$`Upper CI (95%)`,
               c(.0927277, .2819272), tolerance = 0.0001)

})
