library(modmarg)
context("Calculate margins for other GLM families / links")

test_that("Probit models are correct", {

  data(margex)
  mod <- glm(outcome ~ treatment * distance, data = margex,
             family = binomial(link = 'probit'))

  z <- mod_marg2(mod, var_interest = 'treatment', type = 'levels')[[1]]

  # stata
  # probit outcome i.treatment##distance
  # margins i.treatment
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : OIM
  #
  # Expression   : Pr(outcome), predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   .0792421    .006962    11.38   0.000     .0655968    .0928874
  #           1  |   .2596228   .0111599    23.26   0.000     .2377498    .2814958
  # ------------------------------------------------------------------------------

  expect_equal(z$Margin, c(.0792421, .2596228), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.006962, .0111599),
               tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(11.38, 23.26), tolerance = 0.01)
  expect_equal(z$P.Value, c(0.000, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(.0655968, .2377498),
               tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.0928874, .2814958),
               tolerance = 0.0001)

})

test_that("Poisson models are correct", {

  data(warpbreaks)
  mod <- glm(breaks ~ wool * tension, data = warpbreaks,
             family = "poisson")

  z <- mod_marg2(mod, var_interest = 'tension', type = 'levels')[[1]]

  # stata
  # poisson breaks i.wool##i.tension
  # margins i.tension
  #
  # Predictive margins                                Number of obs   =         54
  # Model VCE    : OIM
  #
  # Expression   : Predicted number of events, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #      tension |
  #           L  |   36.38889   1.421832    25.59   0.000     33.60215    39.17563
  #           M  |   26.38889   1.210805    21.79   0.000     24.01575    28.76202
  #           H  |   21.66667   1.097134    19.75   0.000     19.51632    23.81701
  # ------------------------------------------------------------------------------
  #

  expect_equal(z$Margin, c(36.38889, 26.38889, 21.66667),
               tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(1.421832, 1.210805, 1.097134),
               tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(25.59, 21.79, 19.75), tolerance = 0.01)
  expect_equal(z$P.Value, c(0.000, 0.000, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(33.60215, 24.01575, 19.51632),
               tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(39.17563, 28.76202, 23.81701),
               tolerance = 0.0001)

})

