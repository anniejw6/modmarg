library(modmarg)
context("Calculate margins for other GLM families / links")

test_that("Probit models are correct", {

  data(margex)
  mod <- glm(outcome ~ as.factor(treatment) * distance, data = margex,
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

  # EFFECTS

  z <- mod_marg2(mod, var_interest = 'treatment', type = 'effects')[[1]]
  # stata
  # probit outcome i.treatment##distance
  # margins, dydx(treatment)
  #
  # Average marginal effects                          Number of obs   =       3000
  # Model VCE    : OIM
  #
  # Expression   : Pr(outcome), predict()
  # dy/dx w.r.t. : 1.treatment
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   .1803807   .0131534    13.71   0.000     .1546004    .2061609
  # ------------------------------------------------------------------------------

  expect_equal(z$Margin, c(0, .1803807), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0, .0131534), tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(NaN, 13.71), tolerance = 0.01)
  expect_equal(z$`Lower CI (95%)`, c(0, .1546004), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(0, .2061609), tolerance = 0.0001)

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

  # EFFECTS

  # stata
  # poisson breaks i.wool##i.tension
  # margins, dydx(tension)
  #
  # Average marginal effects                          Number of obs   =         54
  # Model VCE    : OIM
  #
  # Expression   : Predicted number of events, predict()
  # dy/dx w.r.t. : 2.tension 3.tension
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #      tension |
  #           M  |        -10   1.867526    -5.35   0.000    -13.66028   -6.339716
  #           H  |  -14.72222   1.795914    -8.20   0.000    -18.24215   -11.20229
  # ------------------------------------------------------------------------------

  z <- mod_marg2(mod, var_interest = 'tension', type = 'effects')[[1]]
  expect_equal(z$Margin, c(0, -10.000, -14.72222), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0, 1.867526, 1.795914),
               tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(NaN, -5.35, -8.20), tolerance = 0.01)
  expect_equal(z$`Lower CI (95%)`, c(0, -13.66028, -18.24215),
               tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(0, -6.339716, -11.20229),
               tolerance = 0.0001)

  # Do offsets work?
  data(warpbreaks)
  set.seed(123456)
  warpbreaks$off <- c(
    3, 2, 1, 2, 4, 3, 3, 3, 2, 1, 2, 4, 3, 3, 4, 2, 4, 3, 3, 3,
    4, 2, 2, 3, 2, 4, 3, 2, 2, 1, 3, 4, 2, 1, 2, 4, 4, 2, 2, 3,
    2, 3, 2, 3, 2, 2, 2, 2, 3, 3, 2, 2, 1, 2)

  # stata
  # poisson breaks i.wool##i.tension, offset(off)
  # margins i.tension

  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #      tension |
  #           L  |   40.03297   1.596391    25.08   0.000      36.9041    43.16184
  #           M  |   22.89519   1.050774    21.79   0.000     20.83571    24.95467
  #           H  |    26.3223   1.369167    19.23   0.000     23.63878    29.00581
  # ------------------------------------------------------------------------------
  #
  # margins, dydx(tension)
  #
  # Average marginal effects                          Number of obs   =         54
  # Model VCE    : OIM
  #
  # Expression   : Predicted number of events, predict()
  # dy/dx w.r.t. : 2.tension 3.tension
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #      tension |
  #           M  |  -17.13778   1.911175    -8.97   0.000    -20.88361   -13.39194
  #           H  |  -13.71068   2.103113    -6.52   0.000     -17.8327   -9.588651
  # ------------------------------------------------------------------------------

  mod <- glm(breaks ~ wool * tension + offset(off), data = warpbreaks,
             family = "poisson")

  z1 <- mod_marg2(mod, var_interest = 'tension', type = 'levels')[[1]]
  z2 <- mod_marg2(mod, var_interest = 'tension', type = 'effects')[[1]]

  expect_equal(z1$Margin, c(40.03297, 22.89519, 26.3223),
               tolerance = 0.0001)
  expect_equal(z1$Standard.Error, c(1.596391, 1.050774, 1.369167),
               tolerance = 0.0001)
  expect_equal(z1$P.Value, c(0, 0, 0), tolerance = 0.0001)
  expect_equal(z1$`Lower CI (95%)`, c(36.9041, 20.83571, 23.63878),
               tolerance = 0.0001)
  expect_equal(z1$`Upper CI (95%)`, c(43.16184, 24.95467, 29.00581),
               tolerance = 0.0001)

  expect_equal(z2$Margin, c(0, -17.13778, -13.71068),
               tolerance = 0.0001)
  expect_equal(z2$Standard.Error, c(0, 1.911175, 2.103113),
               tolerance = 0.0001)
  expect_equal(z2$P.Value, c(NaN, 0, 0), tolerance = 0.0001)
  expect_equal(z2$`Lower CI (95%)`, c(0, -20.88361, -17.8327),
               tolerance = 0.0001)
  expect_equal(z2$`Upper CI (95%)`, c(0, -13.39194, -9.588651),
               tolerance = 0.0001)

})

