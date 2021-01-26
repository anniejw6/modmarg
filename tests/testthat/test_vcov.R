library(modmarg)

# -------------------------------------------
context("Test clustered standard errors")

test_that("clustered SEs work in OLS", {

  data(margex)
  mod <- glm(outcome ~ treatment + distance,
             data = margex, family = 'gaussian')

  data(cvcov)
  v <- cvcov$ols$clust
  d <- cvcov$ols$stata_dof

  z <- marg(mod, var_interest = 'treatment',
                 type = 'levels', vcov_mat = v, dof = d)[[1]]

  # stata
  # reg outcome i.treatment distance, vce(cluster arm)
  # margins i.treatment

  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   .0802249   .0481047  1.66771   0.2373     -.126753    .2872028
  #           1  |   .2588702   .0467188  5.54103   0.0311     .0578554    .4598851
  # ------------------------------------------------------------------------------

  expect_equal(z$Margin, c(0.0802249, 0.2588702), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0.0481047, 0.0467188),
               tolerance = 0.0001)
  expect_equal(z$P.Value, c(0.2373, 0.0311), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(-.126753, .0578554),
               tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.2872028, .4598851),
               tolerance = 0.0001)

  expect_warning(marg(mod, var_interest = 'treatment',
                           type = 'levels', vcov_mat = v))
})

test_that("clustered SEs work with logit", {

  data(margex)
  mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
  data(cvcov)
  v <- cvcov$logit$clust
  z <- marg(mod, var_interest = 'treatment',
                 type = 'levels', vcov_mat = v)[[1]]

  # stata
  # logit outcome i.treatment distance, vce(cluster arm)
  # margins i.treatment
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Pr(outcome), predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   .0791146   .0473008  1.67259   0.0944    -.0135933    .1718226
  #           1  |   .2600204   .0472214  5.50640   0.0000      .167468    .3525727
  # ------------------------------------------------------------------------------
  #

  expect_equal(z$Margin, c(0.0791146, 0.2600204), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0.0473008, 0.0472214),
               tolerance = 0.0001)
  expect_equal(z$P.Value, c(0.0944, 0.0000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(-.0135933, .167468),
               tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.1718226, .3525727),
               tolerance = 0.0001)

})

test_that("clustered SEs work with interactions", {

  data(mtcars)
  data(cvcov)
  mtcars$am <- factor(mtcars$am)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$cyl[c(1, 10, 20, 31)] <- NA

  v <- cvcov$weird$clust
  d <- cvcov$weird$stata_dof

  mod <- glm(mpg ~ cyl * poly(disp, degree = 2, raw = TRUE) + hp,
             data = mtcars)
  z <- marg(
    mod = mod, var_interest = 'cyl', type = 'levels',
    at = list('disp' = 400),
    vcov_mat = v, dof = d)[[1]]

  # stata
  # reg mpg cyl##c.disp##c.disp hp, vce(cluster gear)
  # margins cyl, at(disp = 400)
  # set sformat %8.5f
  # set pformat %5.4f"
  #
  # Predictive margins                                Number of obs   =         28
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  # at           : disp            =         400
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #          cyl |
  #           4  |   139.2514   156.4439  0.89010   0.4673    -533.8722     812.375
  #           6  |   67.13311   10.27523  6.53349   0.0226     22.92237    111.3439
  #           8  |    16.9802   .6700651 25.34113   0.0016     14.09715    19.86326
  # ------------------------------------------------------------------------------

  expect_equal(z$Margin, c(139.25140, 67.13311, 16.9802),
               tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(156.4439000, 10.2752300, 0.6700651),
               tolerance = 0.0001)
  expect_equal(z$Test.Stat, c(0.89010, 6.53349, 25.34113),
               tolerance = 0.0001)
  expect_equal(z$P.Value, c(0.4673, 0.0226, 0.0016),
               tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(-533.8722, 22.92237, 14.09715),
               tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(812.375, 111.3439, 19.86326),
               tolerance = 0.0001)

})

# -------------------------------------------
context("Test robust standard errors")

test_that("robust SEs work with OLS", {
  data(margex)
  mod <- glm(outcome ~ treatment + distance,
             data = margex, family = 'gaussian')

  data(rvcov)
  v <- rvcov$ols

  z <- marg(
    mod = mod, var_interest = 'treatment', type = 'levels',
    vcov_mat = v, dof = mod$df.residual)[[1]]

  # stata
  # . reg outcome i.treatment c.distance, robust
  #
  # Linear regression                                      Number of obs =    3000
  # F(  2,  2997) =  211.53
  # Prob > F      =  0.0000
  # R-squared     =  0.0701
  # Root MSE      =  .36212
  #
  # ------------------------------------------------------------------------------
  #              |               Robust
  #      outcome |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   .1786453   .0131956    13.54   0.000     .1527721    .2045186
  #     distance |  -.0002314   .0000128   -18.10   0.000    -.0002564   -.0002063
  #        _cons |   .0937792   .0073598    12.74   0.000     .0793484    .1082101
  # ------------------------------------------------------------------------------
  #
  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   .0802249   .0070072    11.45   0.000     .0664855    .0939643
  #           1  |   .2588702   .0111928    23.13   0.000     .2369238    .2808167
  # ------------------------------------------------------------------------------

  expect_equal(z$Label, as.factor(c('treatment = 0', 'treatment = 1')))
  expect_equal(z$Margin, c(.0802249, .2588702), tolerance = 0.0000001)
  expect_equal(z$Standard.Error, c(.0070072, .0111928), tolerance = 0.0000001)
  expect_equal(z$Test.Stat, c(11.45, 23.13), tolerance = 0.0001)
  expect_equal(z$P.Value, c(0, 0), tolerance = 0.00001)
  expect_equal(z$`Lower CI (95%)`, c(.0664855, .2369238), tolerance = 0.000001)
  expect_equal(z$`Upper CI (95%)`, c(.0939643, .2808167), tolerance = 0.000001)
})
