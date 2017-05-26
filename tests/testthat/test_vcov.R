library(modmarg)
context("Use different variance-covariance matrices")

test_that("clustered standard errors are correct", {

  # Gaussian model

  data(margex)
  mod <- glm(outcome ~ treatment + distance, data = margex, family = 'gaussian')

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

  # Binary model

  data(margex)
  mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
  data(cvcov)
  v <- cvcov$logit$clust
  d <- cvcov$logit$stata_dof
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

test_that("clustered standard errors work with interaction terms", {

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
