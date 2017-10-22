library(modmarg)
context("Predicted SE")

test_that("se is calculated correctly", {

  data(margex)
  mm <- glm(outcome ~ treatment + distance, margex, family = 'binomial')

  df0 <- transform(mm$data, treatment = 0)
  df1 <- transform(mm$data, treatment = 1)

  covar0 <- model.matrix(mm$formula, df0)
  covar1 <- model.matrix(mm$formula, df1)

  # Type = response?? question of when to convert, etc.
  p0 <- predict(mm, newdata = df0)
  p1 <- predict(mm, newdata = df1)

  binom_family <- make.link('logit')
  ld_fun <- binom_family$mu.eta

  expect_equal(calc_pred_se(vcov_model = vcov(mm),
                       jac = calc_jacob(p0, as.matrix(covar0), ld_fun)),
               c(.0069456),
               tolerance = .0001)

  expect_equal(calc_pred_se(vcov_model = vcov(mm),
                       jac = calc_jacob(p1, as.matrix(covar1), ld_fun)),
               c(.0111772),
               tolerance = .0001)

  data(mtcars)
  mtcars$gear <- factor(mtcars$gear)
  mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')

  df3 <- transform(mm$data, gear = factor(3, levels = levels(mtcars$gear)))
  df4 <- transform(mm$data, gear = factor(4, levels = levels(mtcars$gear)))
  df5 <- transform(mm$data, gear = factor(5, levels = levels(mtcars$gear)))

  covar3 <- model.matrix(mm$formula, df3)
  covar4 <- model.matrix(mm$formula, df4)
  covar5 <- model.matrix(mm$formula, df5)

  # Type = response?? question of when to convert, etc.
  p3 <- predict(mm, newdata = df3)
  p4 <- predict(mm, newdata = df4)
  p5 <- predict(mm, newdata = df5)

  expect_equal(calc_pred_se(vcov_model = vcov(mm),
                       jac = calc_jacob(p3, as.matrix(covar3), ld_fun)),
               c(0.0124141),
               tolerance = .0001)

  expect_equal(calc_pred_se(vcov_model = vcov(mm),
                       jac = calc_jacob(p4, as.matrix(covar4), ld_fun)),
               c(0.0290699),
               tolerance = .0001)

  expect_equal(calc_pred_se(vcov_model = vcov(mm),
                       jac = calc_jacob(p5, as.matrix(covar5), ld_fun)),
               c(0.0121496),
               tolerance = .0001)

})
