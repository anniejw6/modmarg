library(modmarg)
context("Jacobian matricies")

test_that("jacobian works correctly", {

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

  expect_equal(calc_jacob(p0, as.matrix(covar0), ld_fun),
               c(0, 0.74390659, 0.07240388)[c(3, 1, 2)],
               tolerance = .0001)

  expect_equal(calc_jacob(p1, as.matrix(covar1), ld_fun),
               c(0.18766468, 2.1907626, 0.18766468)[c(3, 1, 2)],
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

  expect_equal(
    calc_jacob(p3, as.matrix(covar3), ld_fun),
    c(0, 0, 0.090535558, 1.316670875, 24.040145060, 0.004931502)[c(6, 1:5)],
    tolerance = .0001)

  expect_equal(
    calc_jacob(p4, as.matrix(covar4), ld_fun),
    c(0.02704185, 0, 0.56997483, 4.31214, 90.5598, 0.02704185)[c(6, 1:5)],
    tolerance = .0001)

  expect_equal(
    calc_jacob(p5, as.matrix(covar5), ld_fun),
    c(0, 0.00469992, 0.12826647, 0.52297052, 13.961691, 0.00469992)[c(6, 1:5)],
    tolerance = .0001)

})


test_that("jacobian of discrete variables works correctly", {

  z <- as.matrix(data.frame(a = 1:3,
                            b = 4:6,
                            c = 8:10))

  expect_equivalent(as.matrix(discrete_effect_jacob(z, 2)),
                    matrix(c(-1, -1, -1, 0, 0, 0, 1, 1, 1),
                           ncol = 3, nrow = 3, byrow = T))

  expect_equivalent(as.matrix(discrete_effect_jacob(z, 3)),
                    matrix(c(rep(-2, 3), rep(-1, 3), rep(0, 3)),
                           ncol = 3, nrow = 3, byrow = T))

})
