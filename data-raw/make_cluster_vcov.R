
# Create clustered variance-covariance matrix for package tests

rm(list = ls())

cluster_se <- function(model, cluster, data = model$data){
  data <- data[, names(data) %in% c(all.vars(model$formula), cluster)]
  data <- data[complete.cases(data), ]

  # get parameters
  n <- nrow(data)
  m <- length(unique(data[, cluster]))
  k <- length(coef(model))

  # sum over clusters using data.table
  u <- data.table::data.table(sandwich::estfun(model))
  u$cluster <- data[, cluster]
  u_clust <- u[, lapply(.SD, sum), keyby = cluster]
  # Drop cluster variable, go back to matrix
  u_clust <- as.matrix(u_clust)[, -1]

  # Sandwich estimator and DFC correction is different for different models
  if(family(model)[['link']] == 'identity'){
    dfc <- (m / (m - 1)) * ((n - 1) / (n - k))
    clust <- dfc * sandwich::sandwich(model, meat = crossprod(u_clust) / n)
  } else if(family(model)[['link']] == 'logit'){
    dfc <- m / (m - 1)
    clust <- vcov(model) %*% (dfc * t(u_clust) %*% u_clust) %*% vcov(model)
  }
  list(clust = clust, stata_dof = m - 1)
}

# Create sample vcov matrix + degrees of freedom for vcov test
# Page 29 "Improved Critical Values using a T-distribution"
# http://cameron.econ.ucdavis.edu/research/Cameron_Miller_JHR_2015_February.pdf

# OLS
data(margex)
margex$treatment <- factor(margex$treatment)
mod <- glm(outcome ~ treatment + distance, data = margex, family = 'gaussian')
ols_cvcov <- cluster_se(mod, "arm")

# Logit
data(margex)
margex$treatment <- factor(margex$treatment)
mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
logit_cvcov <- cluster_se(mod, "arm")

# Polynomial interaction, dropping rows
data(mtcars)
mtcars$am <- factor(mtcars$am)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$cyl[c(1, 10, 20, 31)] <- NA

mod <- glm(mpg ~ cyl * poly(disp, degree = 2, raw = TRUE) + hp,
           data = mtcars)
weird_cvcov <- cluster_se(mod, "gear")

cvcov <- list(ols = ols_cvcov, logit = logit_cvcov, weird = weird_cvcov)
devtools::use_data(cvcov, overwrite = TRUE)
