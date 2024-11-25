test_that("exponential_censored_distribution initializes correctly", {
  rate <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- exponential_censored_distribution$new(rate, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "exponential_censored_distribution")
})

test_that("exponential_censored function works", {
  rate <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- exponential_censored(rate, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "distribution_node")
})

test_that("normal_censored_distribution initializes correctly", {
  mean <- as.greta_array(0)
  sd <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- normal_censored_distribution$new(mean, sd, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "normal_censored_distribution")
})

test_that("normal_censored function works", {
  mean <- as.greta_array(0)
  sd <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- normal_censored(mean, sd, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "distribution_node")
})

test_that("lognormal_censored_distribution initializes correctly", {
  meanlog <- as.greta_array(0)
  sdlog <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- lognormal_censored_distribution$new(meanlog, sdlog, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "lognormal_censored_distribution")
})

test_that("lognormal_censored function works", {
  meanlog <- as.greta_array(0)
  sdlog <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- lognormal_censored(meanlog, sdlog, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "distribution_node")
})

test_that("student_censored_distribution initializes correctly", {
  df <- as.greta_array(3)
  loc <- as.greta_array(0)
  scale <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- student_censored_distribution$new(df, loc, scale, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "student_censored_distribution")
})

test_that("student_censored function works", {
  df <- as.greta_array(3)
  loc <- as.greta_array(0)
  scale <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- student_censored(df, loc, scale, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "distribution_node")
})

test_that("gamma_censored_distribution initializes correctly", {
  shape <- as.greta_array(2)
  rate <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- gamma_censored_distribution$new(shape, rate, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "gamma_censored_distribution")
})

test_that("gamma_censored function works", {
  shape <- as.greta_array(2)
  rate <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- gamma_censored(shape, rate, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "distribution_node")
})

test_that("weibull_censored_distribution initializes correctly", {
  shape <- as.greta_array(2)
  scale <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- weibull_censored_distribution$new(shape, scale, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "weibull_censored_distribution")
})

test_that("weibull_censored function works", {
  shape <- as.greta_array(2)
  scale <- as.greta_array(1)
  is_censored <- as.greta_array(0)
  dist <- weibull_censored(shape, scale, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "distribution_node")
})

test_that("pareto_censored_distribution initializes correctly", {
  scale <- as.greta_array(1)
  alpha <- as.greta_array(2)
  is_censored <- as.greta_array(0)
  dist <- pareto_censored_distribution$new(scale, alpha, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "pareto_censored_distribution")
})

test_that("pareto_censored function works", {
  scale <- as.greta_array(1)
  alpha <- as.greta_array(2)
  is_censored <- as.greta_array(0)
  dist <- pareto_censored(scale, alpha, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "distribution_node")
})

test_that("beta_censored_distribution initializes correctly", {
  alpha <- as.greta_array(2)
  beta <- as.greta_array(2)
  is_censored <- as.greta_array(0)
  dist <- beta_censored_distribution$new(alpha, beta, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "beta_censored_distribution")
})

test_that("beta_censored function works", {
  alpha <- as.greta_array(2)
  beta <- as.greta_array(2)
  is_censored <- as.greta_array(0)
  dist <- beta_censored(alpha, beta, is_censored, "right", 0, 1, dim = c(1, 1))
  expect_s3_class(dist, "distribution_node")
})
