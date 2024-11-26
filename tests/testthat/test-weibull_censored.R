# Test script for weibull_censored distribution

library(greta)
library(testthat)
library(reticulate)

test_that("weibull_censored distribution works correctly", {
  # Simulate data
  set.seed(303)
  n <- 100
  true_shape <- 1.5
  true_scale <- 1
  y <- rweibull(n, shape = true_shape, scale = true_scale)

  # Introduce right censoring at y > 2
  censoring_threshold <- 2
  is_censored <- y > censoring_threshold
  y_obs <- ifelse(is_censored, censoring_threshold, y)

  # Data preparation
  y_greta <- as_data(y_obs)
  is_censored_greta <- as_data(as.numeric(is_censored))

  # Define the model
  shape <- variable(lower = 0)
  scale <- variable(lower = 0)

  distribution(y_greta) <- weibull_censored(
    shape = shape,
    scale = scale,
    is_censored = is_censored_greta,
    censor = "right",
    lower = NULL,
    upper = NULL,
    dim = n
  )

  # Model fitting
  m <- model(shape, scale)
  draws <- mcmc(m, n_samples = 1000)

  # Output results
  summary(draws)

  # Print Python errors before the expectation
  print(py_last_error())

  # Add meaningful expectations
  expect_true(mean(draws$shape) > 1 && mean(draws$shape) < 2)
  expect_true(mean(draws$scale) > 0.5 && mean(draws$scale) < 1.5)
})
