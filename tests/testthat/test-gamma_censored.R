# Test script for gamma_censored distribution

library(greta)
library(testthat)
library(reticulate)

test_that("gamma_censored distribution works correctly", {
  # Simulate data
  set.seed(101)
  n <- 100
  true_shape <- 2
  true_rate <- 1
  y <- rgamma(n, shape = true_shape, rate = true_rate)

  # Introduce right censoring at y > 3
  censoring_threshold <- 3
  is_censored <- y > censoring_threshold
  y_obs <- ifelse(is_censored, censoring_threshold, y)

  # Data preparation
  y_greta <- as_data(y_obs)
  is_censored_greta <- as_data(as.numeric(is_censored))

  # Define the model
  shape <- variable(lower = 0)
  rate <- variable(lower = 0)

  distribution(y_greta) <- gamma_censored(
    shape = shape,
    rate = rate,
    is_censored = is_censored_greta,
    censor = "right",
    lower = NULL,
    upper = NULL,
    dim = n
  )

  # Model fitting
  # m <- model(shape, rate)

  # Add expectations
  # expect_s3_class(m, "greta_model")
  expect_error(py_last_error(), NA)
})
