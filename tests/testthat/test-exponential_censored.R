# Test script for exponential_censored distribution

library(greta)
library(testthat)

test_that("exponential_censored distribution works correctly", {
  # Simulate data
  set.seed(202)
  n <- 100
  true_rate <- 0.5
  y <- rexp(n, rate = true_rate)

  # Introduce left censoring at y < 0.5
  censoring_threshold <- 0.5
  is_censored <- y < censoring_threshold
  y_obs <- ifelse(is_censored, censoring_threshold, y)

  # Data preparation
  y_greta <- as_data(y_obs)
  is_censored_greta <- as_data(as.numeric(is_censored))

  # Define the model
  rate <- variable(lower = 0)

  distribution(y_greta) <- exponential_censored(
    rate = rate,
    is_censored = is_censored_greta,
    censoring_type = "left",
    lower = NULL,
    upper = NULL,
    dim = n
  )

  # Model fitting
  m <- model(rate)
  draws <- mcmc(m, n_samples = 1000)

  # Output results
  summary(draws)
})
