# Test script for beta_censored distribution

library(greta)
library(testthat)
library(reticulate)

test_that("beta_censored distribution works correctly", {
  # Simulate data
  set.seed(505)
  n <- 100
  true_alpha <- 2
  true_beta <- 5
  y <- rbeta(n, shape1 = true_alpha, shape2 = true_beta)

  # Introduce interval censoring between 0.2 and 0.8
  lower_bound <- 0.2
  upper_bound <- 0.8
  is_censored <- y > lower_bound & y < upper_bound
  y_obs <- y
  y_obs[is_censored] <- NA # Interval censored data

  # Data preparation
  y_greta <- as_data(ifelse(is.na(y_obs), 0, y_obs)) # Placeholder for censored data
  is_censored_greta <- as_data(as.numeric(is_censored))

  # Define the model
  alpha <- variable(lower = 0)
  beta <- variable(lower = 0)

  distribution(y_greta) <- beta_censored(
    alpha = alpha,
    beta = beta,
    is_censored = is_censored_greta,
    censor = "interval",
    lower = lower_bound,
    upper = upper_bound,
    dim = n
  )

  # Model fitting
  m <- model(alpha, beta)
  draws <- mcmc(m, n_samples = 1000)

  # Output results
  summary(draws)

  # Print Python errors before the expectation
  print(py_last_error())

  # Add meaningful expectations
  expect_true(mean(draws$alpha) > 1.5 && mean(draws$alpha) < 2.5)
  expect_true(mean(draws$beta) > 4.5 && mean(draws$beta) < 5.5)
})
