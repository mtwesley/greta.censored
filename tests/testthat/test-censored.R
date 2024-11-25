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