exponential_censored_distribution <- R6::R6Class(
  "exponential_censored_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(rate, is_censored, censoring_type, lower, upper, dim) {
      rate <- as.greta_array(rate)
      is_censored <- check_param_greta_array(is_censored)
      check_numeric_length_1(lower)
      check_numeric_length_1(upper)
      check_finite(lower)
      check_finite(upper)
      check_x_gte_y(lower, upper)

      dim <- check_dims(rate, is_censored, target_dim = dim)

      super$initialize("exponential_censored", dim)
      self$add_parameter(rate, "rate")
      self$add_parameter(is_censored, "is_censored")
      self$censoring_type <- censoring_type
      self$lower <- lower
      self$upper <- upper
    },
    tf_distrib = function(parameters, dag) {
      rate <- parameters$rate
      is_censored <- parameters$is_censored
      exp_dist <- tfp$distributions$Exponential(rate = rate)
      censored_log_prob <- switch(
        self$censoring_type,
        "right" = function(y) exp_dist$log_survival_function(y),
        "left" = function(y) exp_dist$log_cdf(y),
        "interval" = function(y) {
          log_cdf_upper <- exp_dist$log_cdf(self$upper)
          log_cdf_lower <- exp_dist$log_cdf(self$lower)
          tf$log(tf$exp(log_cdf_upper) - tf$exp(log_cdf_lower))
        },
        function(y) exp_dist$log_prob(y)
      )
      uncensored_log_prob <- function(y) exp_dist$log_prob(y)
      list(
        log_prob = function(y) {
          tf$where(
            tf$equal(is_censored, 1),
            censored_log_prob(y),
            uncensored_log_prob(y)
          )
        }
      )
    }
  )
)

exponential_censored <- function(rate, is_censored, censoring_type, lower, upper, dim) {
  distrib("exponential_censored", rate, is_censored, censoring_type = censoring_type, lower = lower, upper = upper, dim = dim)
}