#' @export
calc_convexity_adjustment.default <- function(instrument) {
  NULL
}

#' @export
calc_convexity_adjustment.rh_future <- function(instrument) {
  t <- daycount(instrument, instrument$spot_date, instrument$start_date)
  yf <- daycount(instrument, instrument$start_date, instrument$mat_date)
  a <- instrument$convexity_adj$mean_rev_speed
  sigma <- instrument$convexity_adj$volatility

  lambda <- sigma^2 * (1 - exp(-2 * a * t)) * (1 - exp(-a * yf))^2 / 2 / a^3
  phi <- sigma^2 * (1 - exp(-a * t))^2 * (1 - exp(-a * yf)) / 2 / a^3
  z <- lambda + phi

  fa <- exp(-z) * instrument$price + (1 - exp(-z)) * (1 + 1 / yf) * 100
  1 - fa / 100
}

#' @export
calc_inv_convexity_adjustment.default <- function(instrument) {
  NULL
}

#' @export
calc_inv_convexity_adjustment.rh_future <- function(instrument) {
  t <- daycount(instrument, instrument$spot_date, instrument$start_date)
  yf <- daycount(instrument, instrument$start_date, instrument$mat_date)
  a <- instrument$convexity_adj$mean_rev_speed
  sigma <- instrument$convexity_adj$volatility

  lambda <- sigma^2 * (1 - exp(-2 * a * t)) * (1 - exp(-a * yf))^2 / 2 / a^3
  phi <- sigma^2 * (1 - exp(-a * t))^2 * (1 - exp(-a * yf)) / 2 / a^3
  z <- lambda + phi

  fa <- 100 * (1 - instrument$quote)
  round(exp(z) * fa - (exp(z) - 1) * (1 + 1 / yf) * 100, 4)
}

# for (i in seq_along(inst_list)) {
#   if (insttype(inst_list[[i]]) == "future")
#     inst_list[[i]]$price <- calc_inv_convexity_adjustment(inst_list[[i]])
# }

# for (i in seq_along(inst_list)) {
#   if (insttype(inst_list[[i]]) == "future")
#     inst_list[[i]]$quote <- calc_convexity_adjustment(inst_list[[i]])
# }
