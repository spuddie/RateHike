#' @export
bootstrap.rh_curve <- function(curve, inst_list) {
  curve <- set_times(curve, inst_list)
  curve <- set_bootstrap_options(curve)
  curve <- set_interpolate(curve)
  root_find_tol <- get_rh_param('root_find_tol')
  root_find_int_radius <- get_rh_param('root_find_interval_radius')
  spline_update_diff_tol <- get_rh_param('spline_update_diff_tol')
  iter <- 1
  prev_dfs <- rep(0, curve$interpolate$dim_t)
  update_diff <- 1
  while (iter <= curve$bootstrap$max_iter && 
         update_diff > spline_update_diff_tol) {
    for (i in seq_along(inst_list)) {
      if (insttype(inst_list[[i]]) == "synthetic")
        curve <- update_discount_factors(curve, 
                                         inst_list[[i]]$discount_factor, 
                                         pillar = i + 1)
      else {
        if (iter == 1)
          interval <- (1 + c(-1, 1) * root_find_int_radius) *
            get_discount_factors(curve, pillars = i)
        else
          interval <- (1 + c(-1, 1) * root_find_int_radius) *
            get_discount_factors(curve, pillars = i + 1)
        root_res <- uniroot(function(df) try_pillar(curve, 
                                                    inst_list[[i]], 
                                                    df, 
                                                    i + 1), 
                            interval, 
                            extendInt = "yes", 
                            check.conv = TRUE, 
                            tol = root_find_tol)
        curve <- update_discount_factors(curve, 
                                         root_res$root, 
                                         pillar = i + 1)
        curve$bootstrap$func_evals <-
          curve$bootstrap$func_evals + 3 + root_res$iter
      }
    }
    iter <- iter + 1
    next_dfs <- get_discount_factors(curve)
    update_diff <- max(abs(prev_dfs - next_dfs))
    prev_dfs <- next_dfs
  }
  curve$bootstrap$iterations <- iter - 1
  curve$bootstrap$update_diff <- update_diff
  curve
}

#' @export
get_discount_factors.rh_interp_df <- function(curve,
                                              dates = NULL,
                                              times = NULL,
                                              pillars = NULL) {
  result <- get_curve_data(curve, dates, times, pillars)
  result$data
}

#' @export
get_discount_factors.rh_interp_logdf <- function(curve, 
                                                 dates = NULL, 
                                                 times = NULL, 
                                                 pillars = NULL) {
  result <- get_curve_data(curve, dates, times, pillars)
  exp(result$data)
}

#' @export
get_discount_factors.rh_interp_zero <- function(curve, 
                                                dates = NULL, 
                                                times = NULL, 
                                                pillars = NULL) {
  result <- get_curve_data(curve, dates, times, pillars)
  exp(-result$data * result$times)
}

#' @export
get_insta_fwd.rh_interp_df <- function(curve, 
                                       dates = NULL, 
                                       times = NULL, 
                                       pillars = NULL) {
  result <- get_curve_data(curve, dates, times, pillars)
  res_deriv <-
    get_curve_data(curve, dates, times, pillars, deriv = TRUE)
  - res_deriv$data / result$data
}

#' @export
get_insta_fwd.rh_interp_logdf <- function(curve, 
                                          dates = NULL, 
                                          times = NULL, 
                                          pillars = NULL) {
  res_deriv <-
    get_curve_data(curve, dates, times, pillars, deriv = TRUE)
  - res_deriv$data
}

#' @export
get_insta_fwd.rh_interp_zero <- function(curve, 
                                         dates = NULL, 
                                         times = NULL, 
                                         pillars = NULL) {
  result <- get_curve_data(curve, dates, times, pillars)
  res_deriv <-
    get_curve_data(curve, dates, times, pillars, deriv = TRUE)
  result$data + result$times * res_deriv$data
}

#' @export
get_zero_rates.rh_interp_df <- function(curve,
                                        dates = NULL,
                                        times = NULL,
                                        pillars = NULL) {
  result <- get_curve_data(curve, dates, times, pillars)
  - log(result$data) / result$times
}

#' @export
get_zero_rates.rh_interp_logdf <- function(curve,
                                           dates = NULL,
                                           times = NULL,
                                           pillars = NULL) {
  result <- get_curve_data(curve, dates, times, pillars)
  - result$data / result$times
}

#' @export
get_zero_rates.rh_interp_zero <- function(curve,
                                          dates = NULL,
                                          times = NULL,
                                          pillars = NULL) {
  result <- get_curve_data(curve, dates, times, pillars)
  result$data
}

#' @export
re_anchor.default <- function(curve, depo_sn) {
  curve$spot_date_orig <- curve$spot_date
  curve
}

#' @export
re_anchor.rh_anchor_11 <- function(curve, depo_sn) {
  olddf <- get_discount_factors(curve)
  tomorrow <- calc_next_date.default(NULL, curve$today_date, 1, "DY")
  pillar_dates <- c(curve$today_date, tomorrow, get_dates(curve))
  newtimes <- daycount(curve, curve$today_date, pillar_dates)
  curve$spot_date_orig <- curve$spot_date
  curve$spot_date <- curve$today_date
  curve$times <- newtimes
  curve$interpolate$dim_t <- length(curve$times)
  curve$interpolate$dim_d <- length(curve$times)
  curve$interpolate$coeff <- array(NA, c(curve$interpolate$dim_c,
                                         curve$interpolate$dim_t))
  update_discount_factors(curve, c(1, 1, olddf))
}

#' @export
re_anchor.rh_anchor_date <- function(curve, depo_sn) {
  curve$spot_date_orig <- curve$spot_date
  curve$spot_date <- curve$today_date
  curve
}

#' @export
re_anchor.rh_anchor_ontn <- function(curve, depo_sn) {
  olddf <- get_discount_factors(curve)
  tomorrow <- calc_next_date.default(NULL, curve$today_date, 1, "DY")
  pillar_dates <- c(curve$today_date, tomorrow, get_dates(curve))
  newtimes <- daycount(curve, curve$today_date, pillar_dates)
  yearfractions <- diff(daycount(depo_sn, curve$today_date, pillar_dates[1:3]))
  curve$spot_date_orig <- curve$spot_date
  curve$spot_date <- curve$today_date
  curve$times <- newtimes
  curve$interpolate$dim_t <- length(curve$times)
  curve$interpolate$dim_d <- length(curve$times)
  curve$interpolate$coeff <- array(NA, c(curve$interpolate$dim_c,
                                         curve$interpolate$dim_t))
  df_on = 1 / (depo_sn$quote * yearfractions[1] + 1)
  df_tn = 1 / (depo_sn$quote * yearfractions[2] + 1)
  update_discount_factors(curve, c(1, df_on, df_on * df_tn * olddf))
}

rh_get_discount_curve <- function(curvename) {
  curve <- dynGet(curvename, ifnotfound = curvename)
  if (is.character(curve))
    curve <- get(curve, envir = globalenv())
  curve
}

#' @export
set_bootstrap_options.rh_interp_linear <- function(curve) {
  curve$interpolate <- list(dim_c = 2)
  curve$bootstrap <- list(max_iter = 1)
  curve
}

#' @export
set_bootstrap_options.rh_interp_spline <- function(curve) {
  spline_max_sweeps <- get_rh_param('spline_max_sweeps')
  curve$interpolate <- list(dim_c = 4)
  curve$bootstrap <- list(max_iter = spline_max_sweeps)
  curve
}

#' @export
set_bootstrap_options.rh_interp_splinecorr <- function(curve) {
  spline_max_sweeps <- get_rh_param('spline_max_sweeps')
  curve$interpolate <- list(dim_c = 4)
  curve$bootstrap <- list(max_iter = spline_max_sweeps)
  curve
}

#' @export
set_interpolate.rh_curve <- function(curve) {
  curve$interpolate$dim_t <- length(curve$times)
  curve$interpolate$dim_d <- 0
  curve$interpolate$coeff <- array(NA, c(curve$interpolate$dim_c,
                                         curve$interpolate$dim_t))
  curve$bootstrap$func_evals <- 0
  update_discount_factors(curve, 1, pillar = 1)
}

#' @export
set_spot_date.rh_curve <- function(curve, settings_spot_date) {
  # calendar
  if (curve$ccy == "EUR")
    cal <- "target"
  else if (curve$ccy == "GBP")
    cal <- "uk"
  else
    stop("unknown currency in set_spot_date")
  curve$calendar <- cal

  # spot date 
  if (!is.null(settings_spot_date))
    spot_date <- as.Date(settings_spot_date)
  else {
    spot_date <- curve$today_date
    if (curve$ccy == "EUR") {
      spot_date <- calc_next_date.default(curve, spot_date, 1, "DY")
      spot_date <- calc_next_date.default(curve, spot_date, 1, "DY")
    }
  }
  curve$spot_date <- spot_date

  # return
  curve
}

#' @export
set_times.rh_curve <- function(curve, inst_list) {
  curve$times <-
    as.vector(c(0, sapply(lapply(inst_list, "[[", 'mat_date'),
                          function(x) daycount(curve, curve$spot_date, x))))
  curve
}

try_pillar.rh_curve <- function(curve, instrument, df, pillar) {
  curve <- update_discount_factors(curve, df, pillar)
  instrument$quote - fixing(instrument, curve)
}

#' @export
update_discount_factors.rh_interp_df <- function(curve, df, pillar = NULL) {
  if (is.null(pillar)) {
    curve$interpolate$coeff[1,] <- df
  } else {
    if (curve$interpolate$dim_d < pillar) {
      curve$interpolate$dim_d <- pillar
    }
    curve$interpolate$coeff[1, pillar] <- df
  }
  calc_interp_coeff(curve, updated = pillar)
}

#' @export
update_discount_factors.rh_interp_logdf <- function(curve, df, pillar = NULL) {
  if (is.null(pillar)) {
    curve$interpolate$coeff[1,] <- log(df)
  } else {
    if (curve$interpolate$dim_d < pillar) {
      curve$interpolate$dim_d <- pillar
    }
    curve$interpolate$coeff[1, pillar] <- log(df)
  }
  calc_interp_coeff(curve, updated = pillar)
}

#' @export
update_discount_factors.rh_interp_zero <- function(curve, df, pillar = NULL) {
  if (is.null(pillar)) {
    curve$interpolate$coeff[1,] <- c(0, -log(df[-1]) / curve$times[-1])
  } else {
    if (curve$interpolate$dim_d < pillar) {
      curve$interpolate$dim_d <- pillar
    }
    if (pillar == 1)
      curve$interpolate$coeff[1, pillar] <- 0
    else
      curve$interpolate$coeff[1, pillar] <- -log(df) / curve$times[pillar]
  }
  calc_interp_coeff(curve, updated = pillar)
}

