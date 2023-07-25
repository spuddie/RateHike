#' @export
calc_interp_coeff.rh_interp_linear <- function(curve, updated = NULL) {
  if (is.null(updated))
    upd_ind <- seq(curve$interpolate$dim_d)
  else if (updated == 1)
    upd_ind <- 1
  else
    upd_ind <- c(max(1, updated - 1), min(updated, curve$interpolate$dim_d))
  if (tail(upd_ind, 1) == curve$interpolate$dim_d) {
    curve$interpolate$coeff[2, curve$interpolate$dim_d] <- 0
    upd_ind <- head(upd_ind,-1)
  }
  curve$interpolate$coeff[2, upd_ind] <-
    (curve$interpolate$coeff[1, upd_ind + 1] -
       curve$interpolate$coeff[1, upd_ind]) /
    (curve$times[upd_ind + 1] - curve$times[upd_ind])
  curve
}

#' @export
calc_interp_coeff.rh_interp_spline <- function(curve, updated = NULL) {
  rh_calc_interp_coeff_spline(curve, 
                              updated = updated, 
                              lin_corr = FALSE) 
}

#' @export
calc_interp_coeff.rh_interp_splinecorr <- function(curve, updated = NULL) {
  rh_calc_interp_coeff_spline(curve, 
                              updated = updated, 
                              lin_corr = TRUE) 
}

#' @export
calc_spline_coeff_b.rh_interp_bessel <- function(curve, updated = NULL) {
  if (is.null(updated))
    upd_ind <- seq(curve$interpolate$dim_d)
  else {
    if (updated <= 3)
      upd_start <- 1
    else
      upd_start <- max(updated - 1, 1)
    if (updated >= curve$interpolate$dim_d - 2)
      upd_end <- curve$interpolate$dim_d
    else
      upd_end <- min(updated + 1, curve$interpolate$dim_d)
    upd_ind <- seq(upd_start, upd_end)
  }
  if (upd_ind[1] == 1) {
    t <- curve$times[1:3]
    a <- curve$interpolate$coeff[1, 1:3]
    curve$interpolate$coeff[2, 1] <-
      ((t[3] + t[2] - 2 * t[1]) * (a[2] - a[1]) / (t[2] - t[1]) -
         (t[2] - t[1]) * (a[3] - a[2]) / (t[3] - t[2])) / 
      (t[3] - t[1])
    upd_ind <- upd_ind[-1]
  }
  if (tail(upd_ind, 1) == curve$interpolate$dim_d) {
    t <- tail(curve$times, 3)
    a <- tail(curve$interpolate$coeff[1, 1:curve$interpolate$dim_d], 3)
    curve$interpolate$coeff[2, curve$interpolate$dim_d] <-
      ((t[3] - t[2]) * (a[2] - a[1]) / (t[2] - t[1]) -
         (2 * t[3] - t[2] - t[1]) * (a[3] - a[2]) / (t[3] - t[2])) /
      (t[3] - t[1])
    upd_ind <- head(upd_ind,-1)
  }
  ap <- curve$interpolate$coeff[1, upd_ind + 1]
  a <- curve$interpolate$coeff[1, upd_ind]
  am <- curve$interpolate$coeff[1, upd_ind - 1]
  tp <- curve$times[upd_ind + 1]
  t <- curve$times[upd_ind]
  tm <- curve$times[upd_ind - 1]
  curve$interpolate$coeff[2, upd_ind] <-
    ((tp - t) * (a - am) / (t - tm) + (t - tm) * (ap - a) / (tp - t)) /
    (tp - tm)
  curve
}

#' @export
calc_spline_coeff_b.rh_interp_hyman <- function(curve, updated = NULL) {
  rh_calc_hyman_coeff_b(curve, updated = updated, endpoints = "hyman")
}

#' @export
calc_spline_coeff_b.rh_interp_hyman0 <- function(curve, updated = NULL) {
  rh_calc_hyman_coeff_b(curve, updated = updated, endpoints = "hw")
}

#' @export
get_curve_data.rh_curve <- function(curve, 
                                    dates, 
                                    times, 
                                    pillars, 
                                    deriv = FALSE) {
  coeffs <- curve$interpolate$coeff
  if (deriv) {
    coeffs <- array(coeffs[-1, ], dim(coeffs)- c(1, 0))
    coeffs <- (1:(curve$interpolate$dim_c - 1)) * coeffs
  }
  if (is.null(pillars) && is.null(dates) && is.null(times)) {
    data <- coeffs[1,]
    times <- curve$times
  } else if (!is.null(pillars)) {
    data <- coeffs[1, pillars]
    times <- curve$times[pillars]
  } else {
    if (!is.null(dates))
      times <- daycount(curve, curve$spot_date, dates)
    data <- rep(NA, length(times))
    times_in <- curve$times[1:curve$interpolate$dim_d]
    for (k in seq_along(data)) {
      i <- match(TRUE, times[k] < times_in)
      if (is.na(i))
        data[k] <- coeffs[1, curve$interpolate$dim_d]
      else if (i == 1)
        data[k] <- coeffs[1, 1]
      else {
        coeff <- coeffs[, i - 1]
        coeff[is.na(coeff)] <- 0
        data[k] <- tail(coeff, 1)
        if (length(coeff) > 1) {
          for (j in seq(length(coeff) - 1, 1, -1))
            data[k] <- data[k] * (times[k] - times_in[i - 1]) + coeff[j]
        }
      }
    }
  }
  list(data = data, times = times)
}

rh_calc_hyman_coeff_b <- function(curve, updated, endpoints) {
  if (is.null(updated))
    upd_ind <- seq(1, curve$interpolate$dim_d)
  else {
    if (updated <= 3)
      upd_start <- 1
    else
      upd_start <- max(updated - 1, 1)
    if (updated >= curve$interpolate$dim_d - 2)
      upd_end <- curve$interpolate$dim_d
    else
      upd_end <- min(updated + 1, curve$interpolate$dim_d)
    upd_ind <- seq(upd_start, upd_end)
  }
  if (upd_ind[1] == 1) {
    if (endpoints == "hw")
      curve$interpolate$coeff[2, 1] <- 0
    else {
      h1 <- curve$times[2] - curve$times[1]
      h2 <- curve$times[3] - curve$times[2]
      m1 <-
        (curve$interpolate$coeff[1, 2] - curve$interpolate$coeff[1, 1]) / h1
      m2 <-
        (curve$interpolate$coeff[1, 3] - curve$interpolate$coeff[1, 2]) / h2
      curve$interpolate$coeff[2, 1] <-
        ((2 * h1 + h2) * m1 - h1 * m2) / (h1 + h2)
    }
    upd_ind <- upd_ind[-1]
  }
  if (tail(upd_ind, 1) == curve$interpolate$dim_d) {
    if (endpoints == "hw")
      curve$interpolate$coeff[2, curve$interpolate$dim_d] <- 0
    else {
      n <- curve$interpolate$dim_d
      h1 <- curve$times[n] - curve$times[n - 1]
      h2 <- curve$times[n - 1] - curve$times[n - 2]
      m1 <- (curve$interpolate$coeff[1, n] -
               curve$interpolate$coeff[1, n - 1]) / h1
      m2 <- (curve$interpolate$coeff[1, n - 1] -
               curve$interpolate$coeff[1, n - 2]) / h2
      curve$interpolate$coeff[2, n] <-
        ((2 * h1 + h2) * m1 - h1 * m2) / (h1 + h2)
    }
    upd_ind <- head(upd_ind,-1)
  }
  h <- curve$times[upd_ind + 1] - curve$times[upd_ind]
  hm <- curve$times[upd_ind] - curve$times[upd_ind - 1]
  m <- (curve$interpolate$coeff[1, upd_ind + 1] -
          curve$interpolate$coeff[1, upd_ind]) / h
  mm <- (curve$interpolate$coeff[1, upd_ind] -
           curve$interpolate$coeff[1, upd_ind - 1]) / hm
  b <- 3 * mm * m / (mm + m + pmin(mm, m))
  b[m > 0] <- pmin(pmax(0, b[m > 0]), 3 * mm[m > 0], 3 * m[m > 0])
  b[m < 0] <- pmax(pmin(0, b[m < 0]), 3 * mm[m < 0], 3 * m[m < 0])
  b[m * mm < 0] <- 0
  curve$interpolate$coeff[2, upd_ind] <- b
  curve
}

rh_calc_interp_coeff_spline <- function(curve, updated, lin_corr) {
  if (curve$interpolate$dim_d < 3)
    curve <- calc_interp_coeff.rh_interp_linear(curve, updated = updated)
  else {
    curve <- calc_spline_coeff_b(curve, updated)
    if (is.null(updated))
      upd_ind <- seq(curve$interpolate$dim_d)
    else
      upd_ind <- seq(max(1, updated - 2),
                     min(curve$interpolate$dim_d, updated + 1))
    coeff <- curve$interpolate$coef
    if (tail(upd_ind, 1) == curve$interpolate$dim_d) {
      coeff[3:4, curve$interpolate$dim_d] <- 0
      upd_ind <- head(upd_ind, -1)
      corr_last = TRUE
    } else
      corr_last = FALSE
    denom <- curve$times[upd_ind + 1] - curve$times[upd_ind]
    m <- (coeff[1, upd_ind + 1] - coeff[1, upd_ind]) / denom
    coeff[3, upd_ind] <-
      (3 * m - coeff[2, upd_ind + 1] - 2 * coeff[2, upd_ind]) / denom
    coeff[4, upd_ind] <-
      (-2 * m + coeff[2, upd_ind + 1] + coeff[2, upd_ind]) / denom^2
    curve$interpolate$coeff <- coeff
    # Linear correction for last interval
    if (lin_corr && corr_last) {
      curve <- 
        calc_interp_coeff.rh_interp_linear(curve, 
                                           updated = curve$interpolate$dim_d) 
      curve$interpolate$coeff[3:4, curve$interpolate$dim_d - 1] <- 0
    }
  }
  curve
}
