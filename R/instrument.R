#' @export
calc_schedule.rh_sched_tenor <- function(instrument, adjust = "all") {
  if (instrument$mat_unit == "MO")
    mat_m <- instrument$mat_no
  else if (instrument$mat_unit == "YR")
    mat_m <- instrument$mat_no * 12
  else
    stop("unknown mat_unit in calc_schedule.rh_sched_tenor")
  calc_next_date(instrument, 
                 instrument$spot_date, 
                 instrument$tenor_m, 
                 "MO", 
                 length = mat_m / instrument$tenor_m, 
                 adjust = adjust)
}

#' @export
calc_schedule.rh_sched_yearly <- function(instrument, adjust = "all") {
  if ((instrument$mat_unit == "YR" && instrument$mat_no == 1) ||
      (instrument$mat_unit == "MO" && instrument$mat_no <= 12) ||
      (instrument$mat_unit == "WK") ||
      (instrument$mat_unit == "DY"))
    result <- instrument$mat_date
  else if (instrument$mat_unit == "YR")
    result <- calc_next_date(instrument, 
                             instrument$spot_date, 
                             1, 
                             instrument$mat_unit, 
                             length = instrument$mat_no, 
                             adjust = adjust)
  else  if (instrument$mat_unit == "MO")
    result <- c(calc_next_date(instrument, 
                               instrument$spot_date, 
                               instrument$mat_no - 12, 
                               instrument$mat_unit, 
                               adjust = adjust), 
                instrument$mat_date)
  result
}

#' @export
fixing.rh_deposit <- function(instrument, curve) {
  dfs <- get_discount_factors(curve, 
                              dates = c(instrument$spot_date, 
                                        instrument$mat_date))
  t <- daycount(instrument, instrument$spot_date, instrument$mat_date)
  (dfs[1] / dfs[2] - 1) / t
}

#' @export
fixing.rh_fra <- function(instrument, curve) {
  dfs <- get_discount_factors(curve, 
                              dates = c(instrument$start_date, 
                                        instrument$mat_date))
  t <- daycount(instrument, instrument$start_date, instrument$mat_date)
  (dfs[1] / dfs[2] - 1) / t
}

#' @export
fixing.rh_future <- function(instrument, curve) {
  dfs <- get_discount_factors(curve, 
                              dates = c(instrument$start_date, 
                                        instrument$mat_date))
  t <- daycount(instrument, instrument$start_date, instrument$mat_date)
  (dfs[1] / dfs[2] - 1) / t
}

#' @export
fixing.rh_irs <- function(instrument, curve) {
  dfs_forecast <- get_discount_factors(curve,
                                       dates = c(instrument$spot_date,
                                                 instrument$float_schedule))
  if (!is.null(curve$discount_curve))
    curve <- rh_get_discount_curve(curve$discount_curve)
  dfs_fix <- get_discount_factors(curve,
                                  dates = instrument$fix_schedule)
  dfs_float <- get_discount_factors(curve,
                                    dates = instrument$float_schedule)
  accrual_fix <- 
    diff(daycount(instrument, 
                  instrument$spot_date, 
                  c(instrument$spot_date, instrument$fix_schedule)))
  Ac <- dfs_fix %*% accrual_fix
  (head(dfs_forecast,-1) / dfs_forecast[-1] - 1) %*% dfs_float / Ac
}

#' @export
fixing.rh_ois <- function(instrument, curve) {
  dfs <- get_discount_factors(curve,
                              dates = c(instrument$spot_date, 
                                        instrument$payment_schedule))
  yf <- daycount(instrument, 
                 instrument$spot_date, 
                 c(instrument$spot_date, instrument$payment_schedule))
  Ac <- dfs[-1] %*% diff(yf)
  (dfs[1] - tail(dfs, 1)) / Ac
}

#' @export
insttype.rh_synthetic <- function(instrument) {
  "synthetic"
}

#' @export
insttype.rh_deposit <- function(instrument) {
  "deposit"
}

#' @export
insttype.rh_fra <- function(instrument) {
  "fra"
}

#' @export
insttype.rh_future <- function(instrument) {
  "future"
}

#' @export
insttype.rh_irs <- function(instrument) {
  "irs"
}

#' @export
insttype.rh_ois <- function(instrument) {
  "ois"
}

#' @export
set_conventions.rh_instrument <- function(instrument) {
  inst_class <- class(instrument)
  
  # calendar & daycount convention
  if (instrument$ccy == "EUR") {
    cal <- "target"
    if (insttype(instrument) == "irs")
      dcc <- "rh_30e360"
    else
      dcc <- "rh_act360"
  } else if (instrument$ccy == "GBP") {
    cal <- "uk"
    dcc <- "rh_act365"
  } else
    stop("unknown currency in set_conventions")
  instrument$calendar <- cal
  inst_class <- c(inst_class, dcc)

  # spot date
  spot_date <- instrument$today_date
  if (instrument$ccy == "EUR") {
    spot_date <- calc_next_date.default(instrument, spot_date, 1, "DY")
    spot_date <- calc_next_date.default(instrument, spot_date, 1, "DY")
  }
  instrument$spot_date <- spot_date

  # rolling convention
  if (instrument$mat_unit == "DY" || instrument$mat_unit == "WK")
    rc <- "rh_following"
  else
    rc <- "rh_modfollowing"
  inst_class <- c(inst_class, rc)
  
  # end of month convention
  if (insttype(instrument) != "future" &&
      rh_month(instrument$spot_date) !=
      rh_month(busday_adjust.rh_following(NULL, instrument$spot_date + 1))
      &&
      (instrument$mat_unit == "YR" || instrument$mat_unit == "MO"))
    inst_class <- c(inst_class, "rh_end_of_month")
  
  # return
  class(instrument) <- inst_class
  instrument
}

#' @export
set_convexity.default <- function(instrument, mrs, vol) {
  instrument
}

#' @export
set_convexity.rh_future <- function(instrument, mrs, vol) {
  instrument$convexity_adj <- list(mean_rev_speed = mrs,
                                   volatility = vol)
  instrument
}

#' @export
set_mat_date.default <- function(instrument, start_no, start_unit) {
  instrument$mat_date <- calc_next_date(instrument, 
                                        instrument$spot_date, 
                                        instrument$mat_no, 
                                        instrument$mat_unit)
  instrument
}

#' @export
set_mat_date.rh_fra <- function(instrument, start_no, start_unit) {
  instrument$start_no <- start_no
  instrument$start_unit <- start_unit
  instrument$start_date <- calc_next_date(instrument, 
                                          instrument$spot_date, 
                                          start_no, 
                                          start_unit)
  instrument$mat_date <- calc_next_date(instrument, 
                                        instrument$spot_date, 
                                        instrument$mat_no + start_no, 
                                        instrument$mat_unit)
  instrument
}

#' @export
set_mat_date.rh_future <- function(instrument, start_no, start_unit) {
  instrument$start_no <- start_no
  instrument$start_unit <- start_unit
  instrument$start_date <- calc_next_date(instrument, 
                                          instrument$today_date, 
                                          start_no, 
                                          start_unit)
  instrument$mat_date <- calc_next_date(instrument, 
                                        instrument$today_date, 
                                        instrument$mat_no + start_no, 
                                        instrument$mat_unit)
  instrument
}

#' @export
set_schedule.default <- function(instrument, tenor_m) {
  instrument
}

#' @export
set_schedule.rh_irs <- function(instrument, tenor_m) {
  if (instrument$ccy == "EUR")
    sched <- "rh_sched_yearly"
  else if (instrument$ccy == "GBP")
    sched <- "rh_sched_tenor"
  else
    stop("unknown currency in set_schedule")
  class(instrument) <- c(class(instrument), sched)
  instrument$tenor_m <- tenor_m
  instrument$fix_schedule <- calc_schedule(instrument)
  instrument$float_schedule <-
    calc_schedule.rh_sched_tenor(instrument)
  instrument
}

#' @export
set_schedule.rh_ois <- function(instrument, tenor_m) {
  class(instrument) <- c(class(instrument), "rh_sched_yearly")
  instrument$payment_schedule <- calc_schedule(instrument)
  instrument
}
