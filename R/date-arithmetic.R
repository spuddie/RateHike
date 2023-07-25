#' @export
busday_adjust.default <- function(object, datum) {
  result <- busday_adjust.rh_following(object, datum)
}

#' @export
busday_adjust.rh_following <- function(object, datum) {
  weekday <- rh_weekday(datum)
  if (weekday < 6)
    result <- datum
  else
    result <- datum + 8 - weekday
  hol_offset <-
    rh_hol_offset(object$calendar, as.character(result))
  if (!is.null(hol_offset))
    result <- result + hol_offset
  result
}

#' @export
busday_adjust.rh_modfollowing <- function(object, datum) {
  result <- busday_adjust.rh_following(object, datum)
  if (rh_month(result) != rh_month(datum)) {
    weekday <- rh_weekday(datum)
    result <- datum + 5 - weekday
    hol_offset <-
      rh_hol_offset(object$calendar, as.character(result))
    while (!is.null(hol_offset)) {
      result <- result - 1
      hol_offset <-
        rh_hol_offset(object$calendar, as.character(result))
    }
  }
  result
}

#' @export
calc_next_date.default <- function(object,
                                   datum,
                                   n,
                                   period,
                                   length = 1,
                                   adjust = "all") {
  periods <- list( "DY" = "day", "WK" = "week", "MO" = "month", "YR" = "year")
  result <-
    seq(datum, by = paste(n, periods[[period]]), length = length + 1)
  if (adjust == "all")
    adjust <- sequence(length, 2)
  else if (adjust == "none")
    adjust <- sequence(0)
  for (i in adjust)
    result[i] <- busday_adjust(object, result[i])
  result[-1]
}

#' @export
calc_next_date.rh_end_of_month <- function(object,
                                           datum,
                                           n,
                                           period,
                                           length = 1,
                                           adjust = "all") {
  periods <- list("MO" = 1, "YR" = 12)
  months <- seq(rh_month(datum),
                by = n * periods[[period]],
                length = length + 1) + 1
  years <- rh_year(datum) + (months - 1) %/% 12
  months <- (months - 1) %% 12 + 1
  result <- as.Date(sprintf("%d-%02d-01", years, months)) - 1
  if (adjust == "all")
    adjust <- sequence(length, 2)
  else if (adjust == "none")
    adjust <- sequence(0)
  for (i in adjust)
    result[i] <- busday_adjust(object, result[i])
  result[-1]
}

#' @export
calc_next_date.rh_future <- function(object,
                                     datum,
                                     n,
                                     period,
                                     length = 1,
                                     adjust = "all") {
  periods <- list("MO" = 1, "YR" = 12)
  months <- seq(rh_month(datum),
                by = n * periods[[period]],
                length = length + 1)
  years <- rh_year(datum) + (months - 1) %/% 12
  months <- (months - 1) %% 12 + 1
  result <- as.Date(sprintf("%d-%02d-21", years, months))
  corr <- (rh_weekday(result) - 3) %% 7
  result <- result - corr
  if (adjust == "all")
    adjust <- sequence(length, 2)
  else if (adjust == "none")
    adjust <- sequence(0)
  for (i in adjust)
    result[i] <- busday_adjust(object, result[i])
  result[-1]
}

#' apply ACT/365 convention to calculate year fraction
#'
#' @inheritParams daycount
#' @details
#'    the actual number of days in the period, divided by 365
#'
#' @export
#' @family daycount
daycount.rh_act365 <- function(object, fromdate, todate) {
  (as.numeric(todate) - as.numeric(fromdate)) / 365
}

#' apply ACT/360 convention to calculate year fraction
#'
#' @inheritParams daycount
#' @details
#'    the actual number of days in the period, divided by 360
#'
#' @export
#' @family daycount
daycount.rh_act360 <- function(object, fromdate, todate) {
  (as.numeric(todate) - as.numeric(fromdate)) / 360
}

#' apply 30E/360 convention to calculate year fraction
#'
#' @inheritParams daycount
#' @details
#'    year(todate) - year(fromdate)
#'    + 1/12 (month(todate) - month(fromdate))
#'    + 1/360 (day(todate) - day(fromdate)) \cr
#'    when the day = 31, it is corrected to 30.
#'
#' @export
#' @family daycount
daycount.rh_30e360 <- function(object, fromdate, todate) {
  yf <- rh_year(fromdate)
  mf <- rh_month(fromdate)
  df <- rh_dayofmonth(fromdate)
  yt <- rh_year(todate)
  mt <- rh_month(todate)
  dt <- rh_dayofmonth(todate)
  if (df == 31)
    df <- 30
  dt[dt == 31] <- 30
  yt - yf + (mt - mf) / 12 + (dt - df) / 360
}

#' @export
get_dates.rh_act365 <- function(curve, times = NULL) {
  if (is.null(times))
    times <- curve$times
  curve$spot_date + round(times * 365)
}

rh_dayofmonth <- function(datum) {
  as.numeric(format(datum, "%d"))
}

rh_hol_offset <- function(calendar, datestr) {
  if (is.null(calendar))
    hol_offset <- NULL
  else
    hol_offset <- holiday_offset[[calendar]][[datestr]]
  hol_offset
}

rh_month <- function(datum) {
  as.numeric(format(datum, "%m"))
}

rh_weekday <- function(datum) {
  as.numeric(format(datum, "%u"))
}

rh_year <- function(datum) {
  as.numeric(format(datum, "%Y"))
}
