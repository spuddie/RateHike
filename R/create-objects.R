#' creates a curve based on the settings
#'
#' @param settings a structure with the following fields:
#' \describe{
#'    \item{interp_how}{interpolation method (linear, bessel, hyman, hyman0)}
#'    \item{interp_what}{interpolation object(zero, df, logdf)}
#'    \item{spline_corr}{apply spline correction (yes, no); can be missing}
#'    \item{daycount}{a daycount convention: act365, act360, 30e360}
#'    \item{today_date}{today's date yyyy-mm-dd}
#'    \item{anchor}{re-anchoring method (no, date, ontn, 11); can be missing}
#'    \item{snt}{which synthetic instruments to add (no, ois); can be missing}
#'    \item{discount_curve}{name of an exogeneous discount curve, if required}
#'    \item{ccy}{the currency, EUR or GBP are currently supported}
#'    \item{spot_date}{the curve spot date yyyy-mm-dd, can be missing}
#' }
#'
#' @return an object of class \code{\link{rh_curve}}.
#' @export
#'
#' @examples
#'   settings <- list(today_date = "2022-02-22",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    ccy = "EUR")
#'   curve <- rh_new_curve(settings)
rh_new_curve <- function(settings) {
  if (settings$interp_how == "linear")
    interp_class <-
      c(settings$interp_what, settings$interp_how)
  else if (!is.null(settings$spline_corr) && settings$spline_corr == "yes")
    interp_class <-
      c(settings$interp_what, "splinecorr", settings$interp_how)
  else
    interp_class <-
      c(settings$interp_what, "spline", settings$interp_how)
  curve_class <- c("curve",
                   settings$daycount,
                   paste0("interp_", interp_class),
                   paste0("anchor_", settings$anchor))
  curve_list <- list(
    today_date = as.Date(settings$today_date),
    interp_class = interp_class,
    anchor = settings$anchor,
    synth_inst = settings$snt,
    discount_curve = settings$discount_curve,
    ccy = settings$ccy
  )
  curve <- structure(curve_list, class = c(paste0("rh_", curve_class)))
  set_spot_date(curve, settings$spot_date)
}

#' creates a financial instrument
#'
#' @param inst_df structure with the following fields:
#' \describe{
#'   \item{quote}{market quote}
#'   \item{mat_no}{numeric component of maturity}
#'   \item{mat_unit}{unit for maturity (YR, MO, WK, DY)}
#'   \item{insttype}{instrument type (deposit, ois, fra, future, irs)}
#'   \item{start_no}{numeric component of start delay, for forward starting instruments}
#'   \item{start_unit}{unit for start delay (YR, MO, WK, DY), for forward starting instruments}
#'   \item{tenor_m}{tenor, in months, for irs}
#' }
#' @param settings structure with the following fields:
#' \describe{
#'   \item{today_date}{today's date yyyy-mm-dd}
#'   \item{ccy}{the currency (currently only EUR and GBP are supported}
#'   \item{fut_ca_mrs}{optional; the mean reversion speed for the futures convexity adjustment}
#'   \item{fut_ca_vol}{optional; the volatility for the futures convexity adjustment}
#' }
#'
#' @return an object of class \code{\link{rh_instrument}}
#' @export
#'
#' @examples
#'   settings <- list(today_date = "2022-02-22",
#'                    ccy = "EUR")
#'   inst_df <- list(quote = 0.01,
#'                   mat_no = 1,
#'                   mat_unit = "WK",
#'                   insttype = "deposit")
#'   depo1w <- rh_new_instrument(inst_df, settings)
rh_new_instrument <- function(inst_df, settings) {
  inst_list <- list(
    quote = inst_df$quote,
    mat_no = inst_df$mat_no,
    mat_unit = inst_df$mat_unit,
    today_date = as.Date(settings$today_date),
    ccy = settings$ccy
  )
  inst_class <- c("instrument", inst_df$insttype)
  instrument <-
    structure(inst_list, class = paste0("rh_", inst_class))
  instrument <- set_conventions(instrument)
  instrument <-
    set_mat_date(instrument, inst_df$start_no, inst_df$start_unit)
  instrument <-
    set_schedule(instrument, inst_df$tenor_m)
  #set_convexity(instrument, settings$fut_ca_mrs, settings$fut_ca_vol)
}

#' creates an instrument list
#'
#' @param inst_df a data frame with the following fields:
#' \describe{
#'   \item{quote}{market quote}
#'   \item{mat_no}{numeric component of maturity}
#'   \item{mat_unit}{unit for maturity (YR, MO, WK, DY)}
#'   \item{insttype}{instrument type (deposit, ois, fra, future, irs)}
#'   \item{start_no}{numeric component of start delay, for forward starting instruments}
#'   \item{start_unit}{unit for start delay (YR, MO, WK, DY), for forward starting instruments}
#'   \item{tenor_m}{tenor, in months, for irs}
#' }
#' @param settings structure with the following fields:
#' \describe{
#'   \item{today_date}{today's date yyyy-mm-dd}
#'   \item{ccy}{the currency (currently only EUR and GBP are supported}
#'   \item{snt}{which synthetic instruments should be added? (no, ois)}
#'   \item{fut_ca_mrs}{optional; the mean reversion speed for the futures convexity adjustment}
#'   \item{fut_ca_vol}{optional; the volatility for the futures convexity adjustment}
#' }
#'
#' @return an object of class \code{\link{rh_instrument_list}}
#' @export
#'
#' @examples 
#'   settings <- list(today_date = "2022-11-30",
#'                    ccy = "EUR",
#'                    snt = "no")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
rh_new_instrument_list <- function(inst_df, settings) {
  structure(by(inst_df, seq(nrow(inst_df)), rh_new_instrument,
               settings, simplify = TRUE),
            class = c("rh_instrument_list", paste0("rh_snt_", settings$snt)))
}
