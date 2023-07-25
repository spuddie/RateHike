#' Add synthetic instruments to instrument list
#'
#' @param inst_list_raw an object of type \code{\link{rh_instrument_list}}
#' @param settings see \code{\link{rh_new_curve}}
#'
#' @return an object of type \code{\link{rh_instrument_list}}
#' @export
#'
#' @details
#'   the action performed is based on the class of inst_list_raw:
#'   rh_snt_ois. When this class is not assigned to inst_list_raw, 
#'   no synthetic instruments are added.
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "ois",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'
#'   settings$discount_curve <- "curve_ois"
#'   inst_list_ibor_wo <- rh_new_instrument_list(inst_df_ibor, settings)
#'   inst_list_ibor_snt <- add_synthetics(inst_list_ibor_wo, settings)
add_synthetics <- function(inst_list_raw, settings) {
  UseMethod("add_synthetics")
}

#' Benchmark a curve to a reference
#'
#' @param curve a bootstrapped curve (created by \code{\link{bootstrap}})
#' @param bench_zeros a vector of benchmarking zero rates (can be NULL)
#' @param bench_dfs a vector of benchmarking discount factors (can be NULL)
#' @param pillars the indices for which the benchmarks apply.
#'
#' @return an object with fields zeros and dfs, containing the differences for the zero rates and the discount factors respectively
#' @export
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "no",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'   benchmark(curve_ois, 
#'             bench_zeros = c(0, 0, 0), 
#'             bench_dfs = c(1, 1, 1),
#'             pillars = c(2, 5, 8))
benchmark <- function(curve,
                      bench_zeros = NULL,
                      bench_dfs = NULL,
                      pillars = -1) {
  UseMethod("benchmark")
}

#' Bootstrap a yield curve
#'
#' @param curve an object of class \code{\link{rh_curve}}, created by \code{\link{rh_new_curve}}
#' @param inst_list an object of class \code{\link{rh_instrument_list}}, created by \code{\link{rh_new_instrument_list}}
#'
#' @return a bootstrapped curve (class \code{\link{rh_curve}})
#' @export
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "no",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'
#'   settings$discount_curve <- "curve_ois"
#'   inst_list_ibor <- rh_new_instrument_list(inst_df_ibor, settings)
#'   curve_ibor <- bootstrap(rh_new_curve(settings), inst_list_ibor)
bootstrap <- function(curve, inst_list) {
  UseMethod("bootstrap")
}

busday_adjust <- function(object, datum) {
  UseMethod("busday_adjust")
}

#' Calculate the convexity adjustment for futures
#'
#' @param instrument an object of classes \code{\link{rh_instrument}} and `rh_future`; with additionally the fields `price` and `convexity_adj` set:
#' \describe{
#'   \item{price}{the quoted futures price}
#'   \item{convexity_adj}{a structure with info about the convexity adjustment:
#'     \describe{
#'       \item{mean_rev_speed}{the mean reversion speed}
#'       \item{volatility}{the volatility}
#'     }}
#' }
#'
#' @return The equivalent FRA rate of the future
#' @export
#' @family convexity adjustment
#'
#' @details
#'   the method followed is that of Kirikos & Novak: Convexity Conundrums
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    ccy = "EUR")
#'   inst_df <- list(quote = 0.03,
#'                   start_no = 6,
#'                   start_unit = "MO",
#'                   mat_no = 3,
#'                   mat_unit = "MO",
#'                   insttype = "future")
#'   future <- rh_new_instrument(inst_df, settings)
#'   future$price <- 96.9448
#'   future$convexity_adj <- list(mean_rev_speed = 0.025,
#'                                volatility = 0.05)
#'   calc_convexity_adjustment(future)
calc_convexity_adjustment <- function(instrument) {
  UseMethod("calc_convexity_adjustment")
}

#' Calculate the inverse convexity adjustment for futures
#'
#' @param instrument an object of classes \code{\link{rh_instrument}} and `rh_future`; with additionally the field `convexity_adj` set:
#' \describe{
#'   \item{convexity_adj}{a structure with info about the convexity adjustment:
#'     \describe{
#'       \item{mean_rev_speed}{the mean reversion speed}
#'       \item{volatility}{the volatility}
#'     }}
#' }
#'
#' @return The quoted price of the future
#' @export
#' @family convexity adjustment
#'
#' @details
#'   the method followed is that of Kirikos & Novak: Convexity Conundrums
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    ccy = "EUR")
#'   inst_df <- list(quote = 0.03,
#'                   start_no = 6,
#'                   start_unit = "MO",
#'                   mat_no = 3,
#'                   mat_unit = "MO",
#'                   insttype = "future")
#'   future <- rh_new_instrument(inst_df, settings)
#'   future$convexity_adj <- list(mean_rev_speed = 0.025,
#'                                volatility = 0.05)
#'   calc_inv_convexity_adjustment(future)
calc_inv_convexity_adjustment <- function(instrument) {
  UseMethod("calc_inv_convexity_adjustment")
}

calc_next_date <- function(object,
                           datum,
                           n,
                           period,
                           length = 1,
                           adjust = "all") {
  UseMethod("calc_next_date")
}

calc_schedule <- function(instrument, adjust="all") {
  UseMethod("calc_schedule")
}

calc_interp_coeff <- function(curve, updated = NULL) {
  UseMethod("calc_interp_coeff")
}

calc_spline_coeff_b <- function(curve, updated = NULL) {
  UseMethod("calc_spline_coeff_b")
}

#' apply daycount convention to calculate year fraction
#'
#' @param object object with a class to determine which convention to apply, currently the classes "rh_act365", "rh_act360" and "rh_30e360" are implemented
#' @param fromdate start date of the period
#' @param todate end date of the period
#'
#' @return a number, the year fraction
#' @export
#' @family daycount
#'
#' @examples
#'   o_act365 <- structure(list(), class = "rh_act365")
#'   o_act360 <- structure(list(), class = "rh_act360")
#'   o_30e360 <- structure(list(), class = "rh_30e360")
#'   daycount(o_act365, as.Date("2022-02-22"), as.Date("2022-08-10"))
#'   daycount(o_act360, as.Date("2022-02-22"), as.Date("2022-08-10"))
#'   daycount(o_30e360, as.Date("2022-02-22"), as.Date("2022-08-10"))
daycount <- function(object, fromdate, todate) {
  UseMethod("daycount")
}

#' Fixing: fixes a quote for an instrument based on a curve
#'
#' @param instrument a structure of class "rh_instrument"
#' @param curve a structure of class "rh_curve"
#'
#' @return the interest rate quote of the instrument according to the curve
#' @export
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "no",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'   fixing(inst_list_ois[[5]], curve_ois)
#'
#'   inst_df <- list(quote = 0.01,
#'                   mat_no = 1,
#'                   mat_unit = "WK",
#'                   insttype = "deposit")
#'   depo1w <- rh_new_instrument(inst_df, settings)
#'   fixing(depo1w, curve_ois)
fixing <- function(instrument, curve) {
  UseMethod("fixing")
}

get_curve_data <- function(curve, dates, times, pillars, deriv = FALSE) {
  UseMethod("get_curve_data")
}

get_dates <- function(curve, times = NULL) {
  UseMethod("get_dates")
}

#' extract discount factors from a curve
#'
#' @param curve the curve from which to get the data
#' @param dates the dates for which the data is requested (can be missing)
#' @param times the times for which the data is requested (can be missing)
#' @param pillars the index of the pillars for wich the data is requested (can be missing)
#'
#' @return the discount factors as per the request
#' @export
#'
#' @details
#'   at most one of dates, times and pillars should be given \cr
#'   if dates, times and pillars are NULL, the function returns the discount
#'   factors of all pillars \cr
#'   if pillars is not NULL, the discount factors for these pillars are
#'   returned \cr
#'   if pillars is NULL and dates is not NULL, the discount factors for these
#'   dates are returned \cr
#'   if dates and pillars are NULL and times is not NULL, this is used to
#'   determine the discount factors
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "no",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'   get_discount_factors(curve_ois)
#'   get_discount_factors(curve_ois, times = c(0.5, 1, 2, 5))
get_discount_factors <- function(curve,
                                 dates = NULL,
                                 times = NULL,
                                 pillars = NULL) {
  UseMethod("get_discount_factors")
}

#' extract instanteneous forward rates from a curve
#'
#' @inheritParams get_discount_factors
#'
#' @return the instantaneous forward rates as per the request
#' @export
#'
#' @details
#'   at most one of dates, times and pillars should be given \cr
#'   if dates, times and pillars are NULL, the function returns the
#'   instantaneous forward rates for all pillars \cr
#'   if pillars is not NULL, the instantaneous forward rates for these
#'   pillars are returned \cr
#'   if pillars is NULL and dates is not NULL, the instantaneous forward rates
#'   for these dates are returned \cr
#'   if dates and pillars are NULL and times is not NULL, this is used to
#'   determine the instantaneous forward rates
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "no",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'   get_insta_fwd(curve_ois)
#'   get_insta_fwd(curve_ois, times = c(0.5, 1, 2, 5))
get_insta_fwd <- function(curve,
                          dates = NULL,
                          times = NULL,
                          pillars = NULL) {
  UseMethod("get_insta_fwd")
}

#' extract zero rates from a curve
#'
#' @inheritParams get_discount_factors
#'
#' @return the zero rates as per the request
#' @export
#'
#' @details
#'   at most one of dates, times and pillars should be given \cr
#'   if dates, times and pillars are NULL, the function returns the
#'   zero rates for all pillars \cr
#'   if pillars is not NULL, the zero rates for these
#'   pillars are returned \cr
#'   if pillars is NULL and dates is not NULL, the zero rates
#'   for these dates are returned \cr
#'   if dates and pillars are NULL and times is not NULL, this is used to
#'   determine the zero rates
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "no",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'   get_zero_rates(curve_ois)
#'   get_zero_rates(curve_ois, times = c(0.5, 1, 2, 5))
get_zero_rates <- function(curve,
                           dates = NULL,
                           times = NULL,
                           pillars = NULL) {
  UseMethod("get_zero_rates")
}

#' plot a yield curve
#'
#' @param curve an object of class \code{\link{rh_curve}}
#' @param t a vector of timepoints
#' @param max_t the maximum time value to be plotted
#' @param n_points the number of points to be plotted
#' @param fwd_period the discrete forward period to apply
#'
#' @return a plot of the yield curve, including the zero rates, the discount factors, the instantaneous forward rates and the discrete forward rates
#' @export
#'
#' @details
#'   if t is provided, that is used, else a t is constructed
#'   based on max_t and n_points. \cr
#'   the default value for max_t is the last point on the curve, the default value for n_points is 500.
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "no",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'   graph(curve_ois)
#'   graph(curve_ois, max_t = 2)
graph <- function(curve,
                  t = NULL,
                  max_t = NULL,
                  n_points = NULL,
                  fwd_period = "6M") {
  UseMethod("graph")
}

insttype <- function(instrument) {
  UseMethod("insttype")
}

#' Leave-One-Out test for bootstrapping
#'
#' @details
#' This test bootstraps a curve on the full instrument list, and again on
#' a new instrument list with one instrument removed. The difference of 
#' the discount factor for the removed instrument is reported.
#' The argument `indices` can be a vector, in which case each index in the
#' vector is treated separately.
#'
#' @param inst_list an object of class \code{\link{rh_instrument_list}}, created by \code{\link{rh_new_instrument_list}}
#' @param settings see \code{\link{rh_new_curve}}
#' @param indices an index, or a vector of indices, of instruments to leave out
#'
#' @return an object with field diff, containing the differences between the fixings and the quotes for the instruments in inst_list
#' @export
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "no",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   leave_one_out(inst_list_ois, settings, c(5, 10, 15))
leave_one_out <- function(inst_list, settings, indices) {
  UseMethod("leave_one_out")
}

#' Re-anchor a curve
#'
#' @param curve an object of class \code{\link{rh_curve}}
#' @param depo_sn the spot-next deposit (only used for "ontn")
#'
#' @details
#'   the action performed depends on the class of curve; for the classes
#'   rh_anchor_date, rh_anchor_ontn and rh_anchor_11 a specific
#'   re-anchoring takes place; if none of these classes are assigned
#'   the curve is returned unmodified.
#'
#' @return an object of class \code{\link{rh_curve}}
#' @export
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "ontn",
#'                    snt = "ois",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois_2d <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'   curve_ois_0d <- re_anchor(curve_ois_2d, inst_list_ois[[1]])
re_anchor <- function(curve, depo_sn) {
  UseMethod("re_anchor")
}

#' Round-trip test for a bootstrapped curve
#'
#' @details
#'   compare the fixings of an instrument list to the quotes
#'   when used on the instrument list that was used to bootstrap the curve, this is called a round-trip test.
#'
#' @param curve a bootstrapped curve (created by \code{\link{bootstrap}})
#' @param inst_list an object of class \code{\link{rh_instrument_list}}, created by \code{\link{rh_new_instrument_list}}
#'
#' @return an object with field diff, containing the differences between the fixings and the quotes for the instruments in inst_list
#' @export
#'
#' @examples
#'   settings <- list(today_date = "2022-11-30",
#'                    daycount = "act365",
#'                    interp_what = "df",
#'                    interp_how = "linear",
#'                    anchor = "no",
#'                    snt = "no",
#'                    ccy = "EUR")
#'   inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings)
#'   curve_ois <- bootstrap(rh_new_curve(settings), inst_list_ois)
#'   roundtrip(curve_ois, inst_list_ois)
roundtrip <- function(curve, inst_list) {
  UseMethod("roundtrip")
}

set_bootstrap_options <- function(curve) {
  UseMethod("set_bootstrap_options")
}

set_conventions <- function(instrument) {
  UseMethod("set_conventions")
}

set_convexity <- function(instrument, mrs, vol) {
  UseMethod("set_convexity")
}

set_interpolate <- function(curve) {
  UseMethod("set_interpolate")
}

set_mat_date <- function(instrument, start_no, start_unit) {
  UseMethod("set_mat_date")
}

set_schedule <- function(instrument, tenor_m) {
  UseMethod("set_schedule")
}

set_spot_date <- function(curve, settings_spot_date) {
  UseMethod("set_spot_date")
}

set_times <- function(curve, inst_list) {
  UseMethod("set_times")
}

try_pillar <- function(curve, instrument, df, pillar) {
  UseMethod("try_pillar")
}

update_discount_factors <- function(curve, df, pillar) {
  UseMethod("update_discount_factors")
}
