#' @export
add_synthetics.default <- function(inst_list_raw, settings) {
  inst_list_raw
}

#' @export
add_synthetics.rh_snt_ois <- function(inst_list_raw, settings) {
  discount_curve <- rh_get_discount_curve(settings$discount_curve)
  curve_aux <- bootstrap(rh_new_curve(settings), inst_list_raw[1])
  zero_rate <- get_zero_rates(curve_aux, pillars = 2)
  yf <- curve_aux$times[2]
  stub_times_dc <-
    discount_curve$times[discount_curve$times < yf][-1]
  discount_rates <-
    get_zero_rates(discount_curve, times = c(stub_times_dc, yf))
  stub_rates <- head(discount_rates,-1) +
    (zero_rate - tail(discount_rates, 1)) / yf * stub_times_dc
  stub_dates <- get_dates(discount_curve, stub_times_dc)
  stub_times_fc <-
    daycount(curve_aux, curve_aux$spot_date, stub_dates)
  template_stub <- structure(list(discount_factor = NULL, mat_date = NULL),
                             class = c("rh_instrument", "rh_synthetic"))
  for (i in seq_along(stub_rates)) {
    template_stub$discount_factor <- exp(-stub_rates[i] * stub_times_fc[i])
    template_stub$mat_date <- stub_dates[i]
    inst_list_raw <- append(inst_list_raw, list(template_stub))
  }
  inst_order <- order(sapply(inst_list_raw, '[[', 'mat_date'))
  structure(inst_list_raw[inst_order], 
            class = "rh_instrument_list",
            names = seq_along(inst_list_raw))
}
