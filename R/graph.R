#' @export
graph.rh_curve <- function(curve,
                           t = NULL,
                           max_t = NULL,
                           n_points = NULL,
                           fwd_period = "6M") {

  # inputs
  if (is.null(t)) {
    if (is.null(max_t) || max_t >= max(curve$times) - 0.55)
      max_t <- max(curve$times) - 0.55
    if (is.null(n_points))
      n_points <- 500
    t <- seq(0, max_t, length.out = n_points)
    tlim <- seq(0, max(curve$times) - 0.55, length.out = n_points)
  }
  if (max(t) >= max(curve$times) - 0.55)
    xlim <- c(0, max(curve$times))
  else
    xlim <- c(0, max(t))
  periods <- list("1M" = 1 / 12, "3M" = 1 / 4, "6M" = 1 / 2,
                  "12M" = 1, "1Y" = 1)
  interp <- curve$interp_class
  if (interp[1] == "df")
    interp_what <- "discount factor"
  else if (interp[1] == "logdf")
    interp_what <- "log(discount factor)"
  else if (interp[1] == "zero")
    interp_what <- "zero rate"
  if (interp[2] == "splinecorr")
    interp[2] <- "spline (corr)"
  interp <- rev(interp[-1])

  #lims
  zero <- 100 * get_zero_rates(curve, times = tlim)
  discount <- get_discount_factors(curve, times = tlim)
  insta_fwd <- 100 * get_insta_fwd(curve, times = tlim)
  t_fwd <- tlim + periods[[fwd_period]]
  discount_fwd <- get_discount_factors(curve, times = t_fwd)
  discrete_fwd <-
    100 * (discount / discount_fwd - 1) / periods[[fwd_period]]
  yl_min <- min(c(0, zero, insta_fwd, discrete_fwd), na.rm = TRUE)
  yl_max <- max(c(zero, insta_fwd, discrete_fwd), na.rm = TRUE)
  yr_min <- min(discount, na.rm = TRUE)
  yr_max <- max(discount, na.rm = TRUE)

  #data
  zero <- 100 * get_zero_rates(curve, times = t)
  discount <- get_discount_factors(curve, times = t)
  insta_fwd <- 100 * get_insta_fwd(curve, times = t)
  t_fwd <- t + periods[[fwd_period]]
  discount_fwd <- get_discount_factors(curve, times = t_fwd)
  discrete_fwd <-
    100 * (discount / discount_fwd - 1) / periods[[fwd_period]]

  # graphics parameters
  colors <- c("brown3", "chocolate1", "darkgreen", "steelblue3")
  ltys <- c(1, 2, 4, 5)
  if (!is.null(curve$name))
    title1 <- toupper(curve$name)
  else
    title1 <- toupper(substring(deparse(substitute(curve)), 7))
  title2 <- paste0("reference date: ",
                   curve$today_date,
                   ", spot date: ",
                   curve$spot_date_orig,
                   ", re-anchoring: ",
                   curve$anchor)
  title3 <- paste("interpolation:",
                  paste(interp, collapse = " "),
                  "on",
                  interp_what)
  if (!is.null(curve$synth_inst))
    title4 <- paste("synthetic instruments:", curve$synth_inst)

  # plot
  par(mar = c(4, 5, 6, 5) + 0.1)
  plot(t,
       zero,
       'l',
       xlab = "time (years)",
       ylab = "interest rate (%)",
       col = colors[1],
       xlim = xlim,
       ylim = c(yl_min, yl_max),
       xaxs = "i",
       lty = ltys[1])
  lines(t, discrete_fwd, 'l', col = colors[2], lty = ltys[2])
  lines(t, insta_fwd, 'l', col = colors[3], lty = ltys[3])
  par(new = TRUE)
  plot(t,
       discount,
       'l',
       axes = FALSE,
       xlab = "",
       ylab = "",
       col = colors[4],
       xlim = xlim,
       ylim = c(yr_min, yr_max),
       xaxs = "i",
       lty = ltys[4])
  mtext("discount factor", side = 4, col = colors[4], line = 3)
  axis(4,
       ylim = c(yr_min, yr_max),
       col = colors[4],
       col.axis = colors[4],
       las = 1)
  mtext(side = 3, line = 4, cex = 1.5, title1)
  mtext(side = 3, line = 3, title2)
  mtext(side = 3, line = 2, title3)
  if (!is.null(curve$synth_inst))
    mtext(side = 3, line = 1, title4)
  legend("bottom",
         legend = c("zero rate",
                    paste(fwd_period, "forward"),
                    "instantaneous forward",
                    "discount factor"),
         col = colors,
         lty = ltys,
         cex = 0.7)
}
