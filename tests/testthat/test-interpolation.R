test_that("calc_interp_coeff - linear", {
  times = c(5,6,9,11,12,15,16,17,18,19,20)
  values = c(12,3,1,5,19,8,17,13,18,1,7)
  curve <- structure(list(times = times),
                     class=c("rh_curve", "rh_interp_linear", "rh_interp_df"))
  curve$interpolate <- list(dim_d = length(times),
                            dim_c = 2,
                            coeff = array(NA, c(2, length(times))))
  curve <- update_discount_factors(curve, values)
  test_act = curve$interpolate$coeff[1,] +
                  curve$interpolate$coeff[2,] * c(diff(times), 1)
  test_exp = curve$interpolate$coeff[1,-1]
  expect_equal(head(test_act, -1), test_exp)
})

helper_test_spline_cd <- function(spline, type) {
  times = c(5,6,9,11,12,15,16,17,18,19,20)
  values = c(12,3,1,5,19,8,17,13,18,1,7)
  curve <- structure(list(times = times),
                     class=c("rh_curve", spline, type, "rh_interp_df"))
  curve$interpolate <- list(dim_d = length(times),
                                   dim_c = 4,
                                   coeff = array(NA, c(4, length(times))))
  curve <- update_discount_factors(curve, values)
  if (spline == "rh_interp_splinecorr") {
    selection = c(-length(times) + 1, -length(times))
    helper_test_spline_cd_coeff(times[selection],
                                curve$interpolate$coeff[, selection])
    expect_equal(sum(curve$interpolate$coeff[c(1, 2), length(times) - 1]),
                 curve$interpolate$coeff[1, length(times)])
  } else
    helper_test_spline_cd_coeff(times, curve$interpolate$coeff)
}

helper_test_spline_cd_coeff <- function(times, coeff) {
  mm <- diff(coeff[1,]) / diff(times)
  bp <- coeff[2,-1]
  bb <- coeff[2, -length(times)]
  cc <- (3*mm - bp - 2*bb) / diff(times)
  dd = (bp + bb - 2*mm) / (diff(times))^2
  expect_equal(coeff[3, -length(times)], cc)
  expect_equal(coeff[4, -length(times)], dd)
}

test_that("calc_interp_coeff - spline c&d", {
  helper_test_spline_cd("rh_interp_spline", "rh_interp_bessel")
  helper_test_spline_cd("rh_interp_spline", "rh_interp_hyman")
  helper_test_spline_cd("rh_interp_spline", "rh_interp_hyman0")
  helper_test_spline_cd("rh_interp_splinecorr", "rh_interp_bessel")
  helper_test_spline_cd("rh_interp_splinecorr", "rh_interp_hyman")
  helper_test_spline_cd("rh_interp_splinecorr", "rh_interp_hyman0")
})

test_that("hyman splines", {
  times = c(5,6,9,11,12,15,16,17,18,19,20)
  test_x <- seq(5, 20, length.out=1000)
  v_mon = c(1,2,3,5,7,8,12,13,17,18,19)
  v_alt = c(2,1,5,3,8,7,13,12,18,15,19)
  curve <- structure(list(times = times),
                            class=c("rh_curve", "rh_interp_spline", 
                                    "rh_interp_hyman", "rh_interp_df"))
  curve$interpolate <- list(dim_d = length(times),
                                   dim_c = 4,
                                   coeff = array(NA, c(4, length(times))))
  curve <- update_discount_factors(curve, v_mon)
  test_hyman_inc <- get_discount_factors(curve, times=test_x)
  expect_gt(min(diff(test_hyman_inc)), 0)
  curve <- update_discount_factors(curve, -v_mon)
  test_hyman_dec <- get_discount_factors(curve, times=test_x)
  expect_lt(max(diff(test_hyman_dec)), 0)
  curve <- update_discount_factors(curve, v_alt)
  test_hyman_b_alt_act <- curve$interpolate$coeff[2, c(-1, -length(times))]
  test_hyman_b_alt_exp <- test_hyman_b_alt_act
  test_hyman_b_alt_exp[] <- 0
  expect_equal(test_hyman_b_alt_act, test_hyman_b_alt_exp)
  curve <- structure(list(times = times),
                            class=c("rh_curve", "rh_interp_spline", 
                                    "rh_interp_hyman0", "rh_interp_df"))
  curve$interpolate <- list(dim_d = length(times),
                                   dim_c = 4,
                                   coeff = array(NA, c(4, length(times))))
  curve <- update_discount_factors(curve, v_mon)
  test_hyman0_inc <- get_discount_factors(curve, times=test_x)
  expect_gt(min(diff(test_hyman0_inc)), 0)
  curve <- update_discount_factors(curve, -v_mon)
  test_hyman0_dec <- get_discount_factors(curve, times=test_x)
  expect_lt(max(diff(test_hyman0_dec)), 0)
  curve <- update_discount_factors(curve, v_alt)
  test_hyman0_b_alt_act <- curve$interpolate$coeff[2,]
  test_hyman0_b_alt_exp <- test_hyman0_b_alt_act
  test_hyman0_b_alt_exp[] <- 0
  expect_equal(test_hyman0_b_alt_act, test_hyman0_b_alt_exp)
})

test_that("get_curve_data", {
  spot_date = as.Date("2000-01-01")
  coeff = array(1:12, c(4, 3))
  curve <- structure(list(spot_date = spot_date, times = 0:2),
                     class=c("rh_curve", "rh_30e360"))
  curve$interpolate <- list(dim_d = 3,
                            dim_c = 4,
                            coeff = coeff)
  act1 <- get_curve_data(curve, NULL, NULL, NULL)
  exp1 <- list(data = 1 + 4 * 0:2, times = 0:2)
  expect_equal(act1, exp1)
  act2 <- get_curve_data(curve,
                         c(as.Date("2000-07-01"), as.Date("2001-07-01")),
                         NULL, NULL)
  exp2_data = 0.5^(0:3) %*% coeff[, c(1, 2)]
  exp2 <- list(data = as.vector(exp2_data), times = c(0.5, 1.5))
  expect_equal(act2, exp2)
  act3 <- get_curve_data(curve, NULL, c(-1, 0.3, 1.3, 2.3), NULL)
  exp3_data = 0.3^(0:3) %*% coeff[, c(1, 2)]
  exp3 <- list(data = c(1, as.vector(exp3_data), coeff[1, 3]),
               times = c(-1, 0.3, 1.3, 2.3))
  expect_equal(act3, exp3)
  act4 <- get_curve_data(curve, NULL, NULL, c(1, 3))
  exp4 <- list(data = coeff[1, c(1, 3)], times = c(0, 2))
  expect_equal(act4, exp4)
  act5 <- get_curve_data(curve, NULL, c(0.3, 1.3), NULL, deriv = TRUE)
  exp5_data = 0.3^(0:2) %*% (coeff[-1, c(1, 2)] * (1:3))
  exp5 <- list(data = as.vector(exp5_data), times = c(0.3, 1.3))
  expect_equal(act5, exp5)
})
