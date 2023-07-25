test_that("get_insta_fwd", {
  curve <- structure(list(), class=c("rh_curve", "rh_interp_df"))
  curve$times <- 0:3
  curve$interpolate <- list(dim_d = 4,
                            dim_c = 2,
                            coeff = array(c(0.5, 0.6), c(2, 4)))
  expect_identical(get_insta_fwd(curve), rep(-0.6/0.5, 4))
  class(curve) <- c("rh_curve", "rh_interp_logdf")
  expect_identical(get_insta_fwd(curve), rep(-0.6, 4))
  class(curve) <- c("rh_curve", "rh_interp_zero")
  expect_identical(get_insta_fwd(curve), 0:3 * 0.6 + 0.5)
})

test_that("get_discount_factors", {
  curve <- structure(list(), class=c("rh_curve", "rh_interp_df"))
  curve$times <- 0:3
  curve$interpolate <- list(dim_d = 4,
                            coeff = array(0.5, c(1, 4)))
  expect_identical(get_discount_factors(curve), rep(0.5, 4))
  class(curve) <- c("rh_curve", "rh_interp_logdf")
  expect_identical(get_discount_factors(curve), rep(exp(0.5), 4))
  class(curve) <- c("rh_curve", "rh_interp_zero")
  expect_identical(get_discount_factors(curve), exp(-0.5 * 0:3))
})

test_that("get_zero_rates", {
  curve <- structure(list(), class=c("rh_curve", "rh_interp_zero"))
  curve$times <- 0:3
  curve$interpolate <- list(dim_d = 4,
                            coeff = array(0.5, c(1, 4)))
  expect_identical(get_zero_rates(curve), rep(0.5, 4))
  class(curve) <- c("rh_curve", "rh_interp_df")
  expect_identical(get_zero_rates(curve, pillars = -1), -log(0.5) / 1:3)
  class(curve) <- c("rh_curve", "rh_interp_logdf")
  expect_identical(get_zero_rates(curve, pillars = -1), - 0.5 / 1:3)
})

test_that("set_spot_date", {
  today_date = as.Date("2022-02-22")
  curve_eur <- structure(list(ccy = "EUR", today_date = today_date),
                         class = "rh_curve")
  curve_eur_act1 <- set_spot_date(curve_eur, NULL)
  expect_equal(curve_eur_act1$spot_date, as.Date("2022-02-24"))
  expect_equal(curve_eur_act1$calendar, "target")
  curve_eur_act2 <- set_spot_date(curve_eur, "2000-01-01")
  expect_equal(curve_eur_act2$spot_date, as.Date("2000-01-01"))
  expect_equal(curve_eur_act2$calendar, "target")
  curve_gbp <- structure(list(ccy = "GBP", today_date = today_date),
                         class = "rh_curve")
  curve_gbp_act1 <- set_spot_date(curve_gbp, NULL)
  expect_equal(curve_gbp_act1$spot_date, today_date)
  expect_equal(curve_gbp_act1$calendar, "uk")
  curve_gbp_act2 <- set_spot_date(curve_gbp, "2000-01-01")
  expect_equal(curve_gbp_act2$spot_date, as.Date("2000-01-01"))
  expect_equal(curve_gbp_act2$calendar, "uk")
  curve_xxx <- structure(list(ccy = "XXX", today_date = today_date),
                         class = "rh_curve")
  expect_error(set_spot_date(curve_xxx, NULL),
               "unknown currency in set_spot_date")
})

test_that("update_discount_factors", {
  df <- 0.9
  curve <- structure(list(), class=c("rh_curve",
                                     "rh_interp_linear",
                                     "rh_interp_df"))
  curve$times <- 1:3
  curve$interpolate <- list(dim_d = 3,
                            dim_c = 2,
                            coeff = array(1, c(2, 3)))
  curve_df_all <- update_discount_factors(curve, rep(df, 3))
  expect_equal(curve_df_all$interpolate$coeff[1,], rep(df, 3))
  curve_df_2 <- update_discount_factors(curve, df, pillar = 2)
  expect_equal(curve_df_2$interpolate$coeff[1,], c(1, df, 1))
  class(curve) <- c("rh_curve", "rh_interp_linear", "rh_interp_logdf")
  curve_logdf_2 <- update_discount_factors(curve, df, pillar = 2)
  expect_equal(curve_logdf_2$interpolate$coeff[1,], c(1, log(df), 1))
  curve_logdf_all <- update_discount_factors(curve, rep(df, 3))
  expect_equal(curve_logdf_all$interpolate$coeff[1,], rep(log(df), 3))
  class(curve) <- c("rh_curve", "rh_interp_linear", "rh_interp_zero")
  curve_zero_all <- update_discount_factors(curve, rep(df, 3))
  expect_equal(curve_zero_all$interpolate$coeff[1,], c(0, -log(df) / c(2, 3)))
  curve_zero_2 <- update_discount_factors(curve, df, pillar = 2)
  expect_equal(curve_zero_2$interpolate$coeff[1,], c(1, -log(df) / 2, 1))
})

test_that("update_and_get_discount_factor", {
  df <- 0.9
  curve <- structure(list(), class=c("rh_curve",
                                     "rh_interp_linear",
                                     "rh_interp_df"))
  curve$times <- 0:3
  curve$interpolate <- list(dim_d = 3,
                            dim_c = 2,
                            coeff = array(1, c(2, 3)))
  df_act1 <- get_discount_factors(update_discount_factors(curve,
                                                          df,
                                                          pillar = 1),
                                  pillars = 1)
  expect_equal(df_act1, df)
  df_act2 <- get_discount_factors(update_discount_factors(curve,
                                                          df,
                                                          pillar = 2),
                                  pillars = 2)
  expect_equal(df_act2, df)
  df_act3 <- get_discount_factors(update_discount_factors(curve,
                                                          df,
                                                          pillar = 3),
                                  pillars = 3)
  expect_equal(df_act3, df)
  class(curve) <- c("rh_curve", "rh_interp_linear", "rh_interp_logdf")
  logdf_act1 <- get_discount_factors(update_discount_factors(curve,
                                                             df,
                                                             pillar = 1),
                                     pillars = 1)
  expect_equal(logdf_act1, df)
  logdf_act2 <- get_discount_factors(update_discount_factors(curve,
                                                             df,
                                                             pillar = 2),
                                     pillars = 2)
  expect_equal(logdf_act2, df)
  logdf_act3 <- get_discount_factors(update_discount_factors(curve,
                                                             df,
                                                             pillar = 3),
                                     pillars = 3)
  expect_equal(logdf_act3, df)
  class(curve) <- c("rh_curve", "rh_interp_linear", "rh_interp_zero")
  zero_act1 <- get_discount_factors(update_discount_factors(curve,
                                                            df,
                                                            pillar = 1),
                                    pillars = 1)
  expect_equal(zero_act1, 1)
  zero_act2 <- get_discount_factors(update_discount_factors(curve,
                                                            df,
                                                            pillar = 2),
                                    pillars = 2)
  expect_equal(zero_act2, df)
  zero_act3 <- get_discount_factors(update_discount_factors(curve,
                                                            df,
                                                            pillar = 3),
                                    pillars = 3)
  expect_equal(zero_act3, df)
})

