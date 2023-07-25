helper_roundtrip_test <- function(settings, i) {
  settings_ois <- settings
  settings_ois$snt = "no"
  inst_list_ois <- rh_new_instrument_list(inst_df_ois, settings_ois)
  curve_ois <- bootstrap(rh_new_curve(settings_ois), inst_list_ois)
  rt <- roundtrip(curve_ois, inst_list_ois)
  actual <- c(i, 1, rt$diff)
  expected <- actual
  expected[c(-1,-2)] <- 0
  expect_equal(actual, expected)

  settings_ibor <- settings
  inst_list_ibor <- rh_new_instrument_list(inst_df_ibor, settings_ibor)
  curve_ibor_endo <- bootstrap(rh_new_curve(settings_ibor),
                               inst_list_ibor)
  rt <- roundtrip(curve_ibor_endo, inst_list_ibor)
  actual <- c(i, 2, rt$diff)
  expected <- actual
  expected[c(-1,-2)] <- 0
  expect_equal(actual, expected)

  settings_ibor$discount_curve <- "curve_ois"
  inst_list_ibor_snt <- add_synthetics(inst_list_ibor, settings_ibor)
  curve_ibor_exo <- bootstrap(rh_new_curve(settings_ibor),
                              inst_list_ibor_snt)
  curve_ibor_exo <- re_anchor(curve_ibor_exo, inst_list_ois[[1]])
  rt <- roundtrip(curve_ibor_exo, inst_list_ibor)
  actual <- c(i, 3, rt$diff)
  expected <- actual
  expected[c(-1,-2)] <- 0
  expect_equal(actual, expected)
}

test_that("roundtrip", {
  settings <- list(today_date = "2022-11-30",
                   daycount = "act365",
                   interp_what = "zero",
                   interp_how = "linear",
                   anchor = "no",
                   snt = "no",
                   ccy = "GBP")
  helper_roundtrip_test(settings, 1)
  settings$ccy <- "EUR"
  settings$interp_what <- "logdf"
  settings$interp_how <- "bessel"
  helper_roundtrip_test(settings, 2)
  settings$interp_what <- "df"
  settings$interp_how <- "hyman"
  settings$anchor <- "no"
  helper_roundtrip_test(settings, 3)
  settings$interp_how <- "hyman0"
  settings$snt <- "ois"
  settings$spline_corr <- "yes"
  settings$anchor <- "no"
  helper_roundtrip_test(settings, 4)
})

test_that("benchmark", {
  testvec <- 1:10
  curve_zero <- structure(list(),
                          class=c("rh_curve", "rh_interp_zero"))
  curve_zero$interpolate <- list(dim_d = 10,
                                 coeff = array(testvec, c(1, 10)))
  act_zero <- benchmark(curve_zero, bench_zeros = testvec[-1])
  exp_zero <- act_zero
  exp_zero$zeros[] <- 0
  expect_equal(act_zero, exp_zero)
  curve_df <- structure(list(),
                        class=c("rh_curve", "rh_interp_df"))
  curve_df$interpolate <- list(dim_d = 10,
                               coeff = array(testvec, c(1, 10)))
  act_df <- benchmark(curve_df, bench_dfs = testvec[-1])
  exp_df <- act_df
  exp_df$dfs[] <- 0
  expect_equal(act_df, exp_df)
})

