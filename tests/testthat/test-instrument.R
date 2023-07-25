test_that("calc_schedule", {
  inst_mo_tenor <- structure(list(mat_unit = "MO",
                                  spot_date = as.Date("2001-01-01"),
                                  tenor_m = 3,
                                  mat_no = 9),
                             class = "rh_sched_tenor")
  inst_yr_tenor <- structure(list(mat_unit = "YR",
                                  spot_date = as.Date("2001-01-01"),
                                  tenor_m = 3,
                                  mat_no = 1),
                             class = "rh_sched_tenor")
  inst_xx_tenor <- structure(list(mat_unit = "XX"),
                             class = "rh_sched_tenor")
  inst_dy_yearly <- structure(list(mat_unit = "DY",
                                   mat_date = as.Date("2020-02-20")),
                              class = "rh_sched_yearly")
  inst_wk_yearly <- structure(list(mat_unit = "WK",
                                   mat_date = as.Date("2020-02-20")),
                              class = "rh_sched_yearly")
  inst_mo_6_yearly <- structure(list(mat_unit = "MO",
                                     mat_no = 6,
                                     mat_date = as.Date("2020-02-20")),
                                class = "rh_sched_yearly")
  inst_mo_18_yearly <- structure(list(mat_unit = "MO",
                                      mat_no = 18,
                                      spot_date = as.Date("2001-01-01")),
                                 class = "rh_sched_yearly")
  inst_yr_1_yearly <- structure(list(mat_unit = "YR",
                                     mat_no = 1,
                                     mat_date = as.Date("2020-02-20")),
                                class = "rh_sched_yearly")
  inst_yr_3_yearly <- structure(list(mat_unit = "YR",
                                     mat_no = 3,
                                     spot_date = as.Date("2001-01-01")),
                                class = "rh_sched_yearly")
  expect_equal(calc_schedule(inst_mo_tenor, adjust = "none"),
                   c(as.Date("2001-04-01"),
                     as.Date("2001-07-01"),
                     as.Date("2001-10-01")))
  expect_equal(calc_schedule(inst_yr_tenor, adjust = "none"),
                   c(as.Date("2001-04-01"),
                     as.Date("2001-07-01"),
                     as.Date("2001-10-01"),
                     as.Date("2002-01-01")))
  expect_error(calc_schedule(inst_xx_tenor),
               "unknown mat_unit in calc_schedule.rh_sched_tenor")
  expect_equal(calc_schedule(inst_dy_yearly, adjust = "none"),
                   as.Date("2020-02-20"))
  expect_equal(calc_schedule(inst_wk_yearly, adjust = "none"),
                   as.Date("2020-02-20"))
  expect_equal(calc_schedule(inst_mo_6_yearly, adjust = "none"),
                   as.Date("2020-02-20"))
  expect_equal(calc_schedule(inst_mo_18_yearly, adjust = "none"),
                   as.Date("2001-07-01"))
  expect_equal(calc_schedule(inst_yr_1_yearly, adjust = "none"),
                   as.Date("2020-02-20"))
  expect_equal(calc_schedule(inst_yr_3_yearly, adjust = "none"),
                   c(as.Date("2002-01-01"),
                     as.Date("2003-01-01"),
                     as.Date("2004-01-01")))
})

test_that("fixing", {
  dates <- as.Date("2023-05-01") + 50 * 0:4
  dfs <- 1 - 0.01 * 0:4
  dfs2 <- c(1, dfs[-1] + 0.005)
  curve <- structure(list(spot_date = dates[1]),
                     class=c("rh_curve", "rh_interp_df", "rh_act365"))
  curve$times <- daycount(curve, dates[1], dates)
  curve$interpolate <- list(dim_d = 5,
                            coeff = array(dfs, c(1, 5)))
  curve1 <- curve
  curve1$discount_curve <- "curve2"
  curve2 <- curve
  curve2$interpolate$coeff <- array(dfs2, c(1, 5))
  obj_depo <- structure(list(spot_date = dates[1], mat_date = dates[5]),
                        class = c("rh_deposit", "rh_act360"))
  obj_fra <- structure(list(start_date = dates[3], mat_date = dates[5]),
                       class = c("rh_fra", "rh_act360"))
  obj_fut <- structure(list(start_date = dates[3], mat_date = dates[5]),
                       class = c("rh_future", "rh_act360"))
  obj_irs <- structure(list(spot_date = dates[1],
                            float_schedule = dates[c(3,5)],
                            fix_schedule = dates[-1]),
                       class = c("rh_irs", "rh_30e360"))
  obj_ois <- structure(list(spot_date = dates[1],
                            payment_schedule = dates[-1]),
                       class = c("rh_ois", "rh_act360"))
  expect_equal(fixing(obj_depo, curve), (1/dfs[5] - 1) * 360 / 200)
  expect_equal(fixing(obj_fra, curve), (dfs[3]/dfs[5] - 1) * 360 / 100)
  expect_equal(fixing(obj_fut, curve), (dfs[3]/dfs[5] - 1) * 360 / 100)
  accrual_fix <- diff(daycount(obj_irs, dates[1], dates))
  expect_equal(fixing(obj_irs, curve),
               (dfs[c(1,3)] / dfs[c(3,5)] - 1) %*% dfs[c(3,5)] /
                        dfs[-1] %*% accrual_fix)
  expect_equal(fixing(obj_irs, curve1),
               (dfs[c(1,3)] / dfs[c(3,5)] - 1) %*% dfs2[c(3,5)] /
                        dfs2[-1] %*% accrual_fix)
  dyf <- diff(daycount(obj_ois, dates[1], dates))
  expect_equal(fixing(obj_ois, curve), (1 - dfs[5]) / dfs[-1] %*% dyf)
})

test_that("insttype", {
  obj_synthetic = structure(list(), class = "rh_synthetic")
  obj_deposit = structure(list(), class = "rh_deposit")
  obj_fra = structure(list(), class = "rh_fra")
  obj_future = structure(list(), class = "rh_future")
  obj_irs = structure(list(), class = "rh_irs")
  obj_ois = structure(list(), class = "rh_ois")
  expect_identical(insttype(obj_synthetic), "synthetic")
  expect_identical(insttype(obj_deposit), "deposit")
  expect_identical(insttype(obj_fra), "fra")
  expect_identical(insttype(obj_future), "future")
  expect_identical(insttype(obj_irs), "irs")
  expect_identical(insttype(obj_ois), "ois")
})

test_that("set_conventions", {
  today_date = as.Date("2022-02-22")
  today_date2 = as.Date("2022-02-28")
  inst1 <- structure(list(ccy = "EUR",
                          today_date = today_date,
                          mat_unit = "DY"),
                     class = c("rh_instrument", "rh_deposit"))
  inst1_act <- set_conventions(inst1)
  expect_identical(inst1_act$calendar, "target")
  expect_equal(inst1_act$spot_date, as.Date("2022-02-24"))
  expect_identical(class(inst1_act), 
                   c("rh_instrument", "rh_deposit", "rh_act360", 
                     "rh_following"))
  inst2 <- structure(list(ccy = "EUR",
                          today_date = today_date,
                          mat_unit = "WK"),
                     class = c("rh_instrument", "rh_ois"))
  inst2_act <- set_conventions(inst2)
  expect_identical(inst2_act$calendar, "target")
  expect_equal(inst1_act$spot_date, as.Date("2022-02-24"))
  expect_identical(class(inst2_act), 
                   c("rh_instrument", "rh_ois", "rh_act360", 
                     "rh_following"))
  inst3 <- structure(list(ccy = "EUR",
                          today_date = today_date,
                          mat_unit = "MO"),
                     class = c("rh_instrument", "rh_irs"))
  inst3_act <- set_conventions(inst3)
  expect_identical(inst3_act$calendar, "target")
  expect_equal(inst3_act$spot_date, as.Date("2022-02-24"))
  expect_identical(class(inst3_act), 
                   c("rh_instrument", "rh_irs", "rh_30e360", 
                     "rh_modfollowing"))
  inst4 <- structure(list(ccy = "GBP",
                          today_date = today_date2,
                          mat_unit = "YR"),
                     class = c("rh_instrument", "rh_fra"))
  inst4_act <- set_conventions(inst4)
  expect_identical(inst4_act$calendar, "uk")
  expect_equal(inst4_act$spot_date, today_date2)
  expect_identical(class(inst4_act), 
                   c("rh_instrument", "rh_fra", "rh_act365", 
                     "rh_modfollowing", "rh_end_of_month"))
  inst5 <- structure(list(ccy = "GBP",
                          today_date = today_date2,
                          mat_unit = "YR"),
                     class = c("rh_instrument", "rh_future"))
  inst5_act <- set_conventions(inst5)
  expect_identical(inst5_act$calendar, "uk")
  expect_equal(inst5_act$spot_date, today_date2)
  expect_identical(class(inst5_act), 
                   c("rh_instrument", "rh_future", "rh_act365", 
                     "rh_modfollowing"))
  inst6 <- structure(list(ccy = "XXX"), class = "rh_instrument")
  expect_error(set_conventions(inst6),
               "unknown currency in set_conventions")
})

test_that("set_convexity", {
  inst_def <- structure(list(), class = "rh_fra")
  inst_fut <- structure(list(), class = "rh_future")
  inst_fut_expect <- structure(list(convexity_adj = list(mean_rev_speed = 1,
                                                         volatility = 2)), 
                               class = "rh_future")
  expect_identical(set_convexity(inst_def, 1, 2), inst_def)
  expect_identical(set_convexity(inst_fut, 1, 2), inst_fut_expect)
})

test_that("set_mat_date", {
  spot_date = as.Date("2022-02-22")
  inst_depo <- structure(list(spot_date = spot_date,
                              mat_no = 3,
                              mat_unit = "MO"), 
                         class = "rh_deposit")
  mat_date_depo = calc_next_date(NULL, spot_date, 3, "MO")
  expect_equal(set_mat_date(inst_depo, 1, "MO")$mat_date, mat_date_depo)
  inst_fra <- structure(list(spot_date = spot_date,
                             mat_no = 3,
                             mat_unit = "MO"), 
                        class = "rh_fra")
  start_date_fra = calc_next_date(NULL, spot_date, 1, "MO")
  mat_date_fra = calc_next_date(NULL, spot_date, 4, "MO")
  expect_identical(set_mat_date(inst_fra, 1, "MO")$start_no, 1)
  expect_identical(set_mat_date(inst_fra, 1, "MO")$start_unit, "MO")
  expect_equal(set_mat_date(inst_fra, 1, "MO")$start_date, start_date_fra)
  expect_equal(set_mat_date(inst_fra, 1, "MO")$mat_date, mat_date_fra)
  inst_fut <- structure(list(today_date = spot_date,
                             mat_no = 3,
                             mat_unit = "MO"), 
                        class = "rh_future")
  start_date_fut = calc_next_date(inst_fut, spot_date, 1, "MO")
  mat_date_fut = calc_next_date(inst_fut, spot_date, 4, "MO")
  expect_identical(set_mat_date(inst_fut, 1, "MO")$start_no, 1)
  expect_identical(set_mat_date(inst_fut, 1, "MO")$start_unit, "MO")
  expect_equal(set_mat_date(inst_fut, 1, "MO")$start_date, start_date_fut)
  expect_equal(set_mat_date(inst_fut, 1, "MO")$mat_date, mat_date_fut)
})

test_that("set_schedule", {
  spot_date <- as.Date("2022-02-22")
  inst_def <- structure(list(), class = "rh_fra")
  expect_identical(set_schedule(inst_def, 3), inst_def)
  inst_irs1 <- structure(list(ccy = "EUR",
                              spot_date = spot_date,
                              mat_date = spot_date + 1,
                              mat_no = 6,
                              mat_unit = "MO"),
                         class = c("rh_instrument", "rh_irs"))
  inst_irs1_act <- set_schedule(inst_irs1, 3)
  inst_irs1$tenor_m <- 3
  inst_irs1_float <- calc_schedule.rh_sched_tenor(inst_irs1)
  expect_identical(inst_irs1_act$fix_schedule, inst_irs1$mat_date)
  expect_identical(inst_irs1_act$float_schedule, inst_irs1_float)
  inst_irs2 <- structure(list(ccy = "GBP",
                              spot_date = spot_date,
                              mat_no = 3,
                              mat_unit = "YR"),
                         class = c("rh_instrument", "rh_irs"))
  inst_irs2_act <- set_schedule(inst_irs2, 12)
  inst_irs2$tenor_m <- 12
  inst_irs2_sched <- calc_schedule.rh_sched_tenor(inst_irs2)
  expect_identical(inst_irs2_act$fix_schedule, inst_irs2_sched)
  expect_identical(inst_irs2_act$float_schedule, inst_irs2_sched)
  inst_irs3 <- structure(list(ccy = "XXX"),
                         class = c("rh_instrument", "rh_irs"))
  expect_error(set_schedule(inst_irs3, 5),
               "unknown currency in set_schedule")
  inst_ois1 <- structure(list(ccy = "EUR",
                              spot_date = spot_date,
                              mat_date = spot_date + 1,
                              mat_no = 6,
                              mat_unit = "MO"),
                         class = c("rh_instrument", "rh_ois"))
  inst_ois1_act <- set_schedule(inst_ois1, 3)
  inst_ois1$tenor_m <- 3
  expect_identical(inst_ois1_act$payment_schedule, inst_ois1$mat_date)
  inst_ois2 <- structure(list(ccy = "GBP",
                              spot_date = spot_date,
                              mat_no = 3,
                              mat_unit = "YR"),
                         class = c("rh_instrument", "rh_irs"))
  inst_ois2_act <- set_schedule(inst_ois2, 12)
  inst_ois2$tenor_m <- 12
  inst_ois2_sched <- calc_schedule.rh_sched_yearly(inst_ois2)
  expect_identical(inst_ois2_act$payment_schedule, inst_ois2$sched)
})
