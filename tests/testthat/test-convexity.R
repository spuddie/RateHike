test_that("calc_convexity_adjustment", {
  inst_dep <- structure(list(), class = "rh_deposit")
  expect_null(calc_convexity_adjustment(inst_dep))
  settings <- list(today_date = "2022-11-30",
                   ccy = "EUR")
  inst_df <- list(quote = 0.03,
                  start_no = 6,
                  start_unit = "MO",
                  mat_no = 3,
                  mat_unit = "MO",
                  insttype = "future")
  future <- rh_new_instrument(inst_df, settings)
  future$convexity_adj <- list(mean_rev_speed = 0.025,
                               volatility = 0.05)
  future$price <- 96.9448
  expected <- 0.03
  actual = round(calc_convexity_adjustment(future), digits=6)
  expect_equal(actual, expected)
})

test_that("calc_inv_convexity_adjustment", {
  inst_dep <- structure(list(), class = "rh_deposit")
  expect_null(calc_inv_convexity_adjustment(inst_dep))
  settings <- list(today_date = "2022-11-30",
                   ccy = "EUR")
  inst_df <- list(quote = 0.03,
                  start_no = 6,
                  start_unit = "MO",
                  mat_no = 3,
                  mat_unit = "MO",
                  insttype = "future")
  future <- rh_new_instrument(inst_df, settings)
  future$convexity_adj <- list(mean_rev_speed = 0.025,
                               volatility = 0.05)
  expected <- 96.9448
  expect_equal(calc_inv_convexity_adjustment(future), expected)

})
