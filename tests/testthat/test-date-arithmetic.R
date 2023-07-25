test_that("busday_adjust", {
  date1 <- as.Date("2022-04-30")
  date2 <- as.Date("2022-12-25")
  date3 <- as.Date("2023-03-29")
  obj_default <- NULL
  obj_uk_follow <- structure(list(calendar = "uk"),
                             class = "rh_following")
  obj_eur_follow <- structure(list(calendar = "target"),
                              class = "rh_following")
  obj_uk_modfol <- structure(list(calendar = "uk"),
                             class = "rh_modfollowing")
  obj_eur_modfol <- structure(list(calendar = "target"),
                              class = "rh_modfollowing")
  expect_equal(busday_adjust(obj_default, date1), as.Date("2022-05-02"))
  expect_equal(busday_adjust(obj_uk_follow, date1), as.Date("2022-05-03"))
  expect_equal(busday_adjust(obj_eur_follow, date1), as.Date("2022-05-02"))
  expect_equal(busday_adjust(obj_uk_modfol, date1), as.Date("2022-04-29"))
  expect_equal(busday_adjust(obj_eur_modfol, date1), as.Date("2022-04-29"))
  expect_equal(busday_adjust(obj_default, date2), as.Date("2022-12-26"))
  expect_equal(busday_adjust(obj_uk_follow, date2), as.Date("2022-12-28"))
  expect_equal(busday_adjust(obj_eur_follow, date2), as.Date("2022-12-27"))
  expect_equal(busday_adjust(obj_uk_modfol, date2), as.Date("2022-12-28"))
  expect_equal(busday_adjust(obj_eur_modfol, date2), as.Date("2022-12-27"))
  expect_equal(busday_adjust(obj_default, date3), date3)
  expect_equal(busday_adjust(obj_uk_follow, date3), date3)
  expect_equal(busday_adjust(obj_eur_follow, date3), date3)
  expect_equal(busday_adjust(obj_uk_modfol, date3), date3)
  expect_equal(busday_adjust(obj_eur_modfol, date3), date3)
  expect_equal(busday_adjust(obj_eur_modfol, as.Date("2002-03-30")),
                   as.Date("2002-03-28"))
})

test_that("calc_next_date", {
  date1 <- as.Date("2022-04-30")
  date2 <- as.Date("2022-12-25")
  obj_def <- NULL
  obj_eom <- structure(list(), class = "rh_end_of_month")
  obj_fut <- structure(list(), class = "rh_future")
  expect_equal(calc_next_date(obj_def, date1, 3, "DY", adjust = "none"),
                   as.Date("2022-05-03"))
  expect_equal(calc_next_date(obj_def, date1, 3, "WK", adjust = "none"),
                   as.Date("2022-05-21"))
  expect_equal(calc_next_date(obj_def, date1, 3, "MO", adjust = "none"),
                   as.Date("2022-07-30"))
  expect_equal(calc_next_date(obj_def, date1, 3, "YR", adjust = "none"),
                   as.Date("2025-04-30"))
  expect_equal(calc_next_date(obj_def, date1, 3, "DY"),
                   as.Date("2022-05-03"))
  expect_equal(calc_next_date(obj_def, date1, 3, "WK"),
                   as.Date("2022-05-23"))
  expect_equal(calc_next_date(obj_def, date1, 3, "MO"),
                   as.Date("2022-08-01"))
  expect_equal(calc_next_date(obj_def, date1, 3, "YR"),
                   as.Date("2025-04-30"))
  expect_equal(calc_next_date(obj_eom, date1, 3, "MO", adjust = "none"),
                   as.Date("2022-07-31"))
  expect_equal(calc_next_date(obj_eom, date1, 3, "YR", adjust = "none"),
                   as.Date("2025-04-30"))
  expect_equal(calc_next_date(obj_eom, date1, 3, "MO"),
                   as.Date("2022-08-01"))
  expect_equal(calc_next_date(obj_eom, date1, 3, "YR"),
                   as.Date("2025-04-30"))
  expect_equal(calc_next_date(obj_fut, date1, 3, "MO", adjust = "none"),
                   as.Date("2022-07-20"))
  expect_equal(calc_next_date(obj_fut, date1, 3, "YR", adjust = "none"),
                   as.Date("2025-04-16"))
  expect_equal(calc_next_date(obj_fut, date1, 3, "MO"),
                   as.Date("2022-07-20"))
  expect_equal(calc_next_date(obj_fut, date1, 3, "YR"),
                   as.Date("2025-04-16"))
  expect_equal(calc_next_date(obj_def, date2, 3, "DY", adjust = "none"),
                   as.Date("2022-12-28"))
  expect_equal(calc_next_date(obj_def, date2, 3, "WK", adjust = "none"),
                   as.Date("2023-01-15"))
  expect_equal(calc_next_date(obj_def, date2, 3, "MO", adjust = "none"),
                   as.Date("2023-03-25"))
  expect_equal(calc_next_date(obj_def, date2, 3, "YR", adjust = "none"),
                   as.Date("2025-12-25"))
  expect_equal(calc_next_date(obj_eom, date2, 3, "MO", adjust = "none"),
                   as.Date("2023-03-31"))
  expect_equal(calc_next_date(obj_eom, date2, 3, "YR", adjust = "none"),
                   as.Date("2025-12-31"))
  expect_equal(calc_next_date(obj_fut, date2, 3, "MO", adjust = "none"),
                   as.Date("2023-03-15"))
  expect_equal(calc_next_date(obj_fut, date2, 3, "YR", adjust = "none"),
                   as.Date("2025-12-17"))
})

test_that("daycount", {
  start_date1 <- as.Date("2019-11-30")
  start_date2 <- as.Date("2019-12-31")
  mat_date <- as.Date("2020-05-31")
  obj_act365 <- structure(list(), class = "rh_act365")
  obj_act360 <- structure(list(), class = "rh_act360")
  obj_30e360 <- structure(list(), class = "rh_30e360")
  expect_identical(daycount(obj_act365, start_date1, mat_date), 183/365)
  expect_identical(daycount(obj_act360, start_date1, mat_date), 183/360)
  expect_identical(daycount(obj_30e360, start_date1, mat_date), 1/2)
  expect_identical(daycount(obj_act365, start_date2, mat_date), 152/365)
  expect_identical(daycount(obj_act360, start_date2, mat_date), 152/360)
  expect_identical(daycount(obj_30e360, start_date2, mat_date), 1-7/12)
})

test_that("get_dates", {
  date <- as.Date("2001-01-01")
  curve <- structure(list(spot_date = date, times = c(1/365, 10/365)),
                     class = "rh_act365")
  expect_equal(get_dates(curve), c(date + 1, date + 10))
  expect_equal(get_dates(curve, times = c(100/365, 1)),
                   c(date + 100, date + 365))
})

test_that("rh_dayofmonth", {
  date01 <- as.Date("2000-01-01")
  date10 <- as.Date("2000-01-10")
  date15 <- as.Date("2000-01-15")
  date20 <- as.Date("2000-01-20")
  date31 <- as.Date("2000-01-31")
  expect_identical(rh_dayofmonth(date01), 1)
  expect_identical(rh_dayofmonth(date10), 10)
  expect_identical(rh_dayofmonth(date15), 15)
  expect_identical(rh_dayofmonth(date20), 20)
  expect_identical(rh_dayofmonth(date31), 31)
})

test_that("rh_month", {
  date01 <- as.Date("2000-01-01")
  date03 <- as.Date("2000-03-10")
  date05 <- as.Date("2000-05-15")
  date10 <- as.Date("2000-10-20")
  date12 <- as.Date("2000-12-31")
  expect_identical(rh_month(date01), 1)
  expect_identical(rh_month(date03), 3)
  expect_identical(rh_month(date05), 5)
  expect_identical(rh_month(date10), 10)
  expect_identical(rh_month(date12), 12)
})

test_that("rh_weekday", {
  date <- as.Date("2023-03-27")
  expect_identical(rh_weekday(date), 1)
  expect_identical(rh_weekday(date + 1), 2)
  expect_identical(rh_weekday(date + 2), 3)
  expect_identical(rh_weekday(date + 3), 4)
  expect_identical(rh_weekday(date + 4), 5)
  expect_identical(rh_weekday(date + 5), 6)
  expect_identical(rh_weekday(date + 6), 7)
})

test_that("rh_year", {
  date01 <- as.Date("2001-01-01")
  date10 <- as.Date("2010-01-01")
  date15 <- as.Date("2015-01-01")
  date20 <- as.Date("2020-01-01")
  date31 <- as.Date("2031-01-01")
  expect_identical(rh_year(date01), 2001)
  expect_identical(rh_year(date10), 2010)
  expect_identical(rh_year(date15), 2015)
  expect_identical(rh_year(date20), 2020)
  expect_identical(rh_year(date31), 2031)
})

