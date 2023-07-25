### setup
library(RateHike)

settings <- list(today_date = as.Date("2022-11-30"),
                 ccy = "EUR")

df_depo <- list(quote = 0,
                mat_no = 6,
                mat_unit = "MO",
                insttype = "deposit")

depo <- rh_new_instrument(df_depo, settings)

df_ois <- list(quote = 0,
               mat_no = 18,
               mat_unit = "MO",
               insttype = "ois")

ois <- rh_new_instrument(df_ois, settings)

df_fra <- list(quote = 0,
               start_no = 3,
               start_unit = "MO",
               mat_no = 6,
               mat_unit = "MO",
               insttype = "fra")

fra <- rh_new_instrument(df_fra, settings)

df_irs <- list(quote = 0,
               mat_no = 3,
               mat_unit = "YR",
               tenor_m = 6,
               insttype = "irs")

irs <- rh_new_instrument(df_irs, settings)

### 4.3.3 Deposits
settings$today_date
weekdays(settings$today_date)
depo$spot_date
weekdays(depo$spot_date)
depo$mat_date
weekdays(depo$mat_date)
daycount(depo, depo$spot_date, depo$mat_date) * 360

### 4.3.4 OIS
settings$today_date
weekdays(settings$today_date)
ois$spot_date
weekdays(ois$spot_date)
ois$mat_date
weekdays(ois$mat_date)
ois$payment_schedule
weekdays(ois$payment_schedule)
daycount(ois, ois$spot_date, ois$payment_schedule[1]) * 360
daycount(ois, ois$payment_schedule[1], ois$payment_schedule[2]) * 360

### 4.3.5 FRA
settings$today_date
weekdays(settings$today_date)
fra$start_date
weekdays(fra$start_date)
fra$mat_date
weekdays(fra$mat_date)
daycount(fra, fra$start_date, fra$mat_date) * 360

### 4.3.7 IRS
settings$today_date
weekdays(settings$today_date)
irs$spot_date
weekdays(irs$spot_date)
irs$fix_schedule
weekdays(irs$fix_schedule)
irs$float_schedule
weekdays(irs$float_schedule)
diff(c(0, daycount(irs, irs$spot_date, irs$fix_schedule))) * 360
diff(c(0, daycount(fra, irs$spot_date, irs$float_schedule))) * 360
(irs_fixing_dates <- c(as.Date("2023-05-31"), as.Date( "2023-11-30"), 
                       as.Date( "2024-05-30"), as.Date("2024-11-28"), 
                       as.Date( "2025-05-29"), as.Date( "2025-11-28")))
weekdays(irs_fixing_dates)
irs_float_spotdates <- irs_fixing_dates
for (j in seq_along(irs_fixing_dates)) {
  settings$today_date <- irs_fixing_dates[j]
  irs_float_depo <- rh_new_instrument(df_depo, settings)
  irs_float_spotdates[j] <- irs_float_depo$spot_date
}
irs_float_spotdates - irs$float_schedule
