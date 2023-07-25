### setup
library(RateHike)

options(digits = 8)

settings <- list(today_date = "2022-11-30",
                 daycount = "act365",
                 interp_what = "zero",
                 interp_how = "linear",
                 anchor = "no",
                 snt = "no",
                 ccy = "EUR")

inst_df <- data.frame(insttype = c("deposit", "fra", "fra"),
                      mat_no = c(6, 6, 6),
                      mat_unit = c("MO", "MO", "MO"),
                      start_no = c(NA, 6, 9),
                      start_unit = c(NA, "MO", "MO"),
                      quote = c(0.1, 0.12, 0.15))

inst_list <- rh_new_instrument_list(inst_df, settings)

curve <- bootstrap(rh_new_curve(settings), inst_list)

### setting
inst_list[[1]]$spot_date
inst_list[[1]]$mat_date

inst_list[[2]]$spot_date
inst_list[[2]]$start_date
inst_list[[2]]$mat_date

inst_list[[3]]$spot_date
inst_list[[3]]$start_date
inst_list[[3]]$mat_date

### the deposit rate
fixing(inst_list[[1]], curve)
100 * get_zero_rates(curve, dates=as.Date("2023-06-02"))
inst_list[[1]]$mat_date - inst_list[[1]]$spot_date
1e8 * (1 + 182/360 * 0.1)
1e8 * exp(182/365 * 0.09890923)
1e8 * (1 + 182/360 * 0.1) - 1e8 * exp(182/365 * get_zero_rates(curve, dates=as.Date("2023-06-02")))

### the first fra rate
fixing(inst_list[[2]], curve)
100 * get_zero_rates(curve, dates=as.Date("2023-12-04"))
inst_list[[2]]$mat_date - inst_list[[2]]$start_date
N <- 1e8 * (1 + 182/360 * 0.1)
105055556 * (1 + 185/360 * 0.12)
1e8 * exp((182+185)/365 * 0.10856425)
N * (1 + 185/360 * 0.12) - 1e8 * exp((182+185)/365 * get_zero_rates(curve, dates=as.Date("2023-12-04")))

### the second fra rate
fixing(inst_list[[3]], curve)
100 * get_zero_rates(curve, dates=as.Date("2024-03-04"))
100 * get_zero_rates(curve, dates=as.Date("2023-09-04"))
inst_list[[3]]$start_date - inst_list[[2]]$start_date
100 * (0.09890923 * 91/185 + 0.10856425 * 94/185)
100 * (get_zero_rates(curve, dates=as.Date("2023-06-02")) * 91/185 + get_zero_rates(curve, dates=as.Date("2023-12-04")) * 94/185)
inst_list[[3]]$start_date - inst_list[[3]]$spot_date
(N <- 1e8 * exp((182+94)/365 * get_zero_rates(curve, dates=as.Date("2023-09-04"))))
inst_list[[3]]$mat_date - inst_list[[3]]$start_date
108166468 * (1 + 182/360 * 0.15)
inst_list[[3]]$mat_date - inst_list[[3]]$spot_date
1e8 * exp(458/365 * 0.12081403)
N * (1 + 182/360 * 0.15) - 1e8 * exp(458/365 * get_zero_rates(curve, dates=as.Date("2024-03-04")))

### interpolation effects

dates <- as.Date(c("2023-06-02", "2023-09-04", "2023-12-04", "2024-03-04"))

params <- expand.grid(interp_what = c("zero", "df", "logdf"), 
                      interp_how= c("linear", "bessel", "hyman", "hyman0"),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)

library(foreach)

zeros <- foreach (paramnr = seq(nrow(params)), .combine = 'rbind') %do% {
  settings$interp_what = params$interp_what[paramnr]
  settings$interp_how = params$interp_how[paramnr]
  curve <- bootstrap(rh_new_curve(settings), inst_list)
  foreach (datenr = seq_along(dates), .combine = 'rbind') %do% {
    data.frame(interp_how = params$interp_how[paramnr],
               interp_what = params$interp_what[paramnr],
               datum = dates[datenr],
               zero = get_zero_rates(curve, dates=dates[datenr]),
               fixing = fixing(inst_list[[3]], curve))
  }
}

library(dplyr)
library(tibble)
library(tidyr)
library(knitr)

tibble(zeros) %>% 
  mutate(zero = num(100*zero, digits=6)) %>%
  pivot_wider(names_from = datum, values_from=zero) %>%
  kable(format="latex", booktabs = TRUE)

tibble(zeros) %>% 
  select(fixing)  %>% 
  mutate(error = 100*fixing - 15) %>%
  unique()



