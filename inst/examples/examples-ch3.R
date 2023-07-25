library(RateHike)

### 3.2.5 day count conventions
start_date <- as.Date("2022-11-30")
mat_date <- as.Date("2023-05-31")

obj_act365 <- structure(list(), class = "rh_act365")
obj_act360 <- structure(list(), class = "rh_act360")
obj_30e360 <- structure(list(), class = "rh_30e360")

daycount(obj_act365, start_date, mat_date) * 365
daycount(obj_act365, start_date, mat_date) 
daycount(obj_act360, start_date, mat_date) 
daycount(obj_30e360, start_date, mat_date) 

### 3.2.6 compounding
r <- 0.06
T <- 0.5
1 + r*T
(1 + r/12)^(12*T)
exp(r*T)
