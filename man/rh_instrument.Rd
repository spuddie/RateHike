\name{rh_instrument}
\alias{rh_instrument}
\title{Object of class rh_instrument}
\description{Represents a financial instrument. Available instrument
  types are: deposit, ois, fra, future and irs.}
\format{
\describe{
  \item{quote}{market quote of the instrument}
  \item{mat_no}{numeric component of maturity}
  \item{mat_unit}{unit in which maturity is described (YR, MO, WK, DY)}
  \item{today_date}{today's date}
  \item{ccy}{currency}
  \item{calendar}{holiday calendar}
  \item{spot_date}{spot date}
  \item{mat_date}{maturity date}
  \item{payment_schedule*}{schedule of payment dates (for ois)}
  \item{fixed_schedule*}{schedule of fixed payment dates (for irs)}
  \item{float_schedule*}{schedule of floating payment dates (for irs)}
  \item{start_no*}{numeric component of start period (for fra and future)}
  \item{start_unit*}{unit in wich the start period is described (YR, MO, WK, DY; for fra and future)}
  \item{start_date*}{forward starting date (for fra and future)}
  \item{convexity_adj*}{structure with the elements of the convexity adjustment (for future):
    \describe{
      \item{mean_rev_speed}{the mean reversion speed}
      \item{volatility}{the volatility}
    }}
  }
}
\details{
consists of the fields described above;
  fields marked with a * only belong to certain instrument types.
  Created by \code{\link{rh_new_instrument}}.
}

