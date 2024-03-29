% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-objects.R
\name{rh_new_curve}
\alias{rh_new_curve}
\title{creates a curve based on the settings}
\usage{
rh_new_curve(settings)
}
\arguments{
\item{settings}{a structure with the following fields:
\describe{
   \item{interp_how}{interpolation method (linear, bessel, hyman, hyman0)}
   \item{interp_what}{interpolation object(zero, df, logdf)}
   \item{spline_corr}{apply spline correction (yes, no); can be missing}
   \item{daycount}{a daycount convention: act365, act360, 30e360}
   \item{today_date}{today's date yyyy-mm-dd}
   \item{anchor}{re-anchoring method (no, date, ontn, 11); can be missing}
   \item{snt}{which synthetic instruments to add (no, ois); can be missing}
   \item{discount_curve}{name of an exogeneous discount curve, if required}
   \item{ccy}{the currency, EUR or GBP are currently supported}
   \item{spot_date}{the curve spot date yyyy-mm-dd, can be missing}
}}
}
\value{
an object of class \code{\link{rh_curve}}.
}
\description{
creates a curve based on the settings
}
\examples{
  settings <- list(today_date = "2022-02-22",
                   daycount = "act365",
                   interp_what = "df",
                   interp_how = "linear",
                   anchor = "no",
                   ccy = "EUR")
  curve <- rh_new_curve(settings)
}
