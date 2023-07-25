\name{rh_params}
\alias{rh_params}
\title{RateHike parameters}
\format{
\describe{
   \item{root_find_tol}{the tolerance for root finding; default 1e-12}
   \item{root_find_interval_radius}{the width of the interval for root finding; default 1e-3}
   \item{spline_update_diff_tol}{the tolerance for the maximum
     difference in discount factors when sweeping through the
     instrument list multiple times for bootstrapping a spline
     interpolated curve; default 1e-12}
   \item{spline_max_sweeps}{the maximum number of sweeps for
     bootstrapping a spline interpolated curve; default 10}
}
}
\description{
RateHike parameters
}
\details{
  These parameters can be accessed or changed using the functions
\code{\link{get_rh_param}} and \code{\link{set_rh_param}}.
}
