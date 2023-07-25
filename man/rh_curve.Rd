\name{rh_curve}
\alias{rh_curve}
\title{Object of class rh_curve}
\description{
  Represents a yield curve.
}
\format{
\describe{
  \item{today_date*}{today's date}
  \item{interp_class*}{information about the interpolation scheme}
  \item{anchor*}{re-anchoring choice}
  \item{synth_inst*}{synthetics choice}
  \item{discount_curve*}{exogeneous discounting curve; NULL or missing when discounting endogeneously}
  \item{ccy*}{currency}
  \item{calendar*}{holiday calendar}
  \item{spot_date*}{spot date}
  \item{times}{year fractions of the pillars}
  \item{interpolate}{structure with interpolation information:
  \describe{
    \item{dim_c}{dimension of the coefficients: 2 for linear interpolation, 4 for spline interpolation}
    \item{dim_t}{dimension of the times vector: total number of pillars}
    \item{dim_d}{dimension of the data: number of pillars already added}
    \item{coeff}{coefficient matrix of size (dim_c, dim_t)}
  }}
\item{bootstrap}{information about the bootstrapping:
  \describe{
    \item{max_iter}{maximum number of sweeps through the instruments: 1 for linear interpolation, 10 for spline interpolation}
    \item{func_evals}{number of fixings performed while bootstrapping}
    \item{iterations}{effective number of sweeps through the instruments}
    \item{update_diff}{the difference between the pillar values between sweeps (not used for linear interpolation)}
  }}
}
}
\details{
consists of the fields described above;
  fields marked with a * are created by \code{\link{rh_new_curve}}
  other fields are added by \code{\link{bootstrap}}. Also an output of
  the function \code{\link{re_anchor}}.
}

