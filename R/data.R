#' (dummy) input data for OIS curve in dataframe format
#'
#' This dataframe can be used to run some examples in the package,
#' see for example \code{\link{rh_new_instrument_list}}
#'
#' @description
#' A data frame with 33 observations and 4 variables. 
#' Dummy data, represents inputs for the ESTR OIS curve.
#'
#' @format
#' \describe{
#'   \item{quote}{market quote}
#'   \item{mat_no}{numeric component of maturity}
#'   \item{mat_unit}{unit for maturity (YR, MO, WK, DY)}
#'   \item{insttype}{instrument type (deposit, ois)}
#' }
"inst_df_ois"

#' (dummy) input data for IBOR curve in dataframe format
#'
#' This dataframe can be used to run some examples in the package,
#' see for example \code{\link{bootstrap}}
#'
#' @description
#' A data frame with 29 observations and 7 variables.
#' Dummy data, represents inputs for the EURIBOR 3 months curve.
#'
#' @format
#' \describe{
#'   \item{quote}{market quote}
#'   \item{mat_no}{numeric component of maturity}
#'   \item{mat_unit}{unit for maturity (YR, MO)}
#'   \item{insttype}{instrument type (deposit, future, irs)}
#'   \item{start_no}{numeric component of start delay, for forward starting instruments}
#'   \item{start_unit}{unit for start delay (MO), for futures}
#'   \item{tenor_m}{tenor, in months, for irs}
#' }
"inst_df_ibor"
