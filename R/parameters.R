.onAttach <- function(...) {
  options(rh_params = list(root_find_tol = 1e-12,
                           root_find_interval_radius = 1e-3,
                           spline_update_diff_tol = 1e-12,
                           spline_max_sweeps = 10))
}

#' get RateHike parameter value
#'
#' @param param_name one of the parameters in \code{\link{rh_params}}
#'
#' @return the value of the requested parameter
#' @export
#' @family rh_params
#'
#' @details
#'    if called without argument, all RateHike parameters are returned.
#'
#' @examples
#'    get_rh_param("root_find_tol")
#'    get_rh_param()
get_rh_param <- function(param_name = NULL) {
  if (is.null(param_name))
    getOption("rh_params")
  else
    getOption("rh_params")[[param_name]]
}

#' set RateHike parameter value
#'
#' @inheritParams get_rh_param
#' @param value the new value of the parameter
#'
#' @return this function sets the requested parameter; it invisibly returns the previous version of all RateHike paramters
#' @export
#' @family rh_params
#'
#' @examples
#'    set_rh_param("root_find_tol", 1e-9)
set_rh_param <- function(param_name, value) {
  rh_params = getOption("rh_params")
  rh_params[[param_name]] <- value
  options(rh_params = rh_params)
}
