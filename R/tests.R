#' @export
benchmark.rh_curve <- function(curve, 
                               bench_zeros = NULL, 
                               bench_dfs = NULL,
                               pillars = -1) {
  if (!is.null(bench_zeros))
    diff_zeros <- bench_zeros - get_zero_rates(curve, pillars = pillars)
  else
    diff_zeros <- NULL
  if (!is.null(bench_dfs))
    diff_dfs <- bench_dfs - get_discount_factors(curve, pillars = pillars)
  else
    diff_dfs <- NULL
  structure(list(zeros = diff_zeros, dfs = diff_dfs), class = "rh_benchmark")
}

#' @export
leave_one_out.rh_instrument_list <- function(inst_list, settings, indices) {
  loo_diff <- rep(NA, length(indices))
  names(loo_diff) <- indices
  curve_new <- rh_new_curve(settings)
  curve_full <- bootstrap(curve_new, inst_list)
  for (i in seq_along(indices)) {
    inst_list_loo <- inst_list
    inst_list_loo[[indices[i]]] <- NULL
    check_date <- inst_list[[indices[i]]]$mat_date
    curve_loo <- bootstrap(curve_new, inst_list_loo)
    loo_diff[i] <- get_discount_factors(curve_full, dates = check_date) -
                         get_discount_factors(curve_loo, dates = check_date)
  }
  structure(list(diff = loo_diff), class = "rh_leave_one_out")
}

#' @export
print.rh_benchmark <- function(x, 
                               print_all = TRUE, 
                               print_maxabs = TRUE,
                               ...) {
  rh_print_helper(x$zeros, print_all, print_maxabs, "zeros benchmark")
  rh_print_helper(x$dfs, 
                  print_all, 
                  print_maxabs, 
                  "discount factors benchmark")
}

#' @export
print.rh_leave_one_out <- function(x, 
                                   print_all = TRUE, 
                                   print_maxabs = TRUE,
                                   ...) {
  rh_print_helper(x$diff, print_all, print_maxabs, "leave-one-out")
}

#' @export
print.rh_roundtrip <- function(x, 
                               print_all = TRUE, 
                               print_maxabs = TRUE,
                               ...) {
  rh_print_helper(x$diff, print_all, print_maxabs, "round-trip")
}

rh_print_helper <- function(diff_v, print_all, print_maxabs, label) {
  if (is.null(diff_v))
    message(paste("No", label, "information in input."))
  else {
    if (print_all) {
      message(paste(label, "testing:"))
      print(diff_v)
      message(" ")
    }
    if (print_maxabs) {
      message(paste("maximum absolute error for", label, "testing:"))
      print(max(abs(diff_v[!is.na(diff_v)])))
      message(" ")
    }
  }
}

#' @export
roundtrip.rh_curve <- function(curve, inst_list) {
  rt_diff <- sapply(inst_list, fixing, curve = curve) - 
                                   sapply(inst_list, "[[", "quote")
  structure(list(diff = rt_diff), class = "rh_roundtrip")
}

