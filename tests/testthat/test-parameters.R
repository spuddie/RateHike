test_that("parameters", {
  rh_params <- list(root_find_tol = 1e-12,
                    root_find_interval_radius = 1e-3,
                    spline_update_diff_tol = 1e-12,
                    spline_max_sweeps = 10)
  rh_params_test <- rh_params
  expect_equal(get_rh_param(), rh_params_test)
  set_rh_param('root_find_tol', 1)
  rh_params_test['root_find_tol'] <- 1
  expect_equal(get_rh_param(), rh_params_test)
  set_rh_param('root_find_interval_radius', 1)
  rh_params_test['root_find_interval_radius'] <- 1
  expect_equal(get_rh_param(), rh_params_test)
  expect_equal(get_rh_param('root_find_tol'), rh_params_test$root_find_tol)
  expect_equal(get_rh_param('root_find_interval_radius'), 
               rh_params_test$root_find_interval_radius)
  expect_equal(get_rh_param('spline_update_diff_tol'), 
               rh_params_test$spline_update_diff_tol)
  expect_equal(get_rh_param('spline_max_sweeps'), 
               rh_params_test$spline_max_sweeps)
  set_rh_param('spline_update_diff_tol', 1)
  rh_params_test['spline_update_diff_tol'] <- 1
  expect_equal(get_rh_param(), rh_params_test)
  set_rh_param('spline_max_sweeps', 1)
  rh_params_test['spline_max_sweeps'] <- 1
  expect_equal(get_rh_param(), rh_params_test)
  options(rh_params = rh_params)
})
