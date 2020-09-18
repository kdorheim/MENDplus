
# Make  sure the helper functions that are useful in devlopment but not exported  to be  used by
# package users work as  expected.
context('helper_functions')


testthat::test_that("assign_parameters:", {

  testthat::expect_error(assign_parameters(data.frame(x = 'f'), 'data.table::is.data.table(x = dt) is not TRUE'))
  testthat::expect_error(assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 'n'), 'dt[["value"]] is not a numeric or integer vector'))
  x <- assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 11))
  testthat::expect_true(is.null(x))

  # If this function works then there should be an object called g, with the value of 11.
  testthat::expect_equal(g, 11)


})

# Define the initial state values
B = 2; D = 1
P = 10; Q = 0.1
M = 5; EP = 0.00001
EM =  0.00001; IC = 0
Tot = 18.10002

state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)


testthat::test_that("solver:", {

  # Should run
  good <- solver(params = MEND2013_params, time = 0:10, state = state,
         carbon_pools_func = MEND2013_pools, carbon_fluxes_func = MEND2013_fluxes)
  expect_true(is.data.frame(good))

  # Should throw error messages
  expect_error(solver(params = MEND2013_params, time = 0:10, state = state, carbon_pools_func = MEND2013_fluxes, carbon_fluxes_func = MEND2013_fluxes))
  expect_error(solver(params = MEND2013_params, time = 0:10, state = state, carbon_pools_func = MEND2013_pools, carbon_fluxes_func = MEND2013_pools))
  expect_error(solver(params = MEND2013_params, time = 0:10, state = state, carbon_pools_func = list(), carbon_fluxes_func = MEND2013_pools))
  expect_error(solver(params = MEND2013_params[1:10, ], time = 0:10, state = state,
                 carbon_pools_func = MEND2013_pools, carbon_fluxes_func = MEND2013_fluxes))

})


testthat::test_that("modify_fluxes_func", {

  good <- function(){list('F1' = sum)}
  x <- modify_fluxes_func(params = MEND2013_params, state = state, flux_func = MEND2013_fluxes, replace_with = good)
  expect_true(is.function(x))

  test <- function(){list('bad' = sum)}
  expect_error(modify_fluxes_func(params = MEND2013_params, state = state, flux_func = MEND2013_fluxes, replace_with = test),
               msg =  'all elements of replace_with must be within func')

  bad <- function(){list('F1' = 'test')}
  expect_error(modify_fluxes_func(params = MEND2013_params, state = state, flux_func = MEND2013_fluxes,
                                  replace_with = list('F1' = 'test')), msg = 'fluxes input must be a list of functions')

  bad <- list('F1' = 'test')
  expect_error(modify_fluxes_func(params = MEND2013_params, state = state, flux_func = MEND2013_fluxes,
                                  replace_with = list('F1' = 'test')))

})
