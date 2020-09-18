# A series of tests that makes sure the MEND2013_pools function which
# defines the system of differential equations that represents the
# dynamics of the different carbon pools behaves correctly.

context('2013 MEND model')

B = 2; D = 1
P = 10; Q = 0.1
M = 5; EP = 0.00001
EM =  0.00001; IC = 0
Tot = 18.10002

state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)

testthat::test_that("MEND2013_fluxes", {

  testthat::expect_error(MEND2013_fluxes(state = state[1:2], parms = MEND2013_params))
  testthat::expect_error(MEND2013_fluxes(state = state, parms = MEND2013_params[ ,1:2]))

  x <- MEND2013_fluxes(state = state, parms = MEND2013_params)
  testthat::expect_true(all(unlist(lapply(x, is.function))))

  xx <- x$F1()
  testthat::expect_true(is.numeric(xx))

})


testthat::test_that("MEND2013_pools", {

  # Make sure it works with default fluxes defined.
  state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)
  testthat::expect_error(MEND2013_pools(t = t, state = state, parms = MEND2013_params, flux_function =  'not a list'))
  testthat::expect_error(MEND2013_pools(t = t, state = state[1:2], parms = MEND2013_params, flux_function = function(){}), 'missing states: Q,  B,  D,  EP,  EM,  IC,  Tot')
  testthat::expect_error(MEND2013_pools(t = t, state = state, parms = MEND2013_params[ , 1:2], flux_function =  function(){}))
  testthat::expect_error(MEND2013_pools(t = t, state = rev(state), parms = MEND2013_params, flux_function =  MEND2013_fluxes), 'state pools must be in the following order: P,  M,  Q,  B,  D,  EP,  EM,  IC,  Tot')

  xx <- MEND2013_pools(state = state, parms = MEND2013_params)
  # Length of the output should equal the number of states (pools of carbon) and is some number.
  testthat::expect_true(is.numeric(sum(unlist(xx))))
  testthat::expect_equal(length(xx[[1]]), length(state))

  # Make sure the function works when the flux is defined.
  yy <- MEND2013_pools(state = state, parms = MEND2013_params, flux_function = MEND2013_fluxes)
  # Length of the output should equal the number of states (pools of carbon) and is some number.
  testthat::expect_true(is.numeric(sum(unlist(yy))))
  testthat::expect_equal(length(yy[[1]]), length(state))

})


testthat::test_that("MEND2013", {

  # Quickly solve for mini MEND 2013 run.
  out <- solver(params = MEND2013_params, time = 0:10, state = state,
                carbon_pools_func = MEND2013_pools, carbon_fluxes_func = MEND2013_fluxes)
  testthat::expect_true(data.table::is.data.table(out))

})
