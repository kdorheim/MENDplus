# A series of tests that makes sure the MEND_carbon_pools function which
# defines the system of differential equations that represents the
# dynamics of the different carbon pools behaves correctly.

context('original MEND model')

state <- c(2e+00, 1e+00, 1e+01, 1e-01, 5e+00, 1e-05, 1e-05, 4)
names(state) <- c("B", "D", "P", "Q", "M", "EP", "EM", 'T')
t <- 1


testthat::test_that("MEND_fluxes", {

  testthat::expect_error(MEND_fluxes(state = state[1:2], parms = default_parameters))
  testthat::expect_error(MEND_fluxes(state = state, parms = default_parameters[ ,1:2]))

  x <- MEND_fluxes(state = state, parms = default_parameters)
  testthat::expect_true(all(unlist(lapply(x, is.function))))

  xx <- x$F1()
  testthat::expect_true(is.numeric(xx))

})


testthat::test_that("MEND_carbon_pools", {

  testthat::expect_error(MEND_carbon_pools(t = t, state = state, parms = default_parameters, flux_function =  'not a list'))
  testthat::expect_error(MEND_carbon_pools(t = t, state = state[1:2], parms = default_parameters, flux_function = function(){}))
  testthat::expect_error(MEND_carbon_pools(t = t, state = state, parms = default_parameters[ , 1:2], flux_function =  function(){}))

  # Make sure it works with default fluxes defined.
  xx <- MEND_carbon_pools(state = state, parms = default_parameters)
  # Length of the output should equal the number of states (pools of carbon) and is some number.
  testthat::expect_true(is.numeric(sum(unlist(xx))))
  testthat::expect_equal(length(xx[[1]]), length(state))

  # Make sure the function works when the flux is defined.
  yy <- MEND_carbon_pools(state = state, parms = default_parameters, flux_function = MEND_fluxes)
  # Length of the output should equal the number of states (pools of carbon) and is some number.
  testthat::expect_true(is.numeric(sum(unlist(yy))))
  testthat::expect_equal(length(yy[[1]]), length(state))

})
