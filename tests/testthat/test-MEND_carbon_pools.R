# A series of tests that makes sure the MEND_carbon_pools function which
# defines the system of differential equations that represents the
# dynamics of the different carbon pools behaves correctly.

testthat::test_that("MEND_carbon_pools catches errors", {
  # Make sure that the function throws errors when function input is
  # formatted incorrectly.
  testthat::expect_error(MEND_carbon_pools('not a list'), 'fluxes is not a list')

  flux_names <- paste0(rep('F', length.out = length(1:12)), 1:12)
  flux_list <- as.list(flux_names)
  names(flux_list) <- flux_names

  missing_fluxes <- flux_list[1:5]
  testthat::expect_error(MEND_carbon_pools(missing_fluxes), 'model configured without: F6, F7, F8, F9, F10, F11, F12')
  testthat::expect_error(MEND_carbon_pools(flux_list), 'fluxes input must be a list of functions')

})

