

MEND_carbon_pools <- function(fluxes){

  # Check the fluxes input
  assertthat::assert_that(is.list(fluxes))
  expected_fluxes <- rep('F', length.out = length(1:12))
  expected_fluxes <- paste0(expected_fluxes, 1:12)
  missing_fluxes  <- !expected_fluxes %in% names(fluxes)
  assertthat::assert_that(sum(missing_fluxes) == 0 , msg = paste0('model configured without: ', paste(expected_fluxes[missing_fluxes], collapse = ', ')))
  assertthat::assert_that(all(unlist(lapply(fluxes, is.function))), msg = 'fluxes input must be a list of functions')


}
