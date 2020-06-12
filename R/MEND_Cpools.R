
#' Carbon pools
#'
#' \code{MEND_carbon_pools} Define a system of equations that
#' describe the state of the carbon pools from Wang et al. 2013.
#'
#' @param t is for time
#' @param state A numeric vector of the different MEND carbon pool states.
#' @param parms A data frame of the parameters.
#' @param flux_function a function that will return a list of functions that modify how carbon moves between
#' the different MEND carbon pools the default is set to \code{MEND_fluxes} to reproduce the 2013 Wang et. al
#' paper results. This function must beable to use the same state and parms as inputs.
#' @return A list of the state variables.
#' @export
MEND_carbon_pools <- function(t, state, parms, flux_function = MEND_fluxes){

  # Check the inputs
  required_states <- c("P", "M", "Q", "B", "D", "EP", "EM", "IC", "Tot")
  missing_states  <- required_states[!required_states %in% names(state)]
  assertthat::assert_that(length(missing_states) == 0, msg = paste0('missing states: ', paste(missing_states, collapse = ',  ')))
  assertthat::assert_that(all(required_states  == names(state)), msg = paste0('state pools must be in the following order: ', paste(required_states, collapse = ',  ')))
  assertthat::assert_that(data.table::is.data.table(parms))
  assertthat::assert_that(assertthat::has_name(x = parms, which = c("parameter", "description", "units", "value")))
  assertthat::assert_that(is.function(flux_function))


  # Format the parameters into a vector.
  p        <- parms$value
  names(p) <- parms$parameter

  with(as.list(c(state, p)),{

    # Define the fluxes and check to make sure they meet the requirements to be used
    # by the MEND carbon pool structure.
    fluxes <- flux_function(state = state, parms = parms)

    expected_fluxes <- rep('F', length.out = length(1:8))
    expected_fluxes <- c(paste0(expected_fluxes, 1:8), 'F9.ep', 'F9.em', 'F10.ep', 'F10.em')
    assertthat::assert_that(assertthat::has_name(x = fluxes, which = expected_fluxes))
    assertthat::assert_that(all(unlist(lapply(fluxes, is.function))), msg = 'fluxes input must be a list of functions')

    # Define the system of differental equations to describes the
    # changes in the carbon pool states.
    # -----------------------------------------------------------
    # P = particulate organic carbon
    # M = mineral-associated organic carbon (MOC)
    # Q = active layer of MOC
    # B = microbial biomass carbon
    # D = dissolved organic carbon
    # EP = carbon stored as extracellular enzymes
    # EM = carbon stored as extracellular enzymes
    # Tot = the total carbon pool
    # IC = inorganic carbon (CO2)

    dP <- I.p + (1 - g.d) * fluxes$F8() - fluxes$F2() # The change in the pool size of the POC
    dM <- (1 - f.d) * fluxes$F2() - fluxes$F3()       # The change in themineral assoicated OC
    dQ <- fluxes$F6() - fluxes$F7()                   # The change in the active MOC through adsorption and desorption
    dB <- fluxes$F1() - (fluxes$F4() + fluxes$F5()) - fluxes$F8() - (fluxes$F9.ep() + fluxes$F9.em()) # The change in microbial biomass
    dD <- I.d + f.d * fluxes$F2() + g.d * fluxes$F8() + fluxes$F3() + (fluxes$F10.em() + fluxes$F10.ep())- fluxes$F1() - (fluxes$F6() - fluxes$F7()) # The change in the DOC
    dEP <- fluxes$F9.em() - fluxes$F10.ep() # The change in the pool size of the EP extracellular enzymes
    dEM <- fluxes$F9.em() - fluxes$F10.em() # The change in the pool size of the EM extracellular enzymes
    dIC <- fluxes$F4() + fluxes$F5()        # Total change in the carbon pool
    dTot <- I.p + I.d - (fluxes$F4() + fluxes$F5())  # Total change in the carbon pool

    # Return outputs
    list(c(dP, dM, dQ, dB, dD, dEP, dEM, dIC, dTot))
  })
}

