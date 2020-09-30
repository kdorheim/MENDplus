

#' Define carbon pools for when there are 2 carbon pools
#'
#' \code{MEND2pool_pools} Defines the system of equations that
#' describe the state of the carbon pools modified from  \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#'
#' @param t is for time
#' @param state A numeric vector of the different MEND carbon pool states
#' @param parms A data frame of the parameters, default is set to \code{MEND2pool_params}
#' @param flux_function a function that will return a list of functions that modify how carbon moves between
#' the different MEND carbon pools default is set to \code{MEND2pool_fluxes}
#' @return A list of the state variables
#' @importFrom Rdpack reprompt
#' @family 2013 MEND 2 pool model
#' @family carbon pool functions
#' @export
MEND2pool_pools <- function(t, state, parms = MEND2pool_params, flux_function = MEND2pool_fluxes){

  # Check the inputs
  required_states <- c("P1", "P2", "M","B", "D1", "D2", "Q1", "Q2", "EP1", "EP2", "EM", "IC", "Tot")
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
    expected_fluxes <- c("F1.d1", "F1.d2", "F2.p1", "F2.p2", "F3", "F6.d1", "F6.d2", "F7.d1",
                         "F7.d2", "F8", "F9.ep1", "F9.ep2", "F9.em", "F10.ep1", "F10.ep2", "F10.em")

    assertthat::assert_that(assertthat::has_name(x = fluxes, which = expected_fluxes))
    assertthat::assert_that(all(unlist(lapply(fluxes, is.function))), msg = 'fluxes input must be a list of functions')

    # Define the system of differental equations to describes the
    # changes in the carbon pool states.
    # -----------------------------------------------------------
    # P = particulate organic carbon
    dP1 <- I.p1 + (1 - g.d) * fluxes$F8() - fluxes$F2.p1()
    dP2 <- I.p2 - fluxes$F2.p2()
    # M = mineral-associated organic carbon (MOC)
    dM <- (1 - f.d) * (fluxes$F2.p1() + fluxes$F2.p2()) - fluxes$F3()
    # Q = active layer of MOC
    dQ1 <- fluxes$F6.d1() - fluxes$F7.d1()
    dQ2 <- fluxes$F6.d2() - fluxes$F7.d2()
    # B = microbial biomass carbon
    dB <- E.c1 * fluxes$F1.d1() + E.c2 * fluxes$F1.d2() - fluxes$F8() - (fluxes$F9.em() + fluxes$F9.ep1() + fluxes$F9.ep2())
    # D = dissolved organic carbon
    dD1 <- I.d1  + g.d * fluxes$F8() + fluxes$F3() + (fluxes$F10.em() + fluxes$F10.ep1() + fluxes$F10.ep2()) - fluxes$F1.d1() - (fluxes$F6.d1() - fluxes$F7.d1())
    dD2 <- I.d2 + f.d * (fluxes$F2.p1() +  fluxes$F2.p2()) - fluxes$F1.d2() - (fluxes$F6.d2() - fluxes$F7.d2())
    # EP = carbon stored as extracellular enzymes
    dEP1 <- fluxes$F9.ep1() - fluxes$F10.ep1()
    dEP2 <- fluxes$F9.ep2() - fluxes$F10.ep2()
    # EM = carbon stored as extracellular enzymes
    dEM <- fluxes$F9.em() - fluxes$F10.em()
    # IC = inorganic carbon (CO2)
    dIC <- (1 - E.c1) * fluxes$F1.d1() + (1 - E.c2) * fluxes$F1.d2()
    # Tot = the total carbon pool
    dTot <- I.p1 + I.p2 + I.d1 + I.d2 - (1 - E.c1) * fluxes$F1.d1() - (1 - E.c2) * fluxes$F1.d2()

    # Return outputs
    list(c(dP1, dP2, dM, dB, dD1, dD2, dQ1, dQ2, dEP1, dEP2, dEM, dIC, dTot))
  })


}


#' Define carbon fluxes for when there are 2 carbon pools
#'
#' \code{MEND2pool_fluxes} Defines a set equations that determine the fluxes between the \code{MEND2pool_pools}
#' describe the state of the carbon pools modified from  \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#'
#' @param state A numeric vector of the different MEND carbon pool states
#' @param parms A data frame of the parameters, default is set to \code{MEND2pool_params}
#' @return A list of the state variables
#' @importFrom Rdpack reprompt
#' @family 2013 MEND 2 pool model
#' @family carbon flux functions
#' @export
MEND2pool_fluxes <- function(state, parms){

  # Check inputs
  required_states <- c("P1", "P2", "M", "Q1", "Q2", "B", "D1", "D2", "EP1", "EP2", "EM", "IC", "Tot")
  assertthat::assert_that(assertthat::has_name(x = state, which = required_states))
  assertthat::assert_that(data.table::is.data.table(parms))
  assertthat::assert_that(assertthat::has_name(x = parms, which = c("parameter", "description", "units", "value")))

  # Format the parameters into a vector.
  p        <- parms$value
  names(p) <- parms$parameter


  with(as.list(c(state, p)), {

    fxn_list <- list(
      "F1.d1" = function(){
        # DOC uptake by microbial biomass from the 1st DOC pool.
        V.d1 * B * D1 /( k.d1 + D1)
      },
      "F1.d2" = function(){
        # DOC uptake by microbial biomass from the 2nd DOC pool.
        V.d2 * B * D2 /(k.d2 + D2)
      },
      "F2.p1" = function(){
        # Decomposition of the 1st POC pool.
        V.p1 * EP1 * P1 / (K.p1 + P1)
      },
      "F2.p2" = function(){
        # Decomposition of the 2nd POC pool.
        V.p2 * EP2 * P2 / (K.p2 + P2)
      },
      "F3" = function(){
        # MOAM decomposition
        V.m * EM * M / (K.m + M)
      },
      "F6.d1" = function(){
        # Adsorption of the 1st DOC pool
        K.ads * D1 * (1 - Q1 / Q.max1)
      },
      "F6.d2" = function(){
        # Adsorption of the 2nd DOC pool
        K.ads * D2 * (1 - Q2 / Q.max2)

      },
      "F7.d1"=  function(){
        # Desorption
        K.des * Q1 / Q.max1
      },
      "F7.d2" = function(){
        # Desorption
        K.des * Q2 / Q.max2
      },
      "F8" = function(){
        # Microbial biomass decay
        # why is this multiplied by .25?
        (1 - p.ep - p.ep - p.em) * V.d1 * E.c1 * B * 0.25
      },
      "F9.ep1" = function(){
        # Enzyme production
        P1/( P1 + P2)* p.ep * V.d1 * E.c1 * B * 0.5
      },
      "F9.ep2" = function(){
        P2/(P1 + P2) * p.ep * V.d1 * E.c1 * B * 0.5
      },
      "F9.em" = function(){
        p.em * V.d1 * E.c1 * B * 0.5
      },
      "F10.ep1" = function(){
        # Enzyme decay
        r.ep * EP1
      },
      "F10.ep2" = function(){
        # Enzyme decay
        r.ep * EP2
      },
      "F10.em" = function(){
        r.em * EM
      })

    return(fxn_list)

  })

}



