#' Define the stanadard MEND fluxes.
#'
#' \code{MEND_fluxes} Define a system of equations that
#' describe the state of the fluxes between the pools from Wang et al. 2013.
#' By retruning a list of named functions. We will be able to modify how
#' the fluxes are defined and therefore used in \code{MEND_carbon_pools}.
#'
#' @param state A numeric vector of the different MEND carbon pool states.
#' @param parms A data frame of the parameters.
#' @return A list of functions that define
#' @export
MEND_fluxes <- function(state, parms){

  # Check inputs
  assertthat::assert_that(assertthat::has_name(x = state, which = c("B", "D", "P", "Q", "M", "EP", "EM")))
  assertthat::assert_that(data.table::is.data.table(parms))
  assertthat::assert_that(assertthat::has_name(x = parms, which = c("parameter", "description", "units", "value")))

  # Format the parameters into a vector.
  p        <- parms$value
  names(p) <- parms$parameter


  with(as.list(c(state, p)), {

    fxn_list <- list(
      "F1" = function(){
        # DOC uptake by microbial biomass.
        (1/E.c) * (V.d + m.r) * B *D /( K.d + D)
        },
      "F2" = function(){
        # POC decomposition
        V.p * EP * P / (K.p + P)
        },
      "F3" = function(){
        # Break down of mineralized organic carbon
        V.m * EM * M / (K.m + M)
        },
      "F4" = function(){
        # Microbial respiration from biomass growth
        (1/E.c -1) * V.d * B * D /( K.d + D)
        },
      "F5" = function(){
        # Metabolic/maintenance microbial respiration
        (1/E.c -1) * m.r * B * D /( K.d + D)
        },
      "F6" = function(){
        # Adsorption of DOC to mineral-associated organic carbon
        K.ads * D *(1- Q/ Q.max)
        },
      "F7" = function(){
        # Desorption of mineral-associated organic carbon to DOC
        K.des * Q/Q.max
        },
      "F8" = function(){
        # Carbon loss due to microbial biomass mortality
        (1- p.ep- p.em) * m.r * B
        },
      "F9.ep" = function(){
        # Enzyme production
        p.ep * m.r *B
        },
      "F9.em" = function(){
        # Enzyme production
        p.em * m.r * B },
      "F10.ep" = function(){
        # Enzyme turn over
        r.ep * EP },
      "F10.em" = function(){
        # Enzyme turn over
        r.em * EM })

    return(fxn_list)

  })

}
