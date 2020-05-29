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
 # assertthat::assert_that(is.numeric(params) & is.character(class(names(params))))
  assertthat::assert_that(data.table::is.data.table(parms))
  assertthat::assert_that(assertthat::has_name(x = parms, which = c("parameter", "description", "units", "value")))

  # Format the parameters into a vector.
  p        <- parms$value
  names(p) <- parms$parameter


  with(as.list(c(state, p)), {

    fxn_list <- list("F1" = function(){ (1/E.c) * (V.d + m.r) * ((D * B)/(K.d + D)) },
                     "F2" = function(){ (V.p * EP * P ) / (K.p + P) },
                     "F3" = function(){ (V.m * EM * M) / (K.m + M) },
                     "F4" = function(){ ((1/E.c) - 1) * ((V.d  * B * D)/(K.d + D)) },
                     "F5" = function(){ ((1/E.c) - 1)*((m.r * B * D)/(K.d + D)) },
                     "F6" = function(){ K.ads * (1 - (Q/Q.max) * D) },
                     "F7" = function(){ K.des * (Q/Q.max) },
                     "F8" = function(){ ((1 - p.ep - p.em) * m.r) * B },
                     "F9" = function(){ p.ep * m.r * B },
                     "F10" = function(){ p.em * m.r * B },
                     "F11" = function(){ r.ep * EP },
                     "F12" = function(){ r.em * EM })

    return(fxn_list)

  })

}
