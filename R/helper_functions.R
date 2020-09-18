


#' Assign the paramter values
#'
#' Define parameters based on the default parameter table. We may want to
#' readdress this method but for now it will have to do. What would be ideal
#' is to figure out a way to determine which parameters are used in a particular
#' model configuration and which ones are not. For now this is an internal function.
#'
#' @param dt data.table containing the following columns: parameter, value, and units.
#' @return Nothing, but has defined the elements of the parameter column as objects in
#' the environment.
#' @noRd
assign_parameters <- function(dt){

  assertthat::assert_that(data.table::is.data.table(dt))
  assertthat::has_name(x = dt, which = c('parameter', 'value', 'units'))
  assertthat::assert_that(is.character(dt[['parameter']]))
  assertthat::assert_that(is.numeric(dt[['value']]))

  mapply(assign, x = dt$parameter, value = dt$value, inherits = TRUE)

  # Return nothing
  invisible(NULL)
}


#' Sovle for a MENDplus model set up.
#'
#' Based on user defined carbon pools and flux functions solve a system of equations.
#'
#' @param params data.table containing the following columns: parameter, value, and units.
#' @param time a vector of the time setps
#' @param state a vector of the intial state values, must be named
#' @param carbon_pools_func a function defining the carbon  pools
#' @param carbon_fluxes_func a function defining the carbon fluxes between pools
#' @param ... additional arguments that can be read into \code{deSolve::ode}
#' @return a long formatted data.table of the simulation results
#' @importFrom assertthat assert_that
#' @export
#' @family helper function
solver <- function(params, time, state, carbon_pools_func, carbon_fluxes_func, ...){

  # Make sure that the pools and fluxes are being read in as functions and that
  # the have not been used in place of one another.
  assert_that(is.function(carbon_pools_func))
  req_args  <-c('t', 'state', 'parms', 'flux_function')
  pool_args <- as.vector(names(formals(carbon_pools_func)))
  missing   <- req_args[!req_args %in% pool_args]
  assert_that(length(missing) ==  0,  msg = cat('carbon_pool_func missing required arguments: ', paste(missing, collapse = ', ')))


  assert_that(is.function(carbon_fluxes_func))
  req_args  <- c("state", "parms")
  flux_args <- as.vector(names(formals(carbon_fluxes_func)))
  missing   <- req_args[!req_args %in% flux_args]
  assert_that(length(missing) == 0, msg = cat('carbon_flux_func missing required arguments: ', paste(missing, collapse = ', ')))
  assert_that(all(!grepl(pattern = 'flux', flux_args)), msg = 'check carbon_fluxes_func, make sure the pool function is not being used in stead.')


  rslt  <- deSolve::ode(y = state,      # initial state estimates
                        times = time,  # times to solve over
                        parms = params,  # parameter table
                        func = carbon_pools_func, # the pool represenation we are intrested in
                        flux_function = carbon_fluxes_func,  # the flux represenation we are intrested in
                        ...) # extra ode arguments

  # Now format the results into a nice data frame instead of a wide  matrix.
  out <- data.table::melt(data.table::as.data.table(rslt),  measure.vars = names(state),
                          variable.name = "variable", value.name = 'value')
  out$units <- 'mg C/g soil'
  return(out)

}


#' Alter a MEND function
#'
#' Helper function that can be used to modify \code{MEND2013_fluxes}.
#' To help test out different structural configurations of MEND.
#'
#' @param params data.table containing the following columns: parameter, value, and units
#' @param state a vector of the intial state values, must be named
#' @param flux_func a MEND flux object such as the \code{MEND2013_fluxes}
#' @param replace_with a list containing the new elements of the functions
#' @importFrom assertthat assert_that
#' @export
#' @family helper function
modify_fluxes_func <- function(params, state, flux_func, replace_with){

  # Generate the fluxes
  fluxes   <- flux_func(state, params)
  new_flux <- replace_with()

  # Test the inputs
  assert_that(is.function(flux_func))
  assert_that(all(unlist(lapply(fluxes, is.function))))
  assert_that(is.character(names(fluxes)))

  assert_that(is.function(replace_with), msg = 'replace_with is not a function')
  assert_that(is.character(names(new_flux)))
  assert_that(all(names(new_flux) %in% names(fluxes)), msg = 'all elements of replace_with must be within func')
  assert_that(is.list(new_flux))
  assert_that(all(unlist(lapply(new_flux, is.function))))


  # Subset the flux function so that it only contains default fluxes that should not be replaced.
  fluxes <- fluxes[!names(fluxes) %in% names(replace_with)]

  # Add the new elements and read in the parameters.
  p        <- params$value
  names(p) <- params$parameter

  with(as.list(c(state, p)), {

    out <- function(state, parms){

      # new_fluxes <- list(
      #   'F1' = function(){
      #     # DOC uptake by microbial biomass.
      #     (1/E.c) * (V.d + m.r) * B * D /( K.d + B)
      #   })

      append(fluxes, new_flux)
      }

    return(out)

  })

}


