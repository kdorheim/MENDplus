

#' Change parameter values in a parameter table.
#'
#' \code{replace_params} Update a table of MEND parameter values such as \link{default_parameters},
#' with new parameter values. This a helper function to run the mcmc and can also be helpful in
#' a sensitivity analysis
#'
#' @param params a vector of parameters, the vector must include parameter names.
#' @param ptable  a MEND parameter table that will be updated with the values from params. \link{default_parameters}
#' is used as the default paraemter table. However ptable may be set to any data frame that contains the
#' following columns.
#'\describe{
#' \item{parameter}{String character of the default MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from the table 2 of the Wang et al. 2013}}
#' @return A MEND parameter table with updated params values.
#' @importFrom assertthat assert_that
#' @export
replace_params <- function(params, ptable = MENDplus::default_parameters){

  assert_that(!is.null(names(params)), msg = 'params vector must  be  named.')
  assert_that(all(names(params) %in% ptable$parameter),  msg = 'params names not identified as ptable parameter entries.')
  assert_that(all(is.numeric(params)), msg = 'contents of params must be numeric')


  for(index in seq_along(params)){

    ptable[ptable$parameter == names(params)[[index]], ][['value']] <- params[[index]]

  }

  return(ptable)
}



#' Calculate the log prior
#'
#' \code{log_prior} Right now this is set up to calculate the log prior for a set of
#' parameters based on the normal distribtuion.
#' TODO there should probably be some better way to set up and define the hyperparameters.
#'
#' @param params a vector of parameters, the vector must include parameter names.
#' @param ptable  a MEND parameter table that will be updated with the values from params. \link{default_parameters}
#' is used as the default paraemter table. However ptable may be set to any data frame that contains the
#' following columns.
#'\describe{
#' \item{parameter}{String character of the default MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from the table 2 of the Wang et al. 2013}}
#' @return the log prior for the sampled parameters.
#' @export
log_prior <- function(params, ptable = MENDplus::default_parameters){

  lpriors <- mapply(FUN = function(p, pname){

    mean_p <- ptable$value[ptable$parameter == pname]
    stats::dnorm(x = p, mean = mean_p, sd = mean_p * 10, log = T)

  }, p = params, pname = names(params), SIMPLIFY = TRUE)

  return(sum(lpriors))

}


#' Set up the function that will calculate the log posterior.
#'
#' \code{make_logpost} Define the function that will calculate the log
#' posterior. The function returned by \code{make_logpost} will be used in
#' the \code{metrop} to conduct the mcmc.
#' TODO figure out  a better way to document the inputs/
#'
#' @param comp \describe{the comparison data frame it must comain at least two columns,
#' \item{time}{model time}
#' \item{IC}{inorganic carbon, is the the co2 flux from  microbial respiration.}}
#' @param params a named vector of the inital guess of the parameters that will be calibrated in the mcmc.
#' @param t time vector that will be used to run the model.
#' @param state the inital state variables
#' @param carbon_pools_func the function that governs the relationship between the different carbon
#' pools, by default it is set to \code{MEND_carbon_pools}.
#' @param  flux_func the function that governs fluxes, by default is it set to \code{MEND_fluxes}.
#' @param ptable
#'\describe{ A table of MEND parameter calues, by default is it set to \code{default_parameters}. While
#' a different table may be used it must contain the following columns.
#' \item{parameter}{String character of the default MEND parameters.}
#' \item{description}{String character describing the parameter.}
#' \item{units}{String character of the parameter units.}
#' \item{value}{Numeric values taken from the table 2 of the Wang et al. 2013}}
#' @param verbose a TRUE/FALSE indicator to run the function in verbose mode where messages are printed out,
#' by default set to FALSE.
#' @return a function that will return the log posterior.
#' @importFrom assertthat assert_that
#' @export
make_logpost <- function(comp,
                         params,
                         t = seq(0, 1000, 1),
                         state = c(P = 10,  M = 5,  Q = 0.1,  B = 2,  D = 1,
                                   EP = 0.00001,  EM = 0.00001,  IC = 0,  Tot = 18.10002),
                         carbon_pools_func = MEND_carbon_pools,
                         flux_func = MEND_fluxes,
                         ptable = MENDplus::default_parameters,
                         verbose = FALSE){


  assert_that(is.data.frame(comp))
  assert_that(all(c('time', 'IC') %in% names(comp)))
  pnames <- names(params)

  assert_that(is.function(carbon_pools_func), msg = 'must be a function')
  assert_that(is.function(flux_func), msg = 'must be a function')

  function(params){

    names(params) <- pnames

    if(verbose){
      message(paste0(names(params), collapse = ', '))
      message(paste0(params, collapse = ', '))
      message('-----')
    }

    # Update the parameter table so that it includes reflects the new paramter values.
    params_to_use <- replace_params(params, ptable)

    # TODO may be change the solver being used here or make is so that the user has more control of the
    # type of solver that can be set up.
    # Set up the ODE solver to solve MEND governed with MEND's default flux.
    output <- as.data.frame(deSolve::ode(y = state,                     # A vector of the initial size of the C pools
                                         times = t,                     # A vector of the time steps to solve
                                         parms = params_to_use,         # A data.table of parameters (saved as package data)
                                         func = carbon_pools_func,      # This is the  function we want the ODE to solve, the MEND carbon pools
                                         flux_function = flux_func))    # Define the flux functions to use, for now stick with the fluxes from  the 2013 paper.

    # Subset the output results so that the time frames have the same contents. Then
    # for each column of output data calculate MSE between the comparison data and
    # the model output.
    subset_out <- output[output[['time']] %in% comp[['time']], ]
    to_compare <- names(comp)[names(comp) != 'time']

    # If for whatever reason
    if(max(subset_out[['time']]) < max(subset_out[['time']])){

      log_post <- -Inf

    } else {

      MSE <- sapply(to_compare, function(name){
        mean((subset_out[[name]] - comp[[name]])^2)
      }, USE.NAMES = TRUE)

      # Calculate the log likelihood.
      log_likelihood <- log(sum(MSE))

      # Now calcualte the log prior.
      lp <- log_prior(params)

      log_post <- log_likelihood + lp

    }

    return(log_post)

  }
}

