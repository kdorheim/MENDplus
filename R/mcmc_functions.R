

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
replace_params <- function(params, ptable = default_parameters){

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
#' @importFrom assertthat assert_that
#' @export
log_prior <- function(params, ptable = default_parameters){

  lpriors <- mapply(FUN = function(p, pname){

    mean_p <- ptable$value[ptable$parameter == pname]
    dnorm(x = p, mean = mean_p, sd = mean_p * 10, log = T)

  }, p = params, pname = names(params), SIMPLIFY = TRUE)

  return(sum(lpriors))

}
