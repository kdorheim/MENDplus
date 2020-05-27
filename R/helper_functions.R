


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



