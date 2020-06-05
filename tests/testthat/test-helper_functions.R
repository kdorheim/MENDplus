
# Make  sure the helper functions that are useful in devlopment but not exported  to be  used by
# package users work as  expected.
context('helper_functions')


testthat::test_that("assign_parameters:", {

  testthat::expect_error(assign_parameters(data.frame(x = 'f'), 'data.table::is.data.table(x = dt) is not TRUE'))
  testthat::expect_error(assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 'n'), 'dt[["value"]] is not a numeric or integer vector'))
  x <- assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 11))
  testthat::expect_true(is.null(x))

  # If this function works then there should be an object called g, with the value of 11.
  testthat::expect_equal(g, 11)


})

