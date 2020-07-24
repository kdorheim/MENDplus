# Make  sure the helper mcmc functions work properly.
context('mcmc_functions')

test_that('replace_params', {

  params <- c('x' = 1)
  expect_error(replace_params(params))

  params <- c(1)
  expect_error(replace_params(params), 'params vector must  be  named.')

  params <- c('Q.max' = 'h')
  expect_error(replace_params(params), regexp = 'contents of params must be numeric')

  # Make sure it works with the default parameter table.
  params <- c('Q.max' = 40, 'E.c' = 100)
  out <- replace_params(params)
  expect_equal(sum(out != default_parameters, na.rm = TRUE), 2)

  # Make sure it can work with a custom parameter table.
  sub <- default_parameters[1:5, ]
  params <- c('K.p' = 100,  'K.m' = 100)
  out <- replace_params(params, sub)
  expect_equal(dim(out), dim(sub))
  expect_equal(sum(out != sub, na.rm = TRUE), length(params))

})


test_that('log_prior', {

  params <- c('Q.max' = 40, 'E.c' = 100)
  out <- log_prior(params)
  expect_true(is.numeric(out))
})
