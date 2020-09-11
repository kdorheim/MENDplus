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

test_that('make_logpost', {

  # Make up some comparison data
  comp <- data.frame(time = 0:10,
                     IC = seq(from = 0, to = 0.01, length = length(0:10)))

  # Define some parameters to calibrate.
  params1 <- c('K.p' = 25,'K.m' = 125, 'K.d' = 0.13)

  # Make sure that the make_logpost returns a function.
  log_post <- make_logpost(comp = comp, t = 0:11, params = params1)
  expect_true(is.function(log_post))

  # Calculate the log posterior for two different parmetizations and make sure the answer
  # returned by them are different from one another.
  out1 <- log_post(params1)
  expect_true(is.numeric(out1))

  params2 <- c('K.p' = 60,'K.m' = 70, 'K.d' = 0.1)
  out2 <- log_post(params2)
  expect_true(out1 != out2)

})

test_that('make_logpost works with mcmc', {

  # Make up some comparison data
  comp <- data.frame(time = 0:5,
                     IC = seq(from = 0, to = 0.01, length = length(0:5)))

  # Define some parameters to calibrate.
  params1 <- c('K.p' = 25,'K.m' = 125, 'K.d' = 0.13)

  set.seed(42)
  # Make sure that the make_logpost returns a function.
  log_post <- make_logpost(comp = comp, t = 0:5, params = params1)
  nbatch <- 3
  out <- mcmc::metrop(obj = log_post, initial = params1, nbatch = nbatch)
  expect_equal(nrow(out$batch), nbatch)

})
