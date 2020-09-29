# A series of tests that makes sure the MEND2pool_pools function which
# defines the system of differential equations that represents the
# dynamics of the different carbon pools behaves correctly.

context('MEND 2 Pool')

times <- seq(0, 438000, 24) ##per hour 24hour by 365 days =8760  20year=175200

B<-0.5
D1<-0.1
D2<-0.4
P1<-4
P2<-2
Q1<-0.1
Q2<-0.9
M<-10
EP1<-0.00001
EP2<-0.00001
EM<- 0.00001
IC<- 0
Tot<- B+D1+D2+P1+P2+M+Q1+Q2+EP1+EP2+EM

state <- c("P1" = P1,  "P2" = P2,  "M" = M,  "B" = B,  "D1" = D1,  "D2" = D2,  "Q1" = Q1,
           "Q2" = Q2,  "EP1" = EP1,  "EP2" = EP2,  "EM" = EM,  "IC" = IC,  "Tot" = Tot)



testthat::test_that("MEND2pool_fluxes", {

  # Expect errors to be thrown if the input arguments are insufficent.
  expect_error(MEND2pool_fluxes(state = state[1:2], parms = MEND2pool_params))
  expect_error(MEND2pool_fluxes(state = state, parms = MEND2013_params[ ,1:2]))

  # Expect errors to be thrown if ther incorrect inputs are used.
  x <- MEND2pool_fluxes(state = state, parms = MEND2013_params)
  expect_true(all(unlist(lapply(x, is.function))))
  expect_error(x$F1.d1())

  # Make sure that the fluxes return a numeric value.
  x <- MEND2pool_fluxes(state = state, parms = MEND2pool_params)
  y <- c()
  for(i in 1:length(x)){
    y <- c(y, x[[i]]())
  }
  expect_true(all(is.numeric(y)))

})


testthat::test_that("MEND2pool_pools", {

  # Make sure it works with default fluxes defined.
  testthat::expect_error(MEND2pool_pools(t = t, state = state, parms = MEND2pool_params, flux_function =  'not a list'))
  testthat::expect_error(MEND2pool_pools(t = t, state = state[1:2], parms = MEND2pool_params, flux_function = function(){}), 'missing states: M,  B,  D1,  D2,  Q1,  Q2,  EP1,  EP2,  EM,  IC,  Tot')
  testthat::expect_error(MEND2pool_pools(t = t, state = state, parms = MEND2pool_params[ , 1:2], flux_function =  function(){}))
  testthat::expect_error(MEND2pool_pools(t = t, state = rev(state), parms = MEND2pool_params,
                                         flux_function =  MEND2pool_fluxes), 'state pools must be in the following order: P1,  P2,  M,  B,  D1,  D2,  Q1,  Q2,  EP1,  EP2,  EM,  IC,  Tot')

  xx <- MEND2pool_pools(state = state, parms = MEND2pool_params, flux_function = MEND2pool_fluxes)
  # Length of the output should equal the number of states (pools of carbon) and is some number.
  testthat::expect_true(is.numeric(sum(unlist(xx))))
  testthat::expect_equal(length(xx[[1]]), length(state))

  # Make sure the function works when the flux is defined.
  yy <- MEND2pool_pools(state = state, parms = MEND2pool_params, flux_function = MEND2pool_fluxes)
  # Length of the output should equal the number of states (pools of carbon) and is some number.
  testthat::expect_true(is.numeric(sum(unlist(yy))))
  testthat::expect_equal(length(yy[[1]]), length(state))

})


testthat::test_that("MEND2pool", {

  # Quickly solve for mini MEND 2013 run.
  out <- solver(params = MEND2pool_params, time = 0:10, state = state,
                carbon_pools_func = MEND2pool_pools, carbon_fluxes_func = MEND2pool_fluxes)
  testthat::expect_true(data.table::is.data.table(out))

})
