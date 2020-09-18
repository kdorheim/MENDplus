
context('old new test')


testthat::test_that("MEND2013", {

  B = 2; D = 1
  P = 10; Q = 0.1
  M = 5; EP = 0.00001
  EM =  0.00001; IC = 0
  Tot = 18.10002

  state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)

  new <- solver(params = MEND2013_params,
                time = seq(0, 1e3, by = 0.1),
                state = state,
                carbon_pools_func = MEND2013_pools,
                carbon_fluxes_func = MEND2013_fluxes)

  old <- read.csv('compdata/MEND2013_default.csv')

  expect_equal(dim(old), dim(new))
  expect_equal(old$value, new$value)


})


