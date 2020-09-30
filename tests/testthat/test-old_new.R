
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

testthat::test_that("MEND2pool", {

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

  times <- seq(0, 438000, 24) ##per hour 24hour by 365 days =8760  20year=175200

  new <- solver(params = MEND2pool_params,
                time = times,
                state = state,
                carbon_pools_func = MEND2pool_pools,
                carbon_fluxes_func = MEND2pool_fluxes)
  names(new) <- c( "time", "variable", "new_value", "units")

  old <- data.table::as.data.table(read.csv('compdata/RMENDp50y_ip2.csv'))

  expect_equal(dim(old), dim(new))

  expect_equal(old$value, new$new_value)


})

