
params <- c('E.c1',
            'E.c2',
            'V.d1',
            'V.d2',
            'k.d1',
            'k.d2',
            'V.p1',
            'K.p1',
            'V.p2',
            'K.p2',
            'V.m',
            'K.m',
            'K.ads',
            'K.des',
            'Q.max1',
            'Q.max2',
            'p.ep',
            'p.em',
            'r.ep',
            'r.em',
            'I.p1',
            'I.p2',
            'I.d1',
            'I.d2',
            'f.d',
            'g.d')

description <- c('carbon use efficiency of D1',
                 'carbon use efficiency of D2',
                 'maximum specific uptake rate of D1 growth of B',
                 'maximum specific uptake rate of D2 for growth of B',
                 'half-saturation constant of uptake of D2 for growth of B mg C/g soil',
                 'half-saturation constant of uptake of D2 for growth of B mg C/g soil',
                 'maximum specific decomposition rate for P1 by EP',
                 'half-saturation constant for decomposition of P1',
                 'maximum specific decomposition rate for P2 by EP',
                 'half-saturation constant for decomposition of P2',
                 'maximum specific decomposition rate for M by EM',
                 'half-saturation constant for decomposition of M',
                 'specific adsorption rate',
                 'desorption rate',
                 'maximum D1 sorption capacity',
                 'maximum D2 sorption capacity',
                 'fraction of mR for production of EP',
                 'fraction of mR for production of EM',
                 'turnover rate of EP',
                 'turnover rate of EM',
                 'input rate of P1',
                 'input rate of P2',
                 'input rate of D1',
                 'input rate of D2',
                 'fraction of decomposed P allocated to D',
                 'fraction of dead B allocated to D')

units <- c(NA,
           NA,
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           'mg C/g soil',
           'mg C/g soil',
           'mgC mgC^-1 h^-1',
           'mgC / g soil',
           'mgC mgC^-1 h^-1',
           'mgC / g soil',
           'mgC mgC^-1 h^-1',
           'mg C/g soil',
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           'mgC / g soil',
           'mgC / g soil',
           NA,
           NA,
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           'mgC mgC^-1 h^-1',
           NA,
           NA)


# Create the data table of the parameters.
MEND2pool_params <- data.table::data.table(parameter = params,
                                           description = description,
                                           units = units, value = NA_real_)

MEND2pool_params[parameter == 'E.c1', ]$value <- 0.75
MEND2pool_params[parameter == 'E.c2', ]$value <-0.25
MEND2pool_params[parameter == 'V.d1', ]$value <- 5e-4
MEND2pool_params[parameter == 'V.d2', ]$value <- 3e-4
MEND2pool_params[parameter == 'k.d1', ]$value <- 0.26
MEND2pool_params[parameter == 'k.d2', ]$value <- 2.6
MEND2pool_params[parameter == 'V.p1', ]$value <- 1.5
MEND2pool_params[parameter == 'V.p2', ]$value <- 1.5
MEND2pool_params[parameter == 'K.p1', ]$value <- 50
MEND2pool_params[parameter == 'K.p2', ]$value <- 5
MEND2pool_params[parameter == 'K.m', ]$value <-  250
MEND2pool_params[parameter == 'K.ads', ]$value <- 0.006
MEND2pool_params[parameter == 'K.des', ]$value <- 0.001
MEND2pool_params[parameter == 'Q.max1', ]$value <- 0.1
MEND2pool_params[parameter == 'Q.max2', ]$value <- 1.7
MEND2pool_params[parameter == 'p.ep', ]$value <- 0.01
MEND2pool_params[parameter == 'p.em', ]$value <- 0.01
MEND2pool_params[parameter == 'r.ep', ]$value <- 1e-3
MEND2pool_params[parameter == 'r.em', ]$value <- 1e-3
MEND2pool_params[parameter == 'I.p1', ]$value <- 2e-5
MEND2pool_params[parameter == 'I.p2', ]$value <- 2e-5
MEND2pool_params[parameter == 'I.d1', ]$value <- 2e-5
MEND2pool_params[parameter == 'I.d2', ]$value <- 2e-5
MEND2pool_params[parameter == 'f.d', ]$value <- 0.5
MEND2pool_params[parameter == 'g.d', ]$value <- 0.5
MEND2pool_params[parameter == 'V.m', ]$value <- 0.1


assertthat::assert_that(sum(is.na(MEND2pool_params$value)) == 0, msg = 'Not all default parameter values have been defined.')

usethis::use_data(x = MEND2pool_params, overwrite = TRUE)
