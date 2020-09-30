## MEND with two P pools
## By @jianqiuz
rm(list = ls ())

library(deSolve)
##solving MEND model with B, D, P, Q, M, EP, EM and IC (CO2)

Decom <- function(times, states, parameters, flux_function) {
  with(as.list(c(states, parameters)), {
    F1d1 <- Vd1 * B * D1 /( kD1 + D1)  ## microbial uptake of D1
    F1d2 <- Vd2 * B * D2 /( kD2 + D2)  ## microbial uptake of D2
    F2p1 <- Vp1* EP1 * P1 / (kP1 + P1) # POM decomposition
    F2p2 <- Vp2* EP2 * P2 / (kP2 + P2)
    F3 <- Vm * EM * M / (kM + M) #MOAM decomposition
    F6d1 <- Kads * D1 *(1- Q1/ Qmax1) ##adsorption
    F6d2 <- Kads * D2 *(1- Q2/ Qmax2) ##adsorption
    F7d1 <- Kdes * Q1/Qmax1  ##desorption
    F7d2 <- Kdes * Q2/Qmax2  ##desorption
    F8 <- (1- pEP-pEP- pEM) * Vd1*Ec1* B *0.25  #microbial biomass decay
    F9ep1 <- P1/(P1+P2)*pEP *Vd1*Ec1* B  *0.5 #enzyme production
    F9ep2 <- P2/(P1+P2)*pEP *Vd1*Ec1* B *0.5
    F9em <- pEM * Vd1*Ec1*B*0.5
    F10ep1 <- rEP * EP1  #enzyme decay
    F10ep2 <- rEP * EP2
    F10em <- rEM *EM

    dP1 <- Ip1 + (1 - gD) * F8- F2p1
    dP2 <- Ip2 - F2p2
    dM <- (1 - fD) * (F2p1+F2p2) - F3
    dB <- Ec1*F1d1+ Ec2* F1d2 - F8 - (F9em + F9ep1 + F9ep2)
    dD1 <- Id1  + gD * F8 + F3 + (F10em + F10ep1+F10ep2)- F1d1 - (F6d1 - F7d1)
    dD2 <- Id2 + fD * (F2p1+F2p2) - F1d2 - (F6d2 - F7d2)
    dQ1 <- F6d1 - F7d1
    dQ2 <- F6d2 - F7d2
    dEP1 <- F9ep1 - F10ep1
    dEP2 <- F9ep2 - F10ep2
    dEM <- F9em - F10em
    dIC <- (1-Ec1)*F1d1+ (1-Ec2)* F1d2  #CO2 fllux
    dTot <- Ip1 + Ip2 + Id1 + Id2 -(1-Ec1)*F1d1 - (1-Ec2)* F1d2
    return(list(c(dP1, dP2, dM, dB, dD1, dD2, dQ1, dQ2, dEP1, dEP2, dEM, dIC, dTot)))
  })
}



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

states <- c(P1=P1, P2=P2, M=M, B=B, D1=D1, D2=D2,Q1=Q1, Q2=Q2,EP1 = EP1, EP2=EP2,EM = EM, IC = IC, Tot= Tot)
parameters <- c(Ec1 = 0.75,
                Ec2=0.25,
                Vd1 = 5e-4,
                Vd2 = 3e-4,
                kD1 = 0.26,
                kD2=2.6,
                Vp1 = 1.5,
                kP1 = 50,
                Vp2=1.5,
                kP2=5,
                Vm = 0.1,
                kM = 250,
                Kads = 0.006,
                Kdes = 0.001,
                Qmax1 = 0.1,
                Qmax2 = 1.7,
                pEP=0.01,
                pEM=0.01,
                rEP=1e-3,
                rEM=1e-3,
                Ip1 = 2e-5,
                Ip2 =2e-5,
                Id1 = 2e-5,
                Id2 =2e-5,
                fD=0.5,
                gD=0.5)



DecomOut = ode(y = states, parms = parameters, times = times, func = Decom)
print("done")
plot(DecomOut)

out <- data.table::melt(data.table::as.data.table(DecomOut),  measure.vars = names(states),
                        variable.name = "variable", value.name = 'value')
out$units <- 'mg C/g soil'

out_dir <- here::here('tests',  'testthat', 'compdata')
dir.create(out_dir, showWarnings = FALSE)
write.csv(out, file.path(out_dir, "RMENDp50y_ip2.csv"), row.names = FALSE)
