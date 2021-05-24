#Scaling parameter for Anne
rm(list=ls())
#The Problem:
#We have data: y0, y25, y50, y75 that we want to scale like so:

#y0*x = y25
#y0*(x^2) = y50
#y0*(x^3) = y75

#No x will fit perfectly, but what's the best we can do?

#----------------------------

#Build y vectors, one for AZ, one for PF

#ORIGINALLY THIS WAS DONE USING RISK, INSTEAD WE WANT TO CONVERT THIS TO "ODDS RATIO" (OR). 
#Technically mind, this isn't really an odds ratio, more just an odds of efficacy

#OR :   efficacy/(1-efficacy)


#AZ ones correspond all to 1 dose: VE against  (i) severe disease, (ii) infection
y0_AZ <- c(0.8/(1-0.8), 0.63/(1-0.63) )

y25_AZ <- c(0.64/(1-0.64), 0.5/(1-0.5))
y50_AZ <- c(0.4/(1-0.4), 0.1/(1-0.1))
y75_AZ <- c(0.2/(1-0.2), 0)
y100_AZ <- y75_AZ


#PF values correspond to VE against (i/ii) severe disease 1dose/2dose (iii/iv) infection 1dose/2dose (v) infectiousness (1 dose)
y0_PF <- c(0.8/(1-0.8), 0.95/(1-0.95), 0.65/(1-0.65), 0.86/(1-0.86))

y25_PF <- c(0.8/(1-0.8), 0.95/(1-0.95), 0.65/(1-0.65), 0.85/(1-0.85))
y50_PF <- c(0.7/(1-0.7), 0.9/(1-0.9), 0.55/(1-0.55), 0.75/(1-0.75))
y75_PF <- c(0.6/(1-0.6), 0.85/(1-0.85), 0.45/(1-0.45), 0.55/(1-0.55))
y100_PF <- y75_PF

#Lastly we do a separate one for infectious values (1 dose) of either AZ or PF
y0_infect <- 0.45/(1-0.45)

y25_infect <- 0.45/(1-0.45)
y50_infect <- 0.2/(1-0.2)
y75_infect <- 0
y100_infect <- y75_infect

#-----------------------------------------


#We're going to consider multiple scenarios and pick the one that works best. 

#First consider a scenario where we ignore the optimistic (y25) values, and treat pessimistic as 75%
#We build a function that takes a trial x, and returns the summed errors between the estimated y0 and true values.

error_x_AZ <- function(x) {
  
  #Allocate memory:
  y50_estimate_AZ <- c(0,0)
  y75_estimate_AZ <- c(0,0)
  
  y50_estimate_AZ <- (x^2)*y0_AZ
  y75_estimate_AZ <- (x^3)*y0_AZ
  
  error_amt <- (sum((abs(y50_estimate_AZ - y50_AZ)))^1  ) #+  sum(abs(y75_estimate_AZ - y75_AZ)) )
  return(error_amt)
}

#Then find minimum
optimize(error_x_AZ, interval = c(0,4))

#This, didn't work very well...
#Optimum x of 1.39292 doesn't really capture it very well. 


#So new scenario, we consider negative as 100%, and we cap it out at risk of 1 (100%)

error_x_AZ <- function(x) {
  
  #Allocate memory:
  y25_estimate_AZ <- c(0,0)
  y50_estimate_AZ <- c(0,0)
  y100_estimate_AZ <- c(0,0)
  
  y25_estimate_AZ <-  (x)*y0_AZ
  y50_estimate_AZ <-  (x^2)*y0_AZ
  y100_estimate_AZ <- (x^4)*y0_AZ
  
  error_amt <- (sum(abs(y50_estimate_AZ - y50_AZ))  )#+  sum(abs(y100_estimate_AZ - y100_AZ)) + +  sum(abs(y25_estimate_AZ - y25_AZ)) )
  return(error_amt)
}

error_x_AZ(2)

optimize(error_x_AZ, interval = c(0,4))

#----------------------------

#Now PF
#Assume the same worst case is 100%, and that we cap risk at 1.

error_x_PF <- function(x) {
  
  #Allocate memory:
  y25_estimate_PF <- c(0,0,0,0)
  y50_estimate_PF <- c(0,0,0,0)
  y100_estimate_PF <- c(0,0,0,0)
  
  y25_estimate_PF <- (x)*y0_PF
  y50_estimate_PF <- (x^2)*y0_PF
  y100_estimate_PF <- (x^4)*y0_PF
  
  error_amt <- (sum( (abs(y50_estimate_PF - y50_PF))^2  ) ) # +  sum(abs(y100_estimate_PF - y100_PF))  +  sum(abs(y25_estimate_PF - y25_PF)) )
  return(error_amt)
}

optimize(error_x_PF, interval = c(0,1))


#----------------------------

#Now infectiousness
error_x_infect <- function(x) {
  
  #Allocate memory:
  y25_estimate_infect <- 0
  y50_estimate_infect <- 0
  y100_estimate_infect <- 0
  
  y25_estimate_infect <- (x)*y0_infect
  y50_estimate_infect <- (x^2)*y0_infect
  y100_estimate_infect <- (x^4)*y0_infect
  
  error_amt <- (sum(abs(y50_estimate_infect - y50_infect))  )  #+  sum(abs(y100_estimate_infect - y100_infect)) +  sum(abs(y25_estimate_infect - y25_infect)) )
  return(error_amt)
}

optimize(error_x_infect, interval = c(0,4))
