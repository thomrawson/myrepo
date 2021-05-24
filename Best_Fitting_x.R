#Scaling parameter for Anne

#The Problem:
#We have data: y0, y25, y50, y75 that we want to scale like so:

#y0*x = y25
#y0*(x^2) = y50
#y0*(x^3) = y75

#No x will fit perfectly, but what's the best we can do?

#----------------------------

#Build y vectors, one for AZ, one for PF

#AZ ones correspond all to 1 dose: VE against  (i) severe disease, (ii) infection, (iii) infectiousness
y0_AZ <- c(0.2, 0.37, 0.55 )

y25_AZ <- c(0.36, 0.5, 0.55)
y50_AZ <- c(0.6, 0.9, 0.8)
y75_AZ <- c(0.8, 1, 1)
y100_AZ <- y75_AZ


#PF values correspond to VE against (i/ii) severe disease 1dose/2dose (iii/iv) infection 1dose/2dose (v) infectiousness (1 dose)
y0_PF <- c(0.2, 0.05, 0.35, 0.14, 0.55)

y25_PF <- c(0.2, 0.05, 0.35, 0.15, 0.55)
y50_PF <- c(0.3, 0.1, 0.45, 0.25, 0.8)
y75_PF <- c(0.4, 0.15, 0.55, 0.45, 1)
y100_PF <- y75_PF

 #-----------------------------------------


#We're going to consider multiple scenarios and pick the one that works best. 

#First consider a scenario where we ignore the optimistic (y25) values, and treat pessimistic as 75%
#We build a function that takes a trial x, and returns the summed errors between the estimated y0 and true values.

error_x_AZ <- function(x) {

  #Allocate memory:
  y50_estimate_AZ <- c(0,0,0)
  y75_estimate_AZ <- c(0,0,0)
  
  y50_estimate_AZ <- (x^2)*y0_AZ
  y75_estimate_AZ <- (x^3)*y0_AZ
  
  error_amt <- (sum(abs(y50_estimate_AZ - y50_AZ)) +  sum(abs(y75_estimate_AZ - y75_AZ)) )
  return(error_amt)
}

#Then find minimum
optimize(error_x_AZ, interval = c(0,4))

#This, didn't work very well...
#Optimum x of 1.39292 doesn't really capture it very well. 


#So new scenario, we consider negative as 100%, and we cap it out at risk of 1 (100%)

error_x_AZ <- function(x) {
  
  #Allocate memory:
  y50_estimate_AZ <- c(0,0,0)
  y100_estimate_AZ <- c(0,0,0)
  
  y50_estimate_AZ <- pmin(1, (x^2)*y0_AZ)
  y100_estimate_AZ <- pmin(1, (x^4)*y0_AZ)
  
  error_amt <- (sum(abs(y50_estimate_AZ - y50_AZ)) +  sum(abs(y100_estimate_AZ - y100_AZ)) )
  return(error_amt)
}

error_x_AZ(2)

optimize(error_x_AZ, interval = c(0,4))

#----------------------------

#Now PF
#Assume the same worst case is 100%, and that we cap risk at 1.

error_x_PF <- function(x) {
  
  #Allocate memory:
  y50_estimate_PF <- c(0,0,0,0,0)
  y100_estimate_PF <- c(0,0,0,0,0)
  
  y50_estimate_PF <- pmin(1, (x^2)*y0_PF)
  y100_estimate_PF <- pmin(1, (x^4)*y0_PF)
  
  error_amt <- (sum(abs(y50_estimate_PF - y50_PF)) +  sum(abs(y100_estimate_PF - y100_PF)) )
  return(error_amt)
}

optimize(error_x_PF, interval = c(0,4))


