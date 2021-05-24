#Plot the OR (Vaccine Efficacy Odds) efficacy/(1-efficacy)

#SPECIFICALLY,
#we only minimise the error at the central (50%) mark.

rm(list=ls())

require(tidyverse)

#The Problem:
#We have data: y0, y25, y50, y75 that we want to scale like so:

#y0*x = y25
#y0*(x^2) = y50
#y0*(x^3) = y75

#No x will fit perfectly, but what's the best we can do?

#----------------------------

#Build y vectors, one for AZ, one for PF, one for infectiousness combined


#Lastly we do a separate one for infectious values (1 dose) of either AZ or PF
y0_infect <- 0.45/(1-0.45)

y25_infect <- 0.45/(1-0.45)
y50_infect <- 0.33/(1-0.33)
y75_infect <- 0.2/(1-0.2)
y100_infect <- y75_infect

#-----------------------------------------




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
  
  y50_estimate_infect_probability <- y50_estimate_infect/(y50_estimate_infect+1)
  y50_infect_probability <- y50_infect/(y50_infect+1)
  
  y25_estimate_infect_probability <- y25_estimate_infect/(y25_estimate_infect+1)
  y25_infect_probability <- y25_infect/(y25_infect+1)
  
  y100_estimate_infect_probability <- y100_estimate_infect/(y100_estimate_infect+1)
  y100_infect_probability <- y100_infect/(y100_infect+1)
  
  error_amt <- sum(   (abs(y50_estimate_infect_probability - y50_infect_probability))^2 )  +  sum((abs(y100_estimate_infect - y100_infect))^2 ) +  sum(   (abs(y25_estimate_infect - y25_infect))^2    ) 
  return(error_amt)
}

optim_infect <- optimize(error_x_infect, interval = c(0,1))
optim_x_infect <- optim_infect$minimum
optim_x_infect_error <- optim_infect$objective


#Okay, now we want to plot it
######
#AZ
######

percent_mark <- seq(0,100, length.out = 101)

# Infectiousness


infection_efficacy <- (0.45/(1-0.45))*(optim_x_infect^(percent_mark/25))
#Convert back to probability:
infection_efficacy <- infection_efficacy/(infection_efficacy+1)

Table_Data <- data.frame(percent_mark = c(0, 25, 50, 100), infection_efficacy = c(0.45, 0.45, 0.33, 0.2))
Mapped_Data <- data.frame(percent_mark = percent_mark, infection_efficacy = infection_efficacy)

ggplot(data=Mapped_Data, aes(x=percent_mark, y=infection_efficacy, group=1)) +
  geom_line(color="skyblue", size = 2) +
  geom_point(data = Table_Data, color = 'chartreuse3', shape = 5, size = 5, stroke = 2)+
  labs(title="Efficacy against Infectiousness (AZ/PF/Mod (1 dose))  x ~ 0.80",
       x ="Vaccine Escape Percentage", y = "VE") +
  geom_path(data = data.frame(percent_mark = c(50, 50), infection_efficacy = c(0.33, infection_efficacy[51]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(25, 25), infection_efficacy = c(0.45, infection_efficacy[26]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(100, 100), infection_efficacy = c(0.2, infection_efficacy[101]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  ylim(0,0.8)+
  theme_classic() +
  theme(
    plot.title = element_text( size=14, face="bold"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text = element_text(size=14)
  ) -> plot1


plot_grid(plot1, plot2, ncol = 1)
