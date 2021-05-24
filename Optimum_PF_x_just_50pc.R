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

#----------------------------------------
#PF
#Assume the same worst case is 100%.

error_x_PF <- function(x) {
  
  #Allocate memory:
  y25_estimate_PF <- c(0,0,0,0)
  y50_estimate_PF <- c(0,0,0,0)
  y100_estimate_PF <- c(0,0,0,0)
  
  y25_estimate_PF <- (x)*y0_PF
  y50_estimate_PF <- (x^2)*y0_PF
  y100_estimate_PF <- (x^4)*y0_PF
  
  y50_estimate_PF_probability <- y50_estimate_PF/(y50_estimate_PF+1)
  y50_PF_probability <- y50_PF/(y50_PF+1)
  
  error_amt <- (sum( (abs(y50_estimate_PF_probability - y50_PF_probability))^2  ) ) # +  sum(abs(y100_estimate_PF - y100_PF))  +  sum(abs(y25_estimate_PF - y25_PF)) )
  return(error_amt)
}

optim_PF <- optimize(error_x_PF, interval = c(0,1))
optim_x_PF <- optim_PF$minimum
optim_x_PF_error <- optim_PF$objective

#----------------------------

#Okay, now we want to plot it
######
#PF
######

percent_mark <- seq(0,100, length.out = 101)

# Severe Disease (1 dose)

sev_disease_efficacy <- (0.8/(1-0.8))*(optim_x_PF^(percent_mark/25))
#Convert back to probability:
sev_disease_efficacy <- sev_disease_efficacy/(sev_disease_efficacy+1)

Table_Data <- data.frame(percent_mark = c(0, 25, 50, 100), sev_disease_efficacy = c(0.8, 0.8, 0.7, 0.6))
Mapped_Data <- data.frame(percent_mark = percent_mark, sev_disease_efficacy = sev_disease_efficacy)

ggplot(data=Mapped_Data, aes(x=percent_mark, y=sev_disease_efficacy, group=1)) +
  geom_line(color="skyblue", size = 2) +
  geom_point(data = Table_Data, color = 'firebrick', shape = 5, size = 5, stroke = 2)+
  labs(title="Severe Disease (1 dose)",
       x ="Vaccine Escape Percentage", y = "VE") +
  geom_path(data = data.frame(percent_mark = c(50, 50), sev_disease_efficacy = c(0.7, sev_disease_efficacy[51]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+ylim(0.2,1) +
  theme_classic() +
  theme(
    plot.title = element_text( size=14, face="bold"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text = element_text(size=14)
  )  ->  PF_sev_dis_1dose_50_fit


# Severe Disease (2 doses)

sev_dis_efficacy <- (0.95/(1-0.95))*(optim_x_PF^(percent_mark/25))
#Convert back to probability:
sev_dis_efficacy <- sev_dis_efficacy/(sev_dis_efficacy+1)

Table_Data <- data.frame(percent_mark = c(0, 25, 50, 100), sev_dis_efficacy = c(0.95, 0.95, 0.9, 0.85))
Mapped_Data <- data.frame(percent_mark = percent_mark, sev_dis_efficacy = sev_dis_efficacy)

ggplot(data=Mapped_Data, aes(x=percent_mark, y=sev_dis_efficacy, group=1)) +
  geom_line(color="skyblue", size = 2) +
  geom_point(data = Table_Data, color = 'firebrick', shape = 5, size = 5, stroke = 2)+
  labs(title="Severe Disease (2 dose)",
       x ="Vaccine Escape Percentage", y = "VE") +
  geom_path(data = data.frame(percent_mark = c(50, 50), sev_dis_efficacy = c(0.9, sev_dis_efficacy[51]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+ylim(0.2,1) +
  theme_classic() +
  theme(
    plot.title = element_text( size=14, face="bold"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text = element_text(size=14)
  ) ->  PF_sev_dis_2dose_50_fit

# Infection (1 dose)

infection_efficacy <- (0.65/(1-0.65))*(optim_x_PF^(percent_mark/25))
#Convert back to probability:
infection_efficacy <- infection_efficacy/(infection_efficacy+1)

Table_Data <- data.frame(percent_mark = c(0, 25, 50, 100), infection_efficacy = c(0.65, 0.65, 0.55, 0.45))
Mapped_Data <- data.frame(percent_mark = percent_mark, infection_efficacy = infection_efficacy)

ggplot(data=Mapped_Data, aes(x=percent_mark, y=infection_efficacy, group=1)) +
  geom_line(color="skyblue", size = 2) +
  geom_point(data = Table_Data, color = 'firebrick', shape = 5, size = 5, stroke = 2)+
  labs(title="Infection (1 dose)",
       x ="Vaccine Escape Percentage", y = "VE") +
  geom_path(data = data.frame(percent_mark = c(50, 50), infection_efficacy = c(0.55, infection_efficacy[51]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+ylim(0.2,1) +
  theme_classic() +
  theme(
    plot.title = element_text( size=14, face="bold"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text = element_text(size=14)
  ) ->  PF_sinfection_1dose_50_fit


# Infection (2 doses)

infection_efficacy <- (0.86/(1-0.86))*(optim_x_PF^(percent_mark/25))
#Convert back to probability:
infection_efficacy <- infection_efficacy/(infection_efficacy+1)

Table_Data <- data.frame(percent_mark = c(0, 25, 50, 100), infection_efficacy = c(0.86, 0.85, 0.75, 0.55))
Mapped_Data <- data.frame(percent_mark = percent_mark, infection_efficacy = infection_efficacy)

ggplot(data=Mapped_Data, aes(x=percent_mark, y=infection_efficacy, group=1)) +
  geom_line(color="skyblue", size = 2) +
  geom_point(data = Table_Data, color = 'firebrick', shape = 5, size = 5, stroke = 2)+
  labs(title="Infection (2 doses)",
       x ="Vaccine Escape Percentage", y = "VE") +
  geom_path(data = data.frame(percent_mark = c(50, 50), infection_efficacy = c(0.75, infection_efficacy[51]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+ ylim(0.2,1) +
  theme_classic() +
  theme(
    plot.title = element_text( size=14, face="bold"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text = element_text(size=14)
  ) ->  PF_sinfection_2dose_50_fit


require(cowplot)
plot_grid(PF_sev_dis_1dose_50_fit, PF_sev_dis_2dose_50_fit, PF_sinfection_1dose_50_fit, PF_sinfection_2dose_50_fit, labels = c('A', 'B', 'C', 'D'), label_size = 12) -> p

#Make a title
title <- ggdraw() + draw_label("PF Vaccine Efficacy (x ~ 0.766)", fontface='bold')
#Add title:
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
