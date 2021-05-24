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


#PF values correspond to VE against (i/ii) severe disease 1dose/2dose (iii/iv) infection 1dose/2dose (v) infectiousness (1 dose)
y0_PF <- c(0.8/(1-0.8), 0.95/(1-0.95), 0.65/(1-0.65), 0.86/(1-0.86))

y25_PF <- c(0.8/(1-0.8), 0.95/(1-0.95), 0.65/(1-0.65), 0.86/(1-0.86))
y50_PF <- c(0.7/(1-0.7), 0.95/(1-0.95), 0.55/(1-0.55), 0.85/(1-0.85))
y75_PF <- c(0.7/(1-0.7), 0.9/(1-0.9), 0.4/(1-0.4), 0.75/(1-0.75))
y100_PF <- y75_PF



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
  
  y25_estimate_PF_probability <- y25_estimate_PF/(y25_estimate_PF+1)
  y25_PF_probability <- y25_PF/(y25_PF+1)
  
  y50_estimate_PF_probability <- y50_estimate_PF/(y50_estimate_PF+1)
  y50_PF_probability <- y50_PF/(y50_PF+1)
  
  y100_estimate_PF_probability <- y100_estimate_PF/(y100_estimate_PF+1)
  y100_PF_probability <- y100_PF/(y100_PF+1)
  
  error_amt <-  (sum((abs(y50_estimate_PF_probability - y50_PF_probability))^2  )  +  sum(  (abs(y100_estimate_PF_probability - y100_PF_probability))^2  )  +  sum(  (abs(y25_estimate_PF_probability - y25_PF_probability))^2)   )
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

Table_Data <- data.frame(percent_mark = c(0, 25, 50, 100), sev_disease_efficacy = c(0.8, 0.8, 0.7, 0.7))
Mapped_Data <- data.frame(percent_mark = percent_mark, sev_disease_efficacy = sev_disease_efficacy)

ggplot(data=Mapped_Data, aes(x=percent_mark, y=sev_disease_efficacy, group=1)) +
  geom_line(color="skyblue", size = 2) +
  geom_point(data = Table_Data, color = 'chartreuse3', shape = 5, size = 5, stroke = 2)+
  labs(title="Severe Disease (1 dose)",
       x ="Vaccine Escape Percentage", y = "VE") +
  geom_path(data = data.frame(percent_mark = c(50, 50), sev_disease_efficacy = c(0.7, sev_disease_efficacy[51]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(25, 25), sev_disease_efficacy = c(0.8, sev_disease_efficacy[26]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(100, 100), sev_disease_efficacy = c(0.7, sev_disease_efficacy[101]) ),
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

Table_Data <- data.frame(percent_mark = c(0, 25, 50, 100), sev_dis_efficacy = c(0.95, 0.95, 0.95, 0.9))
Mapped_Data <- data.frame(percent_mark = percent_mark, sev_dis_efficacy = sev_dis_efficacy)

ggplot(data=Mapped_Data, aes(x=percent_mark, y=sev_dis_efficacy, group=1)) +
  geom_line(color="skyblue", size = 2) +
  geom_point(data = Table_Data, color = 'chartreuse3', shape = 5, size = 5, stroke = 2)+
  labs(title="Severe Disease (2 dose)",
       x ="Vaccine Escape Percentage", y = "VE") +
  geom_path(data = data.frame(percent_mark = c(50, 50), sev_dis_efficacy = c(0.95, sev_dis_efficacy[51]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(25, 25), sev_dis_efficacy = c(0.95, sev_dis_efficacy[26]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(100, 100), sev_dis_efficacy = c(0.9, sev_dis_efficacy[101]) ),
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

Table_Data <- data.frame(percent_mark = c(0, 25, 50, 100), infection_efficacy = c(0.65, 0.65, 0.55, 0.4))
Mapped_Data <- data.frame(percent_mark = percent_mark, infection_efficacy = infection_efficacy)

ggplot(data=Mapped_Data, aes(x=percent_mark, y=infection_efficacy, group=1)) +
  geom_line(color="skyblue", size = 2) +
  geom_point(data = Table_Data, color = 'chartreuse3', shape = 5, size = 5, stroke = 2)+
  labs(title="Infection (1 dose)",
       x ="Vaccine Escape Percentage", y = "VE") +
  geom_path(data = data.frame(percent_mark = c(50, 50), infection_efficacy = c(0.55, infection_efficacy[51]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(25, 25), infection_efficacy = c(0.65, infection_efficacy[26]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(100, 100), infection_efficacy = c(0.4, infection_efficacy[101]) ),
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

Table_Data <- data.frame(percent_mark = c(0, 25, 50, 100), infection_efficacy = c(0.86, 0.86, 0.85, 0.75))
Mapped_Data <- data.frame(percent_mark = percent_mark, infection_efficacy = infection_efficacy)

ggplot(data=Mapped_Data, aes(x=percent_mark, y=infection_efficacy, group=1)) +
  geom_line(color="skyblue", size = 2) +
  geom_point(data = Table_Data, color = 'chartreuse3', shape = 5, size = 5, stroke = 2)+
  labs(title="Infection (2 doses)",
       x ="Vaccine Escape Percentage", y = "VE") +
  geom_path(data = data.frame(percent_mark = c(50, 50), infection_efficacy = c(0.85, infection_efficacy[51]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(25, 25), infection_efficacy = c(0.86, infection_efficacy[26]) ),
            size = 1, colour = "#EC7014",
            arrow = arrow(type = "open", angle = 40, length = unit(0.1, "inches")) 
  )+
  geom_path(data = data.frame(percent_mark = c(100, 100), infection_efficacy = c(0.75, infection_efficacy[101]) ),
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
title <- ggdraw() + draw_label("PF Vaccine Efficacy (x ~ 0.83)", fontface='bold')
#Add title:
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) -> plot1 # rel_heights values control title margins

plot_grid(plot1, plot2, ncol = 1)

#=======================================================================


e_I <- (0.86/(1-0.86))*(optim_x_PF^(percent_mark/25))
e_I <- e_I/(e_I+1)

e_H <- (0.95/(1-0.95))*(optim_x_PF^(percent_mark/25))
e_H <- e_H/(e_H+1)

#Note, e_S was not used in fitting optim_x
e_S <- (0.86/(1-0.86))*(optim_x_PF^(percent_mark/25))
e_S <- e_S/(e_S+1)

e_S_cond_I <- (e_S - e_I)/(1-e_I)

e_H_cond_I <- (e_H - e_S)/((1-e_I)*(1-e_S_cond_I))

e_Data <- data.frame(percent_mark = percent_mark, e_H_cond_I = e_H_cond_I)


ggplot(data=e_Data, aes(x=percent_mark, y=e_H_cond_I, group=1)) +
  geom_line(color="skyblue", size = 2) +
  labs(title="PF (2 doses)",
       x ="Vaccine Escape Percentage", y = "e_Hosp | Sympt") +
  ylim(0,0.7)+
  theme_classic() +
  theme(
    plot.title = element_text( size=14, face="bold"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text = element_text(size=14)
  ) -> PF2_plot


plot_grid(AZ1_plot, PF1_plot, PF2_plot, nrow = 1)

plot_grid(AZ1_plot, PF1_plot, PF2_plot, nrow = 1, labels = c('A', 'B', 'C'), label_size = 12) -> p

#Make a title
title <- ggdraw() + draw_label("Vaccine efficacy against severe disease conditional on symptomatic disease", fontface='bold')
#Add title:
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) #-> plot1 # rel_heights values control title margins
