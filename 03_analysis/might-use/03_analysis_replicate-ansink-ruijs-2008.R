# _______________________________#
# Environment
# Analysis 03: Replicate Ansink Ruijs 2008
# 
# Stallman
# Started 2023-08-05
# Last edited: 
#________________________________#



# # Startup
# 
# rm(list = ls())
# 
# 
# # bring in the packages, folders, paths
# 
home_folder <- file.path("P:","Projects","environment")
# 
source(file.path(home_folder,"code","00_startup_master.R"))

# packages ----

library(ggplot2)

# parameters ----

# benefits function
a        <- -1.5
b        <- 75
c        <- 0
q_bar    <- 40  # expectation


delta    <- 0.95 # patience
p        <- 5    # ?? probability distribution parameter?
epsilon  <- 0.5 # bargaining power

alpha    <- 0.5 # proportional allocation
beta     <- 20  # guaranteed allocation to upstream in a fixed upstream allocation, q_u = min{beta, Q}. q_d = max{q-beta,0}
gamma    <- 20  # guaranteed allocation to downstream in a fixed downstream allocation q_u = max{Q-gamma,0}. q_d = min{gamma, Q}. 


# gamma distribution parameters
shape        <- 25 #  E(Q)= shape/rate. Variance is shape*scale^2
rate         <- shape/q_bar # rate = 1/scale,

q_lower_g    <- 0
q_upper_g    <- 80

# functions ----
# benefits simple quadratic of form aq^2 + bq + c
# can be a vector or scalar

## benefits ----
B_quadratic <- function(q,
                        a = -1.5,
                        b = 75,
                        c = 0) {
  benefits = a*q^2 + b*q + c
  
  return(benefits)
}

## transfers ----



# plot the probability distribution function

# https://dk81.github.io/dkmathstats_site/rvisual-cont-prob-dists.html

#dgamma() is the gamma density
# dgamma(x, shape, rate - 1, scale = 1/4ate, log = FALSE) 
# have to specify rate or scale but not both

plot_gamma <- function(q_lower_g = 0,
                       q_upper_g = 10,
                       rate      = 2,
                       shape     = 2,
                       x_lab      = "\n Q",
                       y_lab     = "f(Q) \n",
                       title     = "Gamma Distribution",
                       ...) {
  
plot <- ggplot(data.frame(x = c(q_lower_g, q_upper_g)), aes(x = x)) +
                xlim(c(q_lower_g, q_upper_g)) +
                stat_function(fun = dgamma,
                              args = list(rate = rate,
                                          shape = shape),
                              geom = "area",
                              fill = "grey",
                              alpha = 0.25) + 
                stat_function(fun = dgamma, 
                              args = list(rate = rate, shape = shape)) +
                labs(x = x_lab, y = y_lab,
                     title = title)+
                       theme_plot(...)

return(plot)
}

plot <- plot_gamma(q_lower_g = q_lower_g,
                   q_upper_g = q_upper_g,
                   rate = rate,
                   shape = shape,
                    title =  paste0("Gamma Distribution with Rate = ",rate," and shape = ",shape,"\n"),
                   title_size = 10)

plot

save_map(plotname = plot,
         output_folder = output_figures,
         filename = paste0("gamma_distribution_ansink_ruijs_2008_rate_",round(rate,3),"_shape_",round(shape,2),".png"))




