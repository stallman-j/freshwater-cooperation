# _______________________________#
# Environment
# Plot Stone-Geary Utility Functions
# 
# Stallman
# Started 2023-05-23
# Last edited: 
#________________________________#



# Startup

rm(list = ls())


# bring in the packages, folders, paths

home_folder <- file.path("P:","Projects","freshwater-cooperation")

source(file.path(home_folder,"code","00_startup_master.R"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2,
  plotly,
  sf,
  riverdist,
  ggrepel, # for putting text on a plot
  stringr, # string ops
  parallel, # parallel ops
  ggspatial # things like scale bars
)

#parameters
#

# endowments
m_1 <- 6
m_2 <- 6

p_1 <- 1
p_2 <- 1

# part a: zero giving (corner)

sg_iso <- function(x,
                   U,
                   a_1 = 2/3,
                   b_1 = 1,
                   m_1 = 6,
                   m_2 = 6,
                   p_1 = 1,
                   p_2 = 1){
  
  a_2 <- 1-a_1
  b_2 <- -1*b_1
  
  exp(1/(a_2)*U-(a_1/a_2)*log(x-b_1)) + b_2
  
}


df <- data.frame(x = c(0,0,6),
                 y = c(0,12,6))

plot <- ggplot() +
  xlim(.00000002,16)+
  ylim(.00000002,16)+
  stat_function(fun = sg_iso, args = list(U = .7))+
  stat_function(fun = sg_iso, args = list(U = 1))+
  stat_function(fun = sg_iso, args = list(U = 1.2))+
  stat_function(fun = sg_iso, args = list(U = 1.4))+
  stat_function(fun = sg_iso, args = list(U = 1.72)) +
  stat_function(fun = sg_iso, args = list(U = 2)) +
  xlab(expression(x[u])) + ylab(expression(x[d]))+
  labs(title = expression(paste('Corner, Upstream Takes All, ',p[u],x[u]==m[u])),
       #caption = c(paste0("Reproduced from Moffatt and Zevallos (2021)"))
       )+
  geom_polygon(data = data.frame(x = c(0.2,0.2,6),
                                 y = c(6,12,6)),
               aes(x = x, y = y), alpha = 0.2, color = "#63aaff", fill = "#63aaff")+
  theme_map(legend_position = "none",
            axis_line = element_line(color = "darkgrey")
            
  )
plot

save_map(output_folder = file.path(output_figures),
         plotname = plot,
         filename = "SG_give_none.png",
         width = 8,
         height = 8,
         dpi  = 300)

plot <- ggplot() +
  xlim(.00000002,16)+
  ylim(.00000002,16)+
  stat_function(fun = sg_iso, args = list(U = .7, b_1 = -5))+
  stat_function(fun = sg_iso, args = list(U = 1, b_1 = -5))+
  stat_function(fun = sg_iso, args = list(U = 1.6, b_1 = -5))+
  stat_function(fun = sg_iso, args = list(U = 1.86, b_1 = -5))+
  stat_function(fun = sg_iso, args = list(U = 1.95, b_1 = -5)) +
  stat_function(fun = sg_iso, args = list(U = 2, b_1 = -5)) +
  xlab(expression(x[u])) + ylab(expression(x[d]))+
  labs(title = expression(paste('Interior, 0<', p[u],x[u],'<',m[u])),
       #caption = c(paste0("Reproduced from Moffatt and Zevallos (2021)"))
       )+
  geom_polygon(data = data.frame(x = c(0.2,0.2,6),
                                 y = c(6,12,6)),
               aes(x = x, y = y), alpha = 0.2, color = "#63aaff", fill = "#63aaff")+
  theme_map(legend_position = "none",
            axis_line = element_line(color = "darkgrey")
  )

plot

save_map(output_folder = file.path(output_figures),
         plotname = plot,
         filename = "SG_interior.png",
         width = 8,
         height = 8,
         dpi  = 300)


plot <- ggplot() +
  xlim(.00000002,16)+
  ylim(.00000002,16)+
  stat_function(fun = sg_iso, args = list(U = .02, b_1 = -10))+
  stat_function(fun = sg_iso, args = list(U = 1, b_1 = -10))+
  stat_function(fun = sg_iso, args = list(U = 1.6, b_1 = -10))+
  stat_function(fun = sg_iso, args = list(U = 1.78, b_1 = -10))+
  stat_function(fun = sg_iso, args = list(U = 1.9, b_1 = -10)) +
  stat_function(fun = sg_iso, args = list(U = 2, b_1 = -10)) +
  xlab(expression(x[u])) + ylab(expression(x[d]))+
  labs(title = expression(paste('Corner, Upstream Gives All, 0=', p[u],x[u])),
       #caption = c(paste0("Reproduced from Moffatt and Zevallos (2021)"))
       )+
  geom_polygon(data = data.frame(x = c(0.2,0.2,6),
                                 y = c(6,12,6)),
               aes(x = x, y = y), alpha = 0.2, color = "#63aaff", fill = "#63aaff")+
  theme_map(legend_position = "none",
            axis_line = element_line(color = "darkgrey")
  )

plot

save_map(output_folder = file.path(output_figures),
         plotname = plot,
         filename = "SG_give_all.png",
         width = 8,
         height = 8,
         dpi  = 300)



