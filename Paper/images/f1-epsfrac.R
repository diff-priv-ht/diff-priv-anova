###################### load packages and functions ############################

require(ggplot2)
require(purrr)
require(dplyr)
require(tidyverse)
require(rmutil)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/datagen.R?token=AeAa3YlvA4FuDWcBEb9zfFjtKAEktgOqks5bjh7iwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/ggplot_theme.R?token=AeAa3Y14yJtVX39GdK5dfyZkEbs3M77Qks5bjh8qwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/helpers.R?token=AeAa3b71_HFAGlkpAdQ3JYt6SoPat3TZks5bjh9NwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/power_calc.R?token=AeAa3XDD7X9RgE7INdceLtzufMhKQLfTks5bjh9pwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/stat_functions.R?token=AeAa3bvXCCClhPiibKASw6VCr8Ye4KVrks5bjh-AwA%3D%3D', echo=FALSE)

############################# run simulations #################################

wd <- getwd()
setwd(dirname(sys.frame(1)$ofile))

# sigma <- .15    
# effect_size <- 1*sigma
# mus <- c(.5-effect_size,.5,.5+effect_size)
# epsilons <- c(1, .5, .1)
# ps <- seq(from = .7, to = 1.4, by = .1)
# Ns <- seq(from = 30, to = 800, by = 3*20)
# 
# set.seed(4)
# pplot4 <- power_plot_frac(Ns, mus, sigma, epsilon = 1, Statistic = "absFstat",
#                           p = NULL, reps = 500, fracs = c(.5,.55,.6,.65,.7,.75))
# 
# d <- pplot4$data
# write_csv(d, path = "f1-epsfrac.csv")

#################################### plot #####################################
d <- read_csv("f1-epsfrac.csv")

p1 <- ggplot(d, aes(x = N, y = Power, col = as.factor(eps_frac))) +
  geom_line() +
  theme_Publication() + 
  scale_colour_Publication() +
  xlim(c(0,600)) +
  labs(x = "N", col = expression(rho)) +
  ggtitle(expression(paste("Effect of  ", rho, " on power")))

ggsave("f1-epsfrac.png", p1, device = "png", height = 5, width = 8, units = "in", dpi = "retina")
setwd(wd)