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
# Ns <- c(30, 75, 150, 3*round(10^seq(from = 2, to = 2.75, by = .125)))
# 
# set.seed(5)
# pplot5 <- power_plot_p(Ns, mus = c(.35,.5,.65), sigma = .15, epsilon = 1,
#                        Statistic = "Fpstat", ps = ps, reps = 20000, frac = .7)
#                                                               #see power_calc.R
# d <- pplot5$data
# write_csv(d, "fq-power.csv") )

#################################### plot #####################################

d <- read_csv("fq-power.csv")

p1 <- ggplot(d, aes(x = N, y = Power, col = as.factor(exponent)) ) +
  geom_line() +
  theme_Publication() + 
  scale_colour_Publication() +
  labs(x = "Database Size (log scale)", col = "q") +
  ggtitle(expression(paste("Effect of q on ", F[1]))) +
  scale_x_log10()

ggsave("fq-power.png", p1, device = "png", height = 5, width = 8, units = "in", dpi = "retina")
setwd(wd)