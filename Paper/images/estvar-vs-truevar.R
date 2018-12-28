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
# Ns <- c(30,75,150,3*round(10^seq(from = 2, to = 3, by = .125)))
# 
# set.seed(3)
# pplot3 <- power_plot_estimated(Ns, mus, sigma, epsilons,
#                                Statistic = "absFstat", p = NULL, reps = 500, frac = .7)
# 
# d <- ( subset(pplot2[[2]]$data, pplot2[[2]]$data$eps==1) %>%  
#          rbind(subset(pplot3[[2]]$data, pplot3[[2]]$data$eps==1)) ) %>%
#   mutate(stat = rep(c("Estimated", "Ground Truth"), 
#                     c(nrow(pplot2[[1]]),nrow(pplot3[[1]]))))
# write_csv(d, "estvar-vs-truevar.csv") )

#################################### plot #####################################

d1 <- read_csv("estvar-vs-truevar.csv")

p1 <- ggplot(d1, aes(x=N,y=Power, col = stat)) +
  geom_line() +
  xlim(c(0,712)) +
  theme_Publication() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  labs(x = "Database Size (log scale)", y = "Power", col = "Statistic") +
  ggtitle(expression(paste("Power vs. Database Size for ",
                           F[1], " and ", F[2], " Statistics")))

ggsave("estvar-vs-truevar.png", p1, device = "png")
setwd(wd)
