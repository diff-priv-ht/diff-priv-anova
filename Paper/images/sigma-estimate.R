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

# Ns <- seq(from = 30, to = 2000, by = 15)
sigma <- .15
# it <- 100
# estimates <- rep(NA,max(Ns)*it) %>% matrix(nrow = it)
# for (n in Ns){
#   D <- datagen(n, c(.5,.5,.5), sigma, it)
#   estimates[,n] <- absFstat_parallel(D,1)[,4] # see stat_functions.R
# }                                             # col 4 is sigma estimate and
# estimates <- estimates[,!is.na(estimates[2,])]# col 2 is statistic, for eps = 1
# 
# d1 <- data.frame(x = rep(Ns,rep(it,length(Ns))),
#                  y = estimates %>% as.vector)
# 
# write_csv(d1, path = "sigma-estimate.csv")

#################################### plot #####################################

d1 <- read_csv("sigma-estimate.csv")

p1 <- ggplot(d1,aes(x = x,y = y)) +
  geom_point(alpha = .15) +
  geom_hline(yintercept = c(0,sigma), col = c("black", "#EFEFEF")) +
  theme_Publication() +
  xlab("N") +
  labs(y = "estimate", col = "true std dev") +
  ylim(-.35, .5) +
  ggtitle(expression(paste("Accuracy of ", sigma, " estimate"))) +
  theme(legend.position = "none")

ggsave("sigma-estimate.png", p1, device = "png")
setwd(wd)
