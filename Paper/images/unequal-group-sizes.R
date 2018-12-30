library(ggplot2)
library(purrr)
library(dplyr)
library(tidyverse)
library(rmutil)
library(scales)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/datagen.R?token=AeAa3YlvA4FuDWcBEb9zfFjtKAEktgOqks5bjh7iwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/ggplot_theme.R?token=AeAa3Y14yJtVX39GdK5dfyZkEbs3M77Qks5bjh8qwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/helpers.R?token=AeAa3b71_HFAGlkpAdQ3JYt6SoPat3TZks5bjh9NwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/power_calc.R?token=AeAa3XDD7X9RgE7INdceLtzufMhKQLfTks5bjh9pwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/stat_functions.R?token=AeAa3bvXCCClhPiibKASw6VCr8Ye4KVrks5bjh-AwA%3D%3D', echo=FALSE)

################################ data sim ######################################
wd <- getwd()
setwd(dirname(sys.frame(1)$ofile))

# set.seed(10)
# d <- data.frame(x = c(absFstat(datagen(800, rep(0,4), .1, reps = 10000, sizes = rep(1, 4)), -1),
#                       absFstat(datagen(800, rep(0,4), .1, reps = 10000, sizes = c(100,100,100,500)), -1),
#                       absFstat(datagen(800, rep(0,4), .1, reps = 10000, sizes = c(20,5,10,765)), -1),
#                       absFstat(datagen(800, rep(0,4), .1, reps = 10000, sizes = c(3,3,3,791)), -1) ),
#                 type = rep(c("A", "B", "C", "D"),
#                            rep(10000,4)) )
# 
# write_csv(d, path = "unequal-group-sizes.csv")

################################## plot ########################################
d <- read_csv("unequal-group-sizes.csv")

crit_val <- d %>%
  filter(type == "A") %>%
  pull(x) %>%
  quantile(.95)
p1 <- ggplot(d, aes(x = x, fill = type)) +
  geom_density(alpha = .5, lty = 0) +
  labs(fill = "group sizes") +
  xlim(c(0,40)) +
  scale_fill_discrete(labels = c(expression(s[0]), expression(s[1]), 
                               expression(s[2]), expression(s[3]))) +
  theme_Publication() +
  annotate("segment", x = crit_val, y = 0, xend = crit_val, yend = .3, lty = 2) + 
  ggtitle(expression(paste("Reference distributions with unequal ", n[j])))

ggsave("unequal-group-sizes.png", p1, device = "png", height = 5, width = 8, units = "in", dpi = "retina")
setwd(wd)