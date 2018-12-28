###################### load packages and functions ############################

require(ggplot2)
require(purrr)
require(dplyr)
require(tidyverse)
require(rmutil)
source("../../Code/R_code/R/main_script.R", echo = FALSE, chdir = TRUE)

############################# run simulations #################################

sigma <- .15
effect_size <- 1*sigma
mus <- c(.5-effect_size,.5,.5+effect_size)
epsilons <- c(.1, 1, 10)
# Ns <- c(3,30,75,150,3*round(10^seq(from = 2, to = 3.625, by = .125)))

Ns <- c(3, 6, 12, 30, 51, 75, 150, 3*round(10^seq(from = 2, to = 3.625, by = .125)))

set.seed(1)
pplot1 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "Fstat",
                     p = NULL, reps = 10000, frac = .5)       #see power_calc.R

set.seed(2)
pplot2 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "absFstat",
                     p = NULL, reps = 10000, frac = .7)       #see power_calc.R

d <- rbind(pplot1[[2]]$data, pplot2[[2]]$data) %>%
        mutate(stat = rep(c("F2", "F1"), 
                          c(nrow(pplot1[[2]]$data),
                            nrow(pplot2[[2]]$data))))

write_csv(d, "f1-vs-f2-quartet.csv")

#################################### extra data ################################

pplot3 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "Fstat",
                     p = NULL, reps = 10000, frac = .5)       #see power_calc.R
pplot4 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "absFstat",
                     p = NULL, reps = 10000, frac = .7)       #see power_calc.R

pplot5 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "Fstat",
                     p = NULL, reps = 10000, frac = .5)       #see power_calc.R
pplot6 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "absFstat",
                     p = NULL, reps = 10000, frac = .7)       #see power_calc.R

pplot7 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "Fstat",
                     p = NULL, reps = 10000, frac = .5)       #see power_calc.R
pplot8 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "absFstat",
                     p = NULL, reps = 10000, frac = .7)       #see power_calc.R

pplot9 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "Fstat",
                     p = NULL, reps = 10000, frac = .5)       #see power_calc.R
pplot10 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "absFstat",
                     p = NULL, reps = 10000, frac = .7)       #see power_calc.R

d2 <- rbind(pplot1[[2]]$data, pplot2[[2]]$data,
           pplot3[[2]]$data, pplot4[[2]]$data,
           pplot5[[2]]$data, pplot7[[2]]$data,
           pplot7[[2]]$data, pplot8[[2]]$data,
           pplot9[[2]]$data, pplot10[[2]]$data)
d <- d2 %>%
  mutate(stat = rep(rep(c("F2", "F1"), 
                        c(nrow(pplot1[[2]]$data), nrow(pplot2[[2]]$data))),
                    5)) %>%
  group_by(N, eps, stat) %>%
  summarize(Power = mean(Power)) %>%
  ungroup()
d <- d2 %>%
  mutate(stat = rep(rep(c("F2", "F1"), 
                        c(nrow(pplot1[[2]]$data), nrow(pplot2[[2]]$data))),
                    5)) %>%
  slice(1:168)
write_csv(d, "f1-vs-f2-quartet.csv")

#################################### plot #####################################

d <- read_csv("f1-vs-f2-quartet.csv")

d <- d %>%
  filter(eps != 10000) %>%
  mutate(eps = fct_recode(eps,
         "Inf" = "None"))

d$Power[d$N == 3] <- .05

ggplot(d, aes(x = N,y = Power, col = stat)) +
  facet_wrap(~eps, labeller = label_both) +
  geom_line() +
  scale_x_log10() +
  theme_Publication() +
  scale_colour_Publication() +
  scale_fill_Publication() +
  labs(x = "Database Size (log scale)", y = "Power", col = "Statistic") +
  ggtitle(expression(paste("Power comparison of  ",
                           F[1], " and ", F[2], " statistics"))) +
  scale_colour_discrete(labels = c(expression(F[1]), expression(F[2])))

ggsave("f1-vs-f2-quartet.png", p1, device = "png", height = 5, width = 8, units = "in", dpi = "retina")
setwd(wd)

d %>%
  filter(eps == Inf) %>%
  spread(key = stat, value = Power) %>%
  mutate(F2_wins = F2 >= F1)
