###################### load packages and functions ############################

require(purrr)
require(tidyverse)
require(rmutil)
source("../../Code/R_code/R/main_script.R", echo = FALSE, chdir = TRUE)

############################# run simulations #################################

reps <- 10000
sigma <- .15
effect_size <- 1*sigma
mus <- c(.5 - effect_size,
         .5,
         .5 + effect_size)
epsilons <- c(.1, 1, 10)

Ns <- c(3, 6, 12, 30, 51, 75, 150, 3*round(10^seq(from = 2, to = 3.625, by = .125)))

set.seed(1)
q2 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "Fstat",
           p = NULL, reps = reps, frac = .5)[[2]]$data

q1 <- power_plot(Ns, mus, sigma, epsilons, Statistic = "absFstat",
           p = NULL, reps = reps, frac = .7)[[2]]$data

q <- rbind(q2, q1) %>%
  mutate(stat = rep(c("F2", "F1"), 
                    c(nrow(q2),
                      nrow(q1))),
         eps = fct_recode(eps, "Public" = "None"))

q$Power[q$N == 3] <- .05

write_csv(q, "f1-vs-f2-quartet.csv")

#################################### plot #####################################

d <- read_csv("f1-vs-f2-quartet.csv")

p1 <- ggplot(d, aes(x = N,y = Power, col = stat)) +
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

ggsave("f1-vs-f2-quartet.png", p1, device = "png", height = 8, width = 8,
       units = "in", dpi = "retina")

