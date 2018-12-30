###################### load packages and functions ############################

require(ggpubr)
require(purrr)
require(tidyverse)
require(rmutil)
source("../../Code/R_code/R/main_script.R", echo = FALSE, chdir = TRUE)


############################# make plots ######################################

d1 <- read_csv("sim_1.csv")
d2 <- read_csv("sim_2.csv")
d3 <- read_csv("sim_3.csv")
d4 <- read_csv("sim_4.csv")
d5 <- read_csv("sim_5.csv")
d6 <- read_csv("sim_6.csv")

d <- bind_rows(d1, d2, d3, d4, d5, d6)

eps_vec <- c(.1, 1)
k_vec   <- c(3, 7)
rho_vec <- c(0.5, 0.7, 0.85)

plots <- crossing(eps_vec, rho_vec, k_vec) %>%
  mutate(plt = vector("list", n()))

### epsilon = .1 ###

for(i in 1:6) {
  plots$plt[[i]] <- d %>%
    filter(rho == plots$rho_vec[i], 
           epsilon == plots$eps_vec[i], 
           k == plots$k_vec[i]) %>%
    rename(sd = sigma, ef = effect_size) %>%
    mutate(N = log10(N)) %>%
    ggplot(aes(x = N, y = Power, col = as.factor(exponent))) +
    facet_grid(ef ~ sd, labeller = label_both) +
    scale_x_continuous(labels = scales::math_format(10^.x)) +
    geom_line() +
    theme_Publication() + 
    scale_colour_Publication() +
    labs(x = "Database Size (log scale)", col = "q") +
    guides(col = FALSE)
}

g1 <- ggarrange(plots$plt[[1]], plots$plt[[2]],
                plots$plt[[3]], plots$plt[[4]],
                plots$plt[[5]], plots$plt[[6]],
                nrow = 3, ncol = 2) %>%
  annotate_figure(top = text_grob("      k = 3                                                                                                                                                                   k = 7", face = "bold"), 
                  left = text_grob(expression(paste(rho, " = .85                                                                                                                                                  ", rho, " = .7                                                                                                                                                   ", rho, " = .5  ")),
                                   rot = 90, face = "bold", vjust = .5))

ggsave(paste0("par-plot-1.png"), g1, device = "png", 
       height = 22, width = 16, units = "in", dpi = "retina")

### epsilon = 1 ###

for(i in 7:12) {
  plots$plt[[i]] <- d %>%
    filter(rho == plots$rho_vec[i], 
           epsilon == plots$eps_vec[i], 
           k == plots$k_vec[i]) %>%
    rename(sd = sigma, ef = effect_size) %>%
    mutate(N = log10(N)) %>%
    ggplot(aes(x = N, y = Power, col = as.factor(exponent))) +
    facet_grid(ef ~ sd, labeller = label_both) +
    scale_x_continuous(labels = scales::math_format(10^.x)) +
    geom_line() +
    theme_Publication() + 
    scale_colour_Publication() +
    labs(x = "Database Size (log scale)", col = "q") +
    guides(col = FALSE)
}

g2 <- ggarrange(plots$plt[[7]], plots$plt[[8]],
                plots$plt[[9]], plots$plt[[10]],
                plots$plt[[11]], plots$plt[[12]],
                nrow = 3, ncol = 2) %>%
  annotate_figure(top = text_grob("      k = 3                                                                                                                                                                   k = 7", face = "bold"), 
                  left = text_grob(expression(paste(rho, " = .85                                                                                                                                                  ", rho, " = .7                                                                                                                                                   ", rho, " = .5  ")),
                                   rot = 90, face = "bold", vjust = .5))

ggsave(paste0("par-plot-2.png"), g2, device = "png", 
       height = 22, width = 16, units = "in", dpi = "retina")
