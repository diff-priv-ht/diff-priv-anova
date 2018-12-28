
d1 <- read_csv("sim_1.csv")
d2 <- read_csv("sim_2.csv")
d3 <- read_csv("sim_3.csv")
d4 <- read_csv("sim_4.csv")
d5 <- read_csv("sim_5.csv")
d6 <- read_csv("sim_6.csv")

d <- bind_rows(d1, d2, d3, d4, d5, d6)

eps_vec <- c(.1, 1)
k_vec <- c(3, 7)
rho_vec <- c(.5, .7, .85)

plots <- crossing(rho_vec, eps_vec, k_vec) %>%
  mutate(plt = vector("list", n()))

for(i in 1:nrow(plots)) {
  plots$plt[[i]] <- d %>%
    filter(rho == plots$rho_vec[i], 
           epsilon == plots$eps_vec[i], 
           k == plots$k_vec[i]) %>%
    rename(sd = sigma, ef = effect_size) %>%
    ggplot(aes(x = N, y = Power, col = as.factor(exponent))) +
    facet_grid(ef ~ sd, labeller = label_both) +
    geom_line() +
    theme_Publication() + 
    scale_colour_Publication() +
    labs(x = "Database Size (log scale)", col = "q") +
    scale_x_log10()
}

plots$plt[[1]]
plots$plt[[2]]
