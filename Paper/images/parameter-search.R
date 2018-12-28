###################### load packages and functions ############################

require(ggplot2)
require(purrr)
require(dplyr)
require(tidyverse)
require(rmutil)
source("../../Code/R_code/R/main_script.R", echo = FALSE, chdir = TRUE)


############################# run simulations #################################

reps <- 5000

#### Sim 1: q plot (eps = .1, frac = .7) ####

epsilon <- .1
k <- c(3, 7)
sigma <- c(.1, .25)
effect_size <- c(.5, 1)
par_qs <- c(.75, 1, 1.5, 2)

sim_1 <- crossing(epsilon, k, sigma, effect_size) %>%
  mutate(plot_df = vector("list", n()))

for(i in 1:nrow(sim_1)) {
  par_eps <- sim_1$epsilon[i]
  par_k   <- sim_1$k[i]
  par_sig <- sim_1$sigma[i]
  par_mus <- c(rep(.5, par_k - 2),
               .5 - sim_1$effect_size[i] * par_sig, 
               .5 + sim_1$effect_size[i] * par_sig)
  par_Ns  <- par_k*round(10^seq(from = 2.5, to = 3.5, by = .125))
  sim_1$plot_df[[i]] <- power_plot_p(Ns = par_Ns, mus = par_mus, 
                                          sigma = par_sig, epsilon = par_eps,
                                          Statistic = "Fpstat", ps = par_qs, 
                                          reps = reps, frac = .7)$data
}

write_csv(unnest(sim_1, plot_df), "sim_1.csv")

#### Sim 2: q plot (eps = 1, frac = .7) ####

epsilon <- 1
k <- c(3, 7)
sigma <- c(.1, .25)
effect_size <- c(.5, 1)
par_qs <- c(.75, 1, 1.5, 2)

sim_2 <- crossing(epsilon, k, sigma, effect_size) %>%
  mutate(plot_df = vector("list", n()))

for(i in 1:nrow(sim_2)) {
  par_eps <- sim_2$epsilon[i]
  par_k   <- sim_2$k[i]
  par_sig <- sim_2$sigma[i]
  par_mus <- c(rep(.5, par_k - 2),
               .5 - sim_2$effect_size[i] * par_sig, 
               .5 + sim_2$effect_size[i] * par_sig)
  par_Ns <- c(par_k*10, par_k*25, par_k*50, 
              par_k*round(10^seq(from = 2, to = 2.75, by = .125)))
  sim_2$plot_df[[i]] <- power_plot_p(Ns = par_Ns, mus = par_mus, 
                                          sigma = par_sig, epsilon = par_eps, 
                                          Statistic = "Fpstat", ps = par_qs, 
                                          reps = reps, frac = .7)$data
}

write_csv(unnest(sim_2, plot_df), "sim_2.csv")

#### Sim 3: q plot (eps = .1, frac = .5) ####

epsilon <- .1
k <- c(3, 7)
sigma <- c(.1, .25)
effect_size <- c(.5, 1)
par_qs <- c(.75, 1, 1.5, 2)

sim_3 <- crossing(epsilon, k, sigma, effect_size) %>%
  mutate(plot_df = vector("list", n()))

for(i in 1:nrow(sim_3)) {
  par_eps <- sim_3$epsilon[i]
  par_k   <- sim_3$k[i]
  par_sig <- sim_3$sigma[i]
  par_mus <- c(rep(.5, par_k - 2),
               .5 - sim_3$effect_size[i] * par_sig, 
               .5 + sim_3$effect_size[i] * par_sig)
  par_Ns  <- par_k*round(10^seq(from = 2.5, to = 3.5, by = .125))
  sim_3$plot_df[[i]] <- power_plot_p(Ns = par_Ns, mus = par_mus, 
                                     sigma = par_sig, epsilon = par_eps,
                                     Statistic = "Fpstat", ps = par_qs, 
                                     reps = reps, frac = .5)$data
}

write_csv(unnest(sim_3, plot_df), "sim_3.csv")

#### Sim 4: q plot (eps = 1, frac = .5) ####

epsilon <- 1
k <- c(3, 7)
sigma <- c(.1, .25)
effect_size <- c(.5, 1)
par_qs <- c(.75, 1, 1.5, 2)

sim_4 <- crossing(epsilon, k, sigma, effect_size) %>%
  mutate(plot_df = vector("list", n()))

for(i in 1:nrow(sim_4)) {
  par_eps <- sim_4$epsilon[i]
  par_k   <- sim_4$k[i]
  par_sig <- sim_4$sigma[i]
  par_mus <- c(rep(.5, par_k - 2),
               .5 - sim_4$effect_size[i] * par_sig, 
               .5 + sim_4$effect_size[i] * par_sig)
  par_Ns <- c(par_k*10, par_k*25, par_k*50, 
              par_k*round(10^seq(from = 2, to = 2.75, by = .125)))
  sim_4$plot_df[[i]] <- power_plot_p(Ns = par_Ns, mus = par_mus, 
                                          sigma = par_sig, epsilon = par_eps, 
                                          Statistic = "Fpstat", ps = par_qs, 
                                          reps = reps, frac = .5)$data
}

write_csv(unnest(sim_4, plot_df), "sim_4.csv")


#### Sim 5: q plot (eps = 1, frac = .5) ####

epsilon <- 1
k <- c(3, 7)
sigma <- c(.1, .25)
effect_size <- c(.5, 1)
par_qs <- c(.75, 1, 1.5, 2)

sim_5 <- crossing(epsilon, k, sigma, effect_size) %>%
  mutate(plot_df = vector("list", n()))

for(i in 1:nrow(sim_5)) {
  par_eps <- sim_5$epsilon[i]
  par_k   <- sim_5$k[i]
  par_sig <- sim_5$sigma[i]
  par_mus <- c(rep(.5, par_k - 2),
               .5 - sim_5$effect_size[i] * par_sig, 
               .5 + sim_5$effect_size[i] * par_sig)
  par_Ns  <- par_k*round(10^seq(from = 2.5, to = 3.5, by = .125))
  sim_5$plot_df[[i]] <- power_plot_p(Ns = par_Ns, mus = par_mus, 
                                     sigma = par_sig, epsilon = par_eps, 
                                     Statistic = "Fpstat", ps = par_qs, 
                                     reps = reps, frac = .85)$data
}

write_csv(unnest(sim_5, plot_df), "sim_5.csv")


#### Sim 6: q plot (eps = 1, frac = .85) ####

epsilon <- 1
k <- c(3, 7)
sigma <- c(.1, .25)
effect_size <- c(.5, 1)
par_qs <- c(.75, 1, 1.5, 2)

sim_6 <- crossing(epsilon, k, sigma, effect_size) %>%
  mutate(plot_df = vector("list", n()))

for(i in 1:nrow(sim_6)) {
  par_eps <- sim_6$epsilon[i]
  par_k   <- sim_6$k[i]
  par_sig <- sim_6$sigma[i]
  par_mus <- c(rep(.5, par_k - 2),
               .5 - sim_6$effect_size[i] * par_sig, 
               .5 + sim_6$effect_size[i] * par_sig)
  par_Ns <- c(par_k*10, par_k*25, par_k*50, 
              par_k*round(10^seq(from = 2, to = 2.75, by = .125)))
  sim_6$plot_df[[i]] <- power_plot_p(Ns = par_Ns, mus = par_mus, 
                                     sigma = par_sig, epsilon = par_eps, 
                                     Statistic = "Fpstat", ps = par_qs, 
                                     reps = reps, frac = .85)$data
}

write_csv(unnest(sim_6, plot_df), "sim_6.csv")


#ggsave("fq-power.png", p1, device = "png", height = 5, width = 8, units = "in", dpi = "retina")
#setwd(wd)
