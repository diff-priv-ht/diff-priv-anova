###################### load packages and functions ############################

require(tidyverse)
source("../../Code/R_code/R/main_script.R", echo = FALSE, chdir = TRUE)

############################# wrangle python-created data ######################

d1 <- read_csv("rhos-ds1.csv", n_max = 2, col_names = FALSE)
d2 <- read_csv("rhos-ds2.csv", n_max = 2, col_names = FALSE)
d3 <- read_csv("rhos-ds3.csv", n_max = 2, col_names = FALSE)
d4 <- read_csv("rhos-ds4.csv", n_max = 2, col_names = FALSE)
d5 <- read_csv("f1-epsfrac.csv") %>%
  filter(eps_frac == 0.7) %>%
  select(N, Power) %>%
  bind_rows(newrow = data.frame(N = 5970, Power = 1))

d <- data.frame((rbind(t(d1), t(d2), t(d3), t(d4))))
names(d) <- c("N", "Power")
d <- rbind(d, d5)
d$rhos <- rep(LETTERS[1:5], times = c(13, 13, 13, 13, 14))

write_csv(d, "multiple-rhos.csv")

#################################### plot #####################################

d <- read_csv("multiple-rhos.csv") %>%
  mutate(is_original = as.factor(ifelse(rhos == "E", 1, 2)))

p1 <- ggplot(d, aes(x = N,y = Power, col = rhos)) +
  geom_line(aes(linetype = is_original)) +
  scale_x_log10() +
  theme_Publication() +
  scale_colour_Publication() +
  labs(x = "Database Size (log scale)", y = "Power", col = "Rho Values") +
  ggtitle(expression(paste("Power comparison of ", sigma, " estimation methods"))) +
  scale_colour_discrete(name = expression(paste(rho[1], ", ", 
                                          rho[2], ", ",
                                          rho[3], ", ")),
                        labels = c(".42, .18, .40", ".56, .24, .20",
                                   ".14, .06, .80", ".28, .12, .60",
                                   "original")) +
  scale_linetype(guide = "none")

ggsave("multiple-rhos.png", p1, device = "png", height = 5, width = 8, units = "in", dpi = "retina")
