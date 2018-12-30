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

na.omit.mean <- function(vec){
  sum(na.omit(vec))/length(vec)
}

crit_alpha <- function(M, alpha = .05){ #helper function
  apply(M, 2, quantile,probs = 1-alpha)
}

power_stat_estimated_alpha <- function(alpha = .05, Ns, mus, sigma, epsilons,
                                       Statistic = "Fstat", reps = 1, frac = .5,
                                       sizes = rep(1,length(mus)),
                                       lower.tail = TRUE){ 
  if(Statistic == "Fstat"){
    parStat <- Fstat
    par2Stat <- Fstat_parallel
  }
  if(Statistic == "absFstat"){
    parStat <- absFstat
    par2Stat <- absFstat_parallel
  }
  if(Statistic == "Varstat"){
    parStat <- Varstat
    par2Stat <- Varstat_parallel
  }
  if(Statistic=="absVarstat"){
    parStat <- absVarstat
    par2Stat <- absVarstat_parallel
  }
  power <- rep(NA,1+length(epsilons))
  powermat <- rep(power,length(Ns)) %>%
    matrix(nrow=length(Ns))
  epsilons2 <- c(-1,epsilons)
  Reject <- rep(power,reps) %>%
    matrix(nrow=reps)
  for (i in 1:length(Ns)){
    altstat <- datagen(Ns[i], mus, sigma, reps, sizes) %>%
      par2Stat(epsilons, frac)
    sdevs <- altstat[,(2+length(epsilons)):(2*(1+length(epsilons)))]
    altstat <- altstat[,1:(1+length(epsilons))]
    nullstat <- rep(NA,(1+length(epsilons))*reps) %>%
      matrix(nrow=reps)
    for (j in 1:length(epsilons2)){
      Ds0 <- sdevs[,j] %>% map(~ datagen(Ns[i], rep(.5,length(mus)), ., reps*3,
                                         sizes)) # generate reps * 3 null stats
      #to make null dist for each standard deviation estimate
      RRs <- Ds0 %>% map(~ parStat(., epsilons2[j], frac)) %>%
        map(~ quantile(., probs = 1-alpha, na.rm=TRUE)) %>%
        unlist()
      Reject[,j] <- (altstat[,j] > RRs)
      powermat[i,] <- apply(Reject, 2, na.omit.mean)
    }
  }
  powermat
}

############################# run simulations #################################

wd <- getwd()
setwd(dirname(sys.frame(1)$ofile))

# epsilons <- c(1,.5,.1)
# alphas <- c(1,.8,.6,.4,.2,.1,.05,0.01)
# out <- matrix(rep(NA,length(alphas)*(length(epsilons)+1)), nrow = length(alphas))
# 
# set.seed(1)
# for (i in 1:length(alphas)){
#   out[i,]<- power_stat_estimated_alpha(alpha = alphas[i], Ns = 180,
#                                        mus = c(.5,.5,.5), sigma=.03, epsilons,
#                                        "absFstat", reps = 500, frac = .5) %>%
#     as.vector()
# }
# 
# d1 <- data.frame(x = alphas,
#                  y= out %>% as.vector(),
#                  eps = rep(c("Public",epsilons), rep(length(alphas), 1+length(epsilons)))
# )
# 
# write_csv(d1, path = "valid-pvals.csv")

#################################### plot #####################################
d1 <- read_csv("valid-pvals.csv")

p1 <- ggplot(d1, aes(x = x, y = y, col = as.factor(eps)) ) +
  geom_line() +
  ggtitle("Validity of p-values") +
  xlab(expression(alpha)) +
  ylab("Probability of Type I error") +
  labs(col = expression(epsilon)) +
  geom_abline(slope =1, pch = 4, linetype = 2) +
  scale_colour_Publication() +
  theme_Publication()

ggsave("valid-pvals.png", p1, device = "png", height = 5, width = 8, units = "in", dpi = "retina")
setwd(wd)