library(ggplot2)
library(purrr)
library(dplyr)
library(tidyverse)
library(rmutil)
library(tensor)
library(scales)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/datagen.R?token=AeAa3YlvA4FuDWcBEb9zfFjtKAEktgOqks5bjh7iwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/ggplot_theme.R?token=AeAa3Y14yJtVX39GdK5dfyZkEbs3M77Qks5bjh8qwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/helpers.R?token=AeAa3b71_HFAGlkpAdQ3JYt6SoPat3TZks5bjh9NwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/power_calc.R?token=AeAa3XDD7X9RgE7INdceLtzufMhKQLfTks5bjh9pwA%3D%3D', echo=FALSE)
source('https://raw.githubusercontent.com/diff-priv-ht/ANOVA-Group/master/Code/R%20code/R/stat_functions.R?token=AeAa3bvXCCClhPiibKASw6VCr8Ye4KVrks5bjh-AwA%3D%3D', echo=FALSE)

################################ data sim ######################################
wd <- getwd()
setwd(dirname(sys.frame(1)$ofile))


mus <- c(.4875, .5, .5125)
effect_size <- (seq(from = 8, to = 18, by = 2)) %>% map(~ (mus-median(mus))*. + median(mus))
sigmas <- seq(from = .010, to = .17, by = .02)
epsilons <- 1
reps <- 230
Ns <- 210

variables <- cross2(effect_size, sigmas) %>%
  unlist() %>%
  matrix(ncol = 4, byrow = TRUE)
sigmas2 <- variables[,4]
effect_size2 <- split(t(variables[,1:3]), rep(1:nrow(variables), each = 3))

set.seed(1)
power_N_effect_sigma1 <- map2(.x = effect_size2, .y = sigmas2,
                              ~ power_plot_estimated_optimized(Ns = Ns, mus = .x,
                                                               sigma = .y, 
                                                               epsilons = 1,
                                                               Statistic = "absFstat",
                                                               p = NULL, reps, 
                                                               frac = .7))
set.seed(1)
power_N_effect_sigma2 <- map2(.x = effect_size2, .y = sigmas2,
                              ~ power_plot_estimated_optimized(Ns = Ns, mus = .x,
                                                               sigma = .y,
                                                               epsilons = 1,
                                                               Statistic = "absVarstat",
                                                               p = NULL, reps,
                                                               frac = .4))

############################## data manipulation ###############################

threshold <- function(x, thresh = .8){
  ifelse(x > thresh , x, 0)
}

final1 <- rep(NA,length(sigmas)*(length(effect_size))) %>% matrix(nrow = length(sigmas))
final2 <- final1
intermediate1 <- 1:length(Ns)
intermediate2 <- intermediate1

for(i in 1:length(sigmas2)){
  intermediate1[i] <- ((power_N_effect_sigma1[[i]][[1]][,2] %>%
                          lapply(threshold, thresh = .85)) %>% unlist() )
  intermediate2[i] <- ((power_N_effect_sigma2[[i]][[1]][,2] %>%
                          lapply(threshold, thresh = .85)) %>% unlist() )
}
for(j in 1:length(sigmas)){
  final1[j,] <- intermediate1[((j-1)*6+1):(j*6)]
  final2[j,] <- intermediate2[((j-1)*6+1):(j*6)]
}

final1 <- final1 %>% tensor(1)
final2 <- final2 %>% tensor(1)

################################## plot ########################################

tensor_plot <- function(tensor, tensor2, margin = 3, marginlabs = c("1","2","3")){
  # takes a 3 tensor and plots slices of heatmaps
  out <- list()
  if (margin == 3){
    for(i in 1:dim(tensor)[3]){
      index_1 <- dim(tensor)[1]
      index_2 <- dim(tensor)[2]
      d <- data.frame(rowvec = rep(1:index_1, rep(index_2, index_1)),
                      colvec = rep(1:index_2, index_1),
                      power1 = tensor[,,i] %>% t() %>% as.vector(),
                      power2 = tensor2[,,i] %>% t() %>% as.vector()) %>%
        mutate(`Statistics with high power` = 
                 ifelse(!power1 & !power2, "neither",
                        ifelse(power1 & !power2, "absF",
                               ifelse(!power1 & power2, "absvarF", "both"))))
      out[[i]] <- ggplot(d, aes(x = colvec, y = rowvec)) +
        geom_tile(aes(fill = `Statistics with high power`)) +
        scale_fill_manual(values = c("neither" = "darkgray", 
                                     "absF"    = "red", 
                                     "absvarF" = "blue", 
                                     "both"    = "purple")) +
        theme_bw() +
        ggtitle(paste(marginlabs[3],"slice", i)) +
        xlab(marginlabs[2]) +
        ylab(marginlabs[1])
    }
  }
  if (margin == 2){
    for(i in 1:dim(tensor)[2]){
      index_1 <- dim(tensor)[3]
      index_2 <- dim(tensor)[1]
      d <- data.frame(rowvec = rep(1:index_1, rep(index_2, index_1)),
                      colvec = rep(1:index_2, index_1),
                      power1 = tensor[,i,]  %>% as.vector(),
                      power2 = tensor2[,i,] %>% as.vector()) %>%
        mutate(`Statistics with high power` = 
                 ifelse(!power1 & !power2, "neither",
                        ifelse(power1 & !power2, "absF",
                               ifelse(!power1 & power2, "absvarF", "both"))))
      out[[i]] <- ggplot(d, aes(x = colvec, y = rowvec)) +
        geom_tile(aes(fill = `Statistics with high power`)) +
        scale_fill_manual(values = c("neither" = "darkgray", 
                                     "absF"    = "red", 
                                     "absvarF" = "blue", 
                                     "both"    = "purple")) +
        theme_bw() +
        ggtitle(paste(marginlabs[2],"slice", i)) +
        xlab(marginlabs[1]) +
        ylab(marginlabs[3])
    }
  }
  if (margin == 1){
    for(i in 1:dim(tensor)[1]){
      index_1 <- dim(tensor)[2]
      index_2 <- dim(tensor)[3]
      d <- data.frame(rowvec = rep(1:index_1, rep(index_2, index_1)),
                      colvec = rep(1:index_2, index_1),
                      power1 = tensor[i,,] %>% t() %>% as.vector(),
                      power2 = tensor2[i,,] %>% t() %>% as.vector()) %>%
        mutate(`Statistics with high power` = 
                 ifelse(!power1 & !power2, "neither",
                        ifelse(power1 & !power2, "absF",
                               ifelse(!power1 & power2, "absvarF", "both"))))
      out[[i]] <- ggplot(d, aes(x = colvec, y = rowvec)) +
        geom_tile(aes(fill = `Statistics with high power`)) +
        scale_fill_manual(values = c("neither" = "darkgray", 
                                     "absF"    = "red", 
                                     "absvarF" = "blue", 
                                     "both"    = "purple")) +
        theme_bw() +
        ggtitle(paste(marginlabs[1],"slice", i)) +
        xlab(marginlabs[3]) +
        ylab(marginlabs[2])
    }
  }
  print(out)
}

figA <- tensor_plot(final1, final2, margin = 3,
                    marginlabs = c(expression(sigma),
                                   "Effect Size (deviation from grand mean)",
                                   "Database Size"))[[1]] +
  ggtitle(NULL) +
  scale_x_continuous(labels = function(x) 0.05*(x-1) + .2) +
  scale_y_continuous(labels = function(x) 0.02*(x-1) + .01) +
  scale_fill_discrete(labels = c(expression(F[1]), expression(G[1]), "both", "neither")) + 
  labs(fill = "Statistic with power > 0.8") 

# write_csv(figA$data, path = "f1-vs-g1.csv")
ggsave("fig-A.png", figA, device = "png", device = "png", height = 5, width = 8, units = "in", dpi = "retina")
setwd(wd)