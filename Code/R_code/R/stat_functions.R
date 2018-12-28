#' Calculates an F statistic for each iteration, using a dataframe from datagen
#' 
#' @param D a dataframe from datagen
#' @param epsilon differential privacy parameter, 
#' @param frac fraction of differential privacy parameter to allocate to
#'  numerator
#' @examples
#' D <- datagen(N = 60, mus = c(4,5,6), sigma = 1,reps = 100, sizes = c(1,2,1))
#' D <- scale.to.unit(D,bounds=c(1,9))
#' Fstat(D,epsilon = 1, frac = .75)
#' @export
Fstat <- function(D, epsilon, frac = .5, p = NULL){
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                          # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #SSA:
  SSA <- (group_means_mat(D, it = it) - grand_means(D))^2 %>%   # see helpers.R
    rowSums()
  #SSE:
  SSE <- SSE_calc(D, it = it)                                   # see helpers.R
  #Noise
  if (epsilon > 0){
    SSA_noise <- rlaplace(length(epsilon)*it,
                          0,
                          (9+(5/N))/(frac*epsilon))
    SSE_noise <- rlaplace(length(epsilon)*it,
                          0,
                          7/((1-frac)*epsilon))
    ((SSA+SSA_noise)/(k-1))/((SSE+SSE_noise)/(N-k))
  }
  else{
    (SSA/(k-1))/(SSE/(N-k))
  }
}

#' Calculates an absolute value F statistic for each iteration, using a
#' dataframe from datagen
#' 
#' @param D a dataframe from datagen
#' @param epsilon differential privacy parameter
#' @param frac fraction of differential privacy parameter to allocate to
#'  numerator
#' @examples
#' D <- datagen(N = 60, mus = c(4,5,6), sigma = 1,reps = 100, sizes = c(1,2,1))
#' D <- scale.to.unit(D,bounds=c(1,9))
#' absFstat(D,epsilon = 1, frac = .75)
#' @export
absFstat <- function(D, epsilon, frac = .5, p = NULL){ 
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                          # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #SA:
  SA <- abs(group_means_mat(D, it = it) - grand_means(D)) %>%   # see helpers.R
    rowSums()
  #SE stuff:
  SE <- SE_calc(D, it = it)                                     # see helpers.R
  #Noise
  if (epsilon > 0){
    SA_noise <- rlaplace(length(epsilon)*it,
                         0,
                         4/(frac*epsilon))
    SE_noise <- rlaplace(length(epsilon)*it,
                         0,
                         3/((1-frac)*epsilon))
    ((SA+SA_noise)/(k-1))/((SE+SE_noise)/(N-k))
  }
  else{
    ((SA)/(k-1))/((SE)/(N-k))
  }
}

#' Calculates an F statistic and sigma estimate for each iteration and each 
#' epsilon, using a dataframe from datagen. Public statistics (epsilon =
#' infinity) are calculated by default and placed in the first column. Output
#' matrix rows are indexed on iteration number, and columns are indexed by
#' epsilon. Sigma estimates are indexed with their respective statistics on the
#' right half of the matrix.
#' 
#' @param D a dataframe from datagen
#' @param epsilon differential privacy parameter
#' @param frac fraction of differential privacy parameter to allocate to
#'  numerator
#' @examples
#' D <- datagen(N = 60, mus = c(4,5,6), sigma = 1,reps = 100, sizes = c(1,2,1))
#' D <- scale.to.unit(D,bounds=c(1,9))
#' Fstat_parallel(D,epsilon = 1, frac = .75)
#' @export
Fstat_parallel <- function(D, epsilons, frac = .5, p = NULL){
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                          # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #SSA:
  SSA <- (group_means_mat(D, it = it) - grand_means(D))^2 %>%   # see helpers.R
    rowSums()
  #SSE:
  SSE <- SSE_calc(D, it = it)                                   # see helpers.R
  #Noise
  SSA_noise <- rep(0,length(it)) %>% 
    rbind(rlaplace(length(epsilons)*it,0,(9+(5/N))/(frac*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  SSE_noise <- rep(0,length(it)) %>% 
    rbind(rlaplace(length(epsilons)*it,0,7/((1-frac)*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  ANS <- rep(NA,2*it*(1+length(epsilons))) %>% matrix(ncol = 2*(1+length(epsilons)))
  for (i in 1:(1+length(epsilons))){
    ANS[,i] <- ((SSA+SSA_noise[i,])/(k-1))/((SSE+SSE_noise[i,])/(N-k))
    ANS[,i+1+length(epsilons)] <- ((SSE+SSE_noise[i,])/(N-k)) %>% sqrt
  }
  ANS
}

#' Calculates an abs F statistic and sigma estimate for each iteration and each 
#' epsilon, using a dataframe from datagen. Public statistics (epsilon =
#' infinity) are calculated by default and placed in the first column. Output
#' matrix rows are indexed on iteration number, and columns are indexed by
#' epsilon. Sigma estimates are indexed with their respective statistics on the
#' right half of the matrix.
#' 
#' @param D a dataframe from datagen
#' @param epsilon differential privacy parameter
#' @param frac fraction of differential privacy parameter to allocate to
#'  numerator
#' @examples
#' D <- datagen(N = 60, mus = c(4,5,6), sigma = 1,reps = 100, sizes = c(1,2,1))
#' D <- scale.to.unit(D,bounds=c(1,9))
#' absFstat_parallel(D,epsilon = 1, frac = .75)
#' @export
absFstat_parallel <- function(D,epsilons, frac = .5, p = NULL){ 
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                          # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #SA:
  SA <- abs(group_means_mat(D, it = it) - grand_means(D)) %>%   # see helpers.R
    rowSums()
  #SE stuff:
  SE <- SE_calc(D, it = it)                                     # see helpers.R
  #Noise
  SA_noise <- rep(0,length(it)) %>% 
    rbind(rlaplace(length(epsilons)*it,0,4/(frac*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  SE_noise <- rep(0,length(it)) %>%
    rbind(rlaplace(length(epsilons)*it,0,3/((1-frac)*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  ANS <- rep(NA,2*it*(1+length(epsilons))) %>% matrix(nrow = it)
  for (i in 1:(1+length(epsilons))){
    ANS[,i] <- ((SA+SA_noise[i,])/(k-1))/((SE+SE_noise[i,])/(N-k))
    ANS[,i+1+length(epsilons)] <- ((SE+SE_noise[i,])/(N-k))*sqrt(pi/2)
  }
  ANS
}


### Variance Statistics

Varstat <- function(D,epsilon, frac = .5, p = NULL){
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                          # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #Var2 stuff:
  Var2 <- Var2_calc(D)                                          # see helpers.R
  #SSE stuff:
  SSE <- SSE_calc(D, it = it)                                   # see helpers.R
  #Noise
  if (epsilon > 0){
    Var2_noise <- rlaplace(length(epsilon)*it,0,(5+(5/N)+1/(N^2))/(frac*epsilon))
    SSE_noise <- rlaplace(length(epsilon)*it,0,7/((1-frac)*epsilon))
    ((Var2+Var2_noise)/(N-1))/((SSE+SSE_noise)/(N-k))
  }
  else{
    ((Var2)/(N-1))/((SSE)/(N-k))
  }
}

absVarstat <- function(D,epsilon, frac = .5, p = NULL){
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                          # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #Var1:
  Var1 <- Var1_calc(D)                                          # see helpers.R
  #SE stuff:
  SE <- SE_calc(D, it = it)                                     # see helpers.R
  #Noise
  if (epsilon > 0){
    Var1_noise <- rlaplace(length(epsilon)*it,0,(2-2/N)/(frac*epsilon))
    SE_noise <- rlaplace(length(epsilon)*it,0,3/((1-frac)*epsilon))
    ((Var1+Var1_noise)/(N-1))/((SE+SE_noise)/(N-k))
  }
  else{
    ((Var1)/(N-1))/((SE)/(N-k))
  }
}

Varstat_parallel <- function(D,epsilons, frac = .5, p = NULL){
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                          # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #Var2 stuff:
  Var2 <- Var2_calc(D)                                          # see helpers.R
  #SSE stuff:
  SSE <- SSE_calc(D, it = it)                                   # see helpers.R
  #Noise
  Var2_noise <- rep(0,length(it)) %>% 
    rbind(rlaplace(length(epsilons)*it,0,(5+(5/N)+1/(N^2))/(frac*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  SSE_noise <- rep(0,length(it)) %>% 
    rbind(rlaplace(length(epsilons)*it,0,7/((1-frac)*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  ANS <- rep(NA,2*it*(1+length(epsilons))) %>% matrix(ncol = 2*(1+length(epsilons)))
  for (i in 1:(1+length(epsilons))){
    ANS[,i] <- ((Var2+Var2_noise[i,])/(N-1))/((SSE+SSE_noise[i,])/(N-k))
    ANS[,i+1+length(epsilons)] <- ((Var2+Var2_noise[i,])/(N-1)) %>% sqrt
  }
  ANS
}

absVarstat_parallel <- function(D,epsilons, frac = .5, p = NULL){
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                          # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #Var1:
  Var1 <- Var1_calc(D)                                          # see helpers.R
  #SE stuff:
  SE <- SE_calc(D, it = it)                                     # see helpers.R
  #Noise
  Var1_noise <- rep(0,length(it)) %>% 
    rbind(rlaplace(length(epsilons)*it,0,(2-2/N)/(frac*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  SE_noise <- rep(0,length(it)) %>% 
    rbind(rlaplace(length(epsilons)*it,0,3/((1-frac)*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  ANS <- rep(NA,2*it*(1+length(epsilons))) %>% matrix(ncol = 2*(1+length(epsilons)))
  for (i in 1:(1+length(epsilons))){
    ANS[,i] <- ((Var1+Var1_noise[i,])/(N-1))/((SE+SE_noise[i,])/(N-k))
    ANS[,i+1+length(epsilons)] <- ((Var1+Var1_noise[i,])/(N-1))*sqrt(pi/2)
  }
  ANS
}

# Fp Statistic

Fpstat <- function(D, epsilon, frac = .5, p = 1){ 
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                           # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #SPA:
  SPA <- (abs(group_means_mat(D, it = it) - grand_means(D))^p) %>%
    rowSums()                                                    # see helpers.R
  #SPE stuff:
  SPE <- SPE_calc(D, it = it, p = p)                             # see helpers.R
  #Noise
  if(p < 1){
    SPA_sensitivity <- ((N*(3/N)^p)+1)
    SPE_sensitivity <- ((2*(N/2)^(1-p))+1)
  }
  else{
    SPA_sensitivity <- (N * (1-((1 - (3/N))^p))) + 1
    SPE_sensitivity <- (N * (1-((1 - (2/N))^p))) + 1
  }
  if (epsilon > 0){
    SPA_noise <- rlaplace(length(epsilon)*it,
                         0,
                         SPA_sensitivity/(frac*epsilon))
    SPE_noise <- rlaplace(length(epsilon)*it,
                         0,
                         SPE_sensitivity/((1-frac)*epsilon))
    ((SPA+SPA_noise)/(k-1))/((SPE+SPE_noise)/(N-k))
  }
  else{
    ((SPA)/(k-1))/((SPE)/(N-k))
  }
}
  
Fpstat_parallel <- function(D,epsilons, frac = .5, p = 1){ 
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                           # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #SPA:
  SPA <- (abs(group_means_mat(D, it = it) - grand_means(D))^p) %>%
    rowSums()                                                    # see helpers.R
  #SPE stuff:
  SPE <- SPE_calc(D, it = it, p = p)                             # see helpers.R
  #Noise
  if(p < 1){
    SPA_sensitivity <- ((N*(3/N)^p)+1)
    SPE_sensitivity <- ((2*(N/2)^(1-p))+1)
  }
  else{
    SPA_sensitivity <- (N * (1-((1 - (3/N))^p))) + 1
    SPE_sensitivity <- (N * (1-((1 - (2/N))^p))) + 1
  }
  SPA_noise <- rep(0,length(it)) %>% 
    rbind(rlaplace(length(epsilons)*it,0,SPA_sensitivity/(frac*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  SPE_noise <- rep(0,length(it)) %>%
    rbind(rlaplace(length(epsilons)*it,0,SPE_sensitivity/((1-frac)*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  ANS <- rep(NA,2*it*(1+length(epsilons))) %>% matrix(nrow = it)
  for (i in 1:(1+length(epsilons))){
    ANS[,i] <- ((SPA+SPA_noise[i,])/(k-1))/((SPE+SPE_noise[i,])/(N-k))
    ANS[,i+1+length(epsilons)] <- ((SPE+SPE_noise[i,])/(N-k))*sqrt(pi/2)
  }
  ANS
}


Varpstat <- function(D, epsilon, frac = .5, p = 1){ 
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                           # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #SPV:
  SPV <- VarP_calc(D, p = p)
  #SPE stuff:
  SPE <- SPE_calc(D, it = it, p = p)                             # see helpers.R
  #Noise
  if(p < 1){
    SPV_sensitivity <- 1 + (N-1)/(N^p)
    SPE_sensitivity <- ((2*(N/2)^(1-p))+1)
  }
  else{
    SPV_sensitivity <- (N - 1)*(1-((1-(1/N))^p)) + 1
    SPE_sensitivity <- (N * (1-((1 - (2/N))^p))) + 1
  }
  if (epsilon > 0){
    SPV_noise <- rlaplace(length(epsilon)*it,
                          0,
                          SPV_sensitivity/(frac*epsilon))
    SPE_noise <- rlaplace(length(epsilon)*it,
                          0,
                          SPE_sensitivity/((1-frac)*epsilon))
    ((SPV+SPV_noise)/(k-1))/((SPE+SPE_noise)/(N-k))
  }
  else{
    ((SPV)/(k-1))/((SPE)/(N-k))
  }
}

Varpstat_parallel <- function(D, epsilons, frac = .5, p = 1){ 
  D$y <- D$y %>% pmin(1) %>% pmax(0)
  it <- max(D$iteration)
  ns <- group_sizes(D)                                           # see helpers.R
  k <- length(ns)
  N <- sum(ns)
  #SPV:
  SPV <- VarP_calc(D, p = p)                                     # see helpers.R
  #SPE stuff:
  SPE <- SPE_calc(D, it = it, p = p)                             # see helpers.R
  #Noise
  if(p < 1){
    SPV_sensitivity <- 1 + (N-1)/(N^p)
    SPE_sensitivity <- ((2*(N/2)^(1-p))+1)
  }
  else{
    SPV_sensitivity <- (N - 1)*(1-((1-(1/N))^p)) + 1
    SPE_sensitivity <- (N * (1-((1 - (2/N))^p))) + 1
  }
  SPV_noise <- rep(0,length(it)) %>% 
    rbind(rlaplace(length(epsilons)*it,0,SPV_sensitivity/(frac*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  SPE_noise <- rep(0,length(it)) %>%
    rbind(rlaplace(length(epsilons)*it,0,SPE_sensitivity/((1-frac)*epsilons)) %>%
            matrix(nrow = length(epsilons)))
  ANS <- rep(NA,2*it*(1+length(epsilons))) %>% matrix(nrow = it)
  for (i in 1:(1+length(epsilons))){
    ANS[,i] <- ((SPV+SPV_noise[i,])/(k-1))/((SPE+SPE_noise[i,])/(N-k))
    ANS[,i+1+length(epsilons)] <- ((SPV+SPV_noise[i,])/(N-k))*sqrt(pi/2)
  }
  ANS
}

