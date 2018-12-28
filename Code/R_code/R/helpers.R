group_sizes <- function(D){
  D %>%
    filter(iteration==1) %>%
    group_by(group) %>%
    summarise(n_size = n()) %>%
    pull(n_size)
}

group_means_mat <- function(D, it){
  D %>%
    group_by(iteration, group) %>%
    mutate(mean_ij = mean(y)) %>%
    pull(mean_ij) %>%
    matrix(nrow = it, byrow = TRUE)
}

grand_means <- function(D){
  D %>%
    group_by(iteration) %>%
    summarise(mean = mean(y)) %>%
    pull(mean)
}

SSE_calc <- function(D, it){
  D %>%
    group_by(iteration, group) %>%
    mutate(ybar_j = mean(y),
           sqdiff = (y - ybar_j)^2) %>%
    pull(sqdiff) %>%
    matrix(ncol = it) %>%
    colSums()
}

SE_calc <- function(D, it){
  D %>%
    group_by(iteration, group) %>%
    mutate(ybar_j = mean(y),
           absdiff = abs(y - ybar_j)) %>%
    pull(absdiff) %>%
    matrix(ncol = it) %>%
    colSums()
}

Var2_calc <- function(D){
  D %>%
    group_by(iteration) %>%
    summarise(Var2 = (y-mean(y))^2 %>% sum) %>%
    pull(Var2)
}

Var1_calc <- function(D){
  D %>%
    group_by(iteration) %>%
    summarise(Var1 = abs(y-mean(y)) %>% sum) %>%
    pull(Var1)
}

VarP_calc <- function(D, p){
  D %>%
    group_by(iteration) %>%
    summarise(Var1 = (abs(y-mean(y))^p) %>% sum) %>%
    pull(Var1)
}


SPE_calc <- function(D, it, p){
  D %>%
    group_by(iteration, group) %>%
    mutate(ybar_j = mean(y),
           absdiff = abs(y - ybar_j)^p) %>%
    pull(absdiff) %>%
    matrix(ncol = it) %>%
    colSums()
}

#' helper function, omits NAs and finds mean of Bool vector to find power
na.omit.mean <- function(vec){ 
  sum(na.omit(vec))/length(vec)
}

#' helper function, finds critical points of null dists across the colums of a 
#' matrix of statistics distributed under the null hypothesis
crit <- function(M){
  apply(M, 2, quantile,probs = .95, na.rm = TRUE)
}

#` helper function to reduce RAM usage if running with high reps
combine_power <- function(input){ #expects list of many power matricies, with identical parameters
  power_mat <-  rep(NA, length(input)) %>% tensor(rep(NA,length(as.vector(input[[1]]))))
    for(i in 1:length(input)) {
      for(j in 1:ncol(input[[1]]))
      power_mat[i,] <- as.vector(input[[i]])
  }
  power_mat %>% apply(2, function(x){mean(x)}) %>%
    matrix(nrow = nrow(input[[1]]))
}


