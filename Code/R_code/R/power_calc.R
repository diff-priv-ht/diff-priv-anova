#' calculates power for each N,epsilon pair, while generating null hypothesis 
#' data using true value of sigma
#'
#'
power_calc <- function(Ns, mus, sigma, epsilons, Statistic= NULL, p = NULL, reps = 1,
                       frac = .5, sizes = rep(1,length(mus))){
  if(tolower(Statistic) == "fstat"){
    Stat <- Fstat_parallel
  }
  if(tolower(Statistic) == "absfstat"){
    Stat <- absFstat_parallel
  }
  if(tolower(Statistic) == "varstat"){
    Stat <- Varstat_parallel
  }
  if(tolower(Statistic) == "absvarstat"){
    Stat <- absVarstat_parallel
  }
  if(tolower(Statistic) == "varpstat"){
    Stat <- Varpstat_parallel
  }
  if(tolower(Statistic) == "fpstat"){
    Stat <- Fpstat_parallel
  }
  crit_mat <- Ns %>%
    map(~ datagen(., rep(.5,length(mus)), sigma, reps, sizes)) %>% 
    map(~ Stat(., epsilons, frac, p)[,1:(1+length(epsilons))]) %>%
    sapply(crit) %>%
    t()
  altstat <- Ns %>%
    map(~ datagen(., mus, sigma, reps, sizes))  %>%
    map(~ Stat(., epsilons, frac, p)[,1:(1+length(epsilons))])
  power_mat <- rep(NA, length(Ns)*(1+length(epsilons))) %>%
    matrix(nrow = length(Ns))
  for (i in 1:(length(Ns))){
    for (j in 1:(1+length(epsilons))){
      power_mat[i,j] <- (altstat[[i]][,j] > crit_mat[i,j]) %>%
        na.omit.mean()
    }
  }
  power_mat
}



#' calculates power for each N,epsilon pair, while generating null hypothesis
#' data using estimate for sigma
#'
power_calc_estimated <- function(Ns, mus, sigma, epsilons, Statistic = NULL,
                                 p = NULL, reps = 1, frac = .5,
                                 sizes = rep(1,length(mus))){
  Stat <- Varpstat
  Stat_parallel <- Varpstat_parallel
  if(tolower(Statistic) == "fstat"){
    Stat <- Fstat
    Stat_parallel <- Fstat_parallel
  }
  if(tolower(Statistic) == "absfstat"){
    Stat <- absFstat
    Stat_parallel <- absFstat_parallel
  }
  if(tolower(Statistic) == "varstat"){
    Stat <- Varstat
    Stat_parallel <- Varstat_parallel
  }
  if(tolower(Statistic) == "absvarstat"){
    Stat <- absVarstat
    Stat_parallel <- absVarstat_parallel
  }
  epsilons2 <- c(-1, epsilons)       # negative epsilon indicates public stat
  Reject_mat <- rep(NA, reps*length(epsilons2)) %>%
    matrix(nrow = reps)
  power_mat <- rep(NA, length(Ns)*length(epsilons2)) %>%
    matrix(nrow = length(Ns))
  for (i in 1:length(Ns)){
    altstat <- datagen(Ns[i], mus, sigma, reps, sizes) %>%
      Stat_parallel(epsilons, frac, p)
    sdevs <- altstat[,(2+length(epsilons)):(2*(1+length(epsilons)))]
    nullstat <- rep(NA, (1+length(epsilons))*reps) %>%
      matrix(nrow = reps)
    for (j in 1:length(epsilons2)){
      crits <- sdevs[,j] %>%
        map(~ datagen(Ns[i], rep(.5, length(mus)), ., 300, sizes)) %>%
        map(~ Stat(., epsilons2[j], frac, p)) %>%
        map(~ quantile(., probs = .95, na.rm = TRUE)) %>%
        unlist()
      Reject_mat[,j] <- (altstat[,j] > crits)
      power_mat[i,] <- apply(Reject_mat, 2, na.omit.mean)
    }
  }
  power_mat
}


power_plot <- function(Ns, mus, sigma, epsilons, Statistic = 1, p = 1, reps,
                       frac=.5, sizes = rep(1, length(mus))){ 
  #plot power against database size, for each N,epsilon pair
  pow <- power_calc(Ns, mus, sigma, epsilons, Statistic, p, reps, frac, sizes) 
  d <- data.frame(N= rep(Ns,rep(1+length(epsilons),length(Ns))),
                  eps = rep(c("None",epsilons),length(Ns)),
                  Power=pow %>% t %>% as.vector())
  list(pow,
       ggplot(d, aes(x=N, y=Power, col = as.factor(eps))) +
         geom_line() +xlab("Database Size") + labs(col="Epsilon") +
         ggtitle(paste("Power", "stat exponent = ", p))
  )
}

power_plot_estimated <- function(Ns, mus, sigma, epsilons, Statistic = 1,
                                 p = 1, reps, frac=.5){ 
  #plot power against database size, for each N,epsilon pair, using estimated sd
  pow <- power_calc_estimated(Ns, mus, sigma, epsilons, Statistic, p = 1, reps,
                              frac, sizes = rep(1, length(mus))) 
  d <- data.frame(N= rep(Ns,rep(1+length(epsilons),length(Ns))),
                  eps = rep(c("None",epsilons),length(Ns)),
                  Power=pow %>% t() %>% as.vector())
  list(pow,
       ggplot(d, aes(x=N, y=Power, col = as.factor(eps))) +
         geom_line() +xlab("Database Size") + labs(col="Epsilon"))
}

power_plot_p <- function(Ns, mus, sigma, epsilon, Statistic = 1, ps, reps,
                         frac = .5, sizes = rep(1, length(mus))){
  pow <- ps %>% map(~ power_calc(Ns, mus, sigma, epsilon, Statistic, ., reps,
                                 frac)[,2])
  d <- data.frame(N= rep(Ns,length(ps)),
                  exponent = rep(ps,rep(length(Ns), length(ps))),
                  Power=pow %>% unlist)
  ggplot(d, aes(x = N, y = Power, col = as.factor(exponent))) + geom_line() + 
    xlab("Database Size") + labs(col="exponent") +
    ggtitle(paste("Power for epsilon =", epsilon))
}

power_plot_frac <- function(Ns, mus, sigma, epsilon, Statistic = 1, p = 1, reps,
                            fracs = c(.2,.5,.8), sizes = rep(1, length(mus))){
  pow <- fracs %>% 
    map(~ power_calc_estimated(Ns, mus, sigma, epsilon, Statistic, p, reps, .)[,2])
  d <- data.frame(N= rep(Ns,length(fracs)),
                  eps_frac = rep(fracs,rep(length(Ns), length(fracs))),
                  Power=pow %>% unlist)
  ggplot(d, aes(x = N, y = Power, col = as.factor(eps_frac))) + geom_line() + 
    xlab("Database Size") + labs(col="fraction of epsilon on numerator") +
    ggtitle(paste("Power for epsilon =", epsilon))
}

combine_power <- function(input){ #expects list of many power matricies, with identical parameters
  power_mat <-  rep(NA, length(input)) %>% tensor(rep(NA,length(as.vector(input[[1]]))))
  for(i in 1:length(input)) {
    for(j in 1:ncol(input[[1]]))
      power_mat[i,] <- as.vector(input[[i]])
  }
  power_mat %>% apply(2, function(x){mean(x)}) %>%
    matrix(nrow = nrow(input[[1]]))
}

power_plot_estimated_optimized <- function(Ns, mus, sigma, epsilons,
                                           Statistic = "AbsFstat", p = NULL,
                                           reps, frac = .5){
  pow <- rep(100,round(reps/100)) %>% 
    map(~ power_calc_estimated(Ns, mus, sigma, epsilons, Statistic, p, ., frac)) %>%
    combine_power()
  d <- data.frame(N= rep(Ns,rep(1+length(epsilons),length(Ns))),
                  eps = rep(c("None",epsilons),length(Ns)),
                  Power=pow %>% t %>% as.vector())
  list(pow,
       ggplot(d, aes(x=N, y=Power, col = as.factor(eps))) +
         geom_line() +xlab("Database Size") + labs(col="Epsilon") +
         ggtitle(paste("Power for", Statistic, "mus = ", mus, ", sigma =", sigma))
  )
}