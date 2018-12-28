#' generates N draws from differing normal distributions with same variance for
#' each iteration, parallelized with iteration counts.
#' 
#' @param N database size
#' @param mus vector of means for each group
#' @param sigma standard deviation of each group
#' @param reps number of runs/iterations
#' @param p vector of group sizes, uniform by default
#' @examples 
#' datagen(N = 60, mus = c(1,2,3), sigma = 1,reps = 100, sizes = c(15,30,15))
#' @export
datagen <- function(N, mus, sigma, reps = 1 , sizes = rep(1,length(mus))){
  if (length(sizes) != length(mus)) 
    stop("length's of means and relative group sizes differ")
  sizes <- sizes/sum(sizes)
  data.frame(y=rnorm(N*reps,
                     mean = rep(rep(mus, round(N*sizes)), reps),
                     sd = rep(sigma, N*reps)),
             group=(rep(rep(1:length(sizes), round(N*sizes)), reps)),
             iteration=rep(1:reps,rep(N,reps))
  )
}


#' scales data to the unit interval and then truncates data by bounds
#' 
#' @param D a dataframe from datagen
#' @param bound a vector of a lower and upper bounds
#' @examples
#' D <- datagen(N = 60, mus = c(4,5,6), sigma = 1,reps = 100)
#' scale.to.unit(D,c(0,10))
#' @export
scale_to_unit <- function(D, bounds){
  D$y <- ((D$y - min(bounds))/abs(bounds[1]-bounds[2])) %>%
    pmin(1) %>%
    pmax(0)
  D
}
