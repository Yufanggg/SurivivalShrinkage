set.seed(50234)
library(KMsurv)
library(survival)

SurvivalDataGenerator = function(n, distribution = "exponential", 
                                 anycovariate = "no", beta = "no"){
  if (distribution == "exponential" & anycovariate == "no") {
    # simulate survival time from an exponential distribution
    survtime <- rexp(n, 0.2)
    covariate <- NA
  }
  else if (distribution == "exponential" & anycovariate == "binary"){
    # simulate survival time from an exponential distribution
    survtime <- c(rexp(n/2, 0.2), rexp(n/2, 0.2 + beta))
    covariate <- factor(rep(c("F", "M"), each = n/2))
  }
  
  else if (distribution == "exponential" & anycovariate == "continuous"){
    covariate = rnorm(1000)
    lambdas = exp(beta * covariate)
    survtime <- sapply(lambdas, function(lambda) rexp(1, lambda))
  }
  
  # simulate censored data from an exponential distribution
  censtime <- runif(n, 3, 6)
  
  # indicator variable
  status <- as.numeric(survtime <= censtime) # the sum of status is the number of event
  
  # observed time
  obstime <- survtime * status + censtime * (1 - status) 
  
  stan_data <- list(
    N = n,
    t = obstime,
    N_cens = sum(status == 0),
    t_cens = censtime, # Censoring time used in the simulation
    covariate = covariate
  )
  return (stan_Data)
}


