set.seed(50234)
library(KMsurv)
library(survival)

SurvivalDataGenerator = function(n, distribution = "exponential", 
                                 anycovariate = "no", beta = "no"){
  if (distribution == "exponential" & anycovariate == "no") {
    # simulate survival time from an exponential distribution
    survtime <- rexp(n, 0.2)
    covariate <- NA
    K <- 0
  }
  else if (distribution == "exponential" & anycovariate == "binary"){
    # simulate survival time from an exponential distribution
    covariate <- factor(rep(c("F", "M"), each = n/2))
    lambdas = exp(beta * as.numeric(covariate))
    survtime <- sapply(lambdas, function(lambda) rexp(1, lambda))
    intercept <- matrix(1, nrow = n, ncol = 1)
    design_matrix <- cbind(intercept, covariate)
    K = 2
  }
  
  else if (distribution == "exponential" & anycovariate == "continuous"){
    covariate <- rnorm(1000)
    lambdas <- exp(beta * covariate)
    survtime <- sapply(lambdas, function(lambda) rexp(1, lambda))
    intercept <- matrix(1, nrow = n, ncol = 1)
    design_matrix <- cbind(intercept, covariate)
    K = 1
  }
  
  else if (distribution == "exponential_" & anycovariate == "continuous"){
    covariate <- rnorm(1000)
    lambdas <- exp(beta * covariate)
    survtime <- sapply(lambdas, function(lambda) rexp(1, lambda))
    design_matrix <- as.matrix(covariate)
    K = 1
  }
  
  # simulate censored data from a uniform distribution
  censtime <- runif(n, 3, 6)
  
  # indicator variable
  status <- as.numeric(survtime <= censtime) # the sum of status is the number of event
  
  # observed time
  obstime <- survtime * status + censtime * (1 - status) 
  
  stan_data <- list(
    N = sum(status == 1),
    t = survtime[status == 1],
    N_cens = sum(status == 0),
    t_cens = censtime[status == 0], # Censoring time used in the simulation
    K = K, # number of covariates,
    x = as.matrix(design_matrix[status == 1,]),
    x_cens = as.matrix(design_matrix[status == 0,]),
    covariate = covariate,
    obstime = obstime,
    status = status
  )
  return (stan_data)
}


