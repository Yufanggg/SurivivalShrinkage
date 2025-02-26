set.seed(50234)
library(KMsurv)
library(survival)

SurvivalDataGenerator = function(n_samples, Betas, Design_matrix, BaselinePara, BaselineDistr = "exponential"){
  if (BaselineDistr == "exponential"){
    lambda = BaselinePara
    Baseline_survival_time = rexp(n_samples, rate = lambda)
  }
  
  else if (BaselineDistr == "weibull"){
    shape = BaselinePara[1]; scale = BaselinePara[2]
    Baseline_survival_time = rweibull(n_samples, shape = shape,  scale = scale)
    
  }
  
  Survival_time_covarite = Baseline_survival_time * exp(Design_matrix %*% Betas)
  
  # simulate censored data from a uniform distribution
  censtime <- runif(n_samples, 3, 6)
  
  # indicator variable
  status <- as.numeric(Survival_time_covarite <= censtime) # the sum of status is the number of event
  
  # observed time
  obstime <- Survival_time_covarite * status + censtime * (1 - status) 
  
  stan_data <- list(
    N = sum(status == 1),
    t = Survival_time_covarite[status == 1],
    N_cens = sum(status == 0),
    t_cens = censtime[status == 0], # Censoring time used in the simulation
    K = dim(Design_matrix)[2], # number of covariates,
    x = as.matrix(Design_matrix[status == 1,]),
    x_cens = as.matrix(Design_matrix[status == 0,]),
    covariate = Design_matrix,
    obstime = obstime,
    status = status
  )
  return (stan_data)
}


