SurvivalDataGenerator = function(n, baseline = "exponential", betas, X){
  # Load necessary library
  library(survival)
  
  # Define parameters
  n <- 100  # Sample size
  beta <- 0.1 # Rate parameter for exponential distribution
  
  # Generate covariates
  set.seed(42)
  covariate <- rnorm(n)
  rates <- exp(beta * covariate)
  
  # Simulate survival times
  survival_times <- sapply(rates, function(rate) rexp(1, rate = rate))
  
  # Introduce censoring
  censoring_times <- rexp(n, rate = beta)
  observed_times <- pmin(survival_times, censoring_times)
  status <- as.numeric(survival_times <= censoring_times)
  
  # Combine into a data frame
  simulated_data <- data.frame(
    time = as.numeric(observed_times),
    status = status,
    covariate = covariate
  )
  
  # View the first few rows
  head(simulated_data)
}