set.seed(50234)
library(KMsurv)
library(survival)

### Without any covariate
# simulate survival time from an exponential distribution
survtime <- rexp(500, 0.2)

# simulate censored data from an exponential distribution
censtime <- rexp(500, 0.1)

# indicator variable
status <- as.numeric(survtime <= censtime) # the sum of status is the number of event

# observed time
obstime <- survtime*status + censtime*(1-status) # the same with pmin(survtime, censtime)

# plot the survival function S(t), Kaplann meier
plot(survfit(Surv(obstime, status) ~ 1), conf.int = F, mark.time = F)
# true survival function
y <- seq(0, 30, by = 0.1)
truesurv <- exp(-0.2*y)
lines(y, truesurv, type = "l", col = "green")

# plot F(t)
plot(survfit(Surv(obstime, status) ~ 1), conf.int = F, mark.time = F, fun = "event")
lines(y, 1 - truesurv, type = "l", col = "red")


### Having a binary covariate
# simulate survival time from an exponential distribution
survtime <- c(rexp(500, 0.2), rexp(500, 0.5))

# simulate censored data from an exponential distribution
censtime <- runif(1000, 3, 6)

# indicator variable
status <- as.numeric(survtime <= censtime) # the sum of status is the number of event

# observed time
obstime <- survtime*status + censtime*(1-status) # the same with pmin(survtime, censtime)

simulated_data <- data.frame(
  obstime = obstime,
  status = status,
  group = factor(rep(c("F", "M"), each = 500))
)

# plot the survival function S(t), Kaplann meier
plot(survfit(Surv(obstime, status) ~  simulated_data$group), conf.int = F, mark.time = F, data = simulated_data)
coxph(Surv(obstime, status) ~  simulated_data$group)

### Having a continuous covariate
covariate = rnorm(1000)
betas = exp(0.5 * covariate)
survtime <- sapply(betas, function(beta) rexp(1, beta))

# simulate censored data from an exponential distribution
censtime <- runif(1000, 3, 6)

# indicator variable
status <- as.numeric(survtime <= censtime) # the sum of status is the number of event

# observed time
obstime <- survtime*status + censtime*(1-status) # the same with pmin(survtime, censtime)

coxph(Surv(obstime, status) ~  covariate)

