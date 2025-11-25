###################
### Homework 13 ###
###################

# Group members: Keeley Kuru
# Date: 11/25/25

# ==============================
# Load the dragon dataset
# ==============================
dragon <- read.csv("dragon_data.csv")

# Define x and y
x <- dragon$size
y <- dragon$acres_on_fire


# ==============================
# Objective 1 — Analytical OLS
# ==============================
beta1_hat <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
beta0_hat <- mean(y) - beta1_hat * mean(x)

beta0_hat
beta1_hat
# These give the OLS slope and intercept for acres_on_fire ~ size.
# The intercept is negative, which has no biological meaning — dragons cannot burn negative acres.
# This shows a strong positive relationship: bigger dragons burn more land.

# ==============================
# Objective 2
# ==============================

# ---- Objective 2a: Grid Search ----

# RSS function
RSS <- function(b0, b1, x, y) {
  sum((y - (b0 + b1*x))^2)
}

# Parameter grid
b0_vals <- seq(-10, 25, by = 0.1)
b1_vals <- seq(-3, 3, by = 0.1)   # expanded slope range for safety

grid <- expand.grid(b0 = b0_vals, b1 = b1_vals)

grid$RSS <- mapply(RSS, grid$b0, grid$b1, MoreArgs = list(x = x, y = y))

best_grid <- grid[which.min(grid$RSS), ]
best_grid
# This gives the b0 and b1 that minimize RSS over the grid.
# The intercept (β₀) from OLS is around –1.38, but the grid only includes b0 from –10 to 25 by 0.1, 
#    which includes –1.4 but the minimum RSS happened at 0.2 instead.
# WHY? -> the best combination on the 0.1 grid happens to be slightly shifted due to rounding.

# ---- Objective 2b: Optimization with optim() ----

RSS_opt <- function(par, x, y) {
  b0 <- par[1]
  b1 <- par[2]
  sum((y - (b0 + b1*x))^2)
}

start <- c(0, 0)

fit_optim <- optim(start, RSS_opt, x = x, y = y)

fit_optim$par          # b0 and b1
fit_optim$convergence  # 0 = good
# These match the OLS estimates closely!
# The optim() function found the same minimum RSS as OLS, confirming the results.

# ---- Objective 2c: Multiple starting values ----

starts <- list(
  c(0, 0),
  c(20, -2),
  c(-10, 1),
  c(5, 5)
)

results <- lapply(starts, function(s) {
  optim(s, RSS_opt, x = x, y = y)$par
})

names(results) <- c("start1", "start2", "start3", "start4")
results
# All starting values converged to the same solution, confirming the global minimum.

# ==============================
# Objective 3 — Maximum Likelihood Estimation
# ==============================

# ---- Objective 3a: Grid search MLE ----

# Negative log-likelihood function
NLL <- function(b0, b1, sigma, x, y) {
  if(sigma <= 0) return(Inf)  # sigma must be positive
  n <- length(y)
  residuals <- y - (b0 + b1 * x)
  -sum(dnorm(residuals, mean = 0, sd = sigma, log = TRUE))
}

# Parameter grids
b0_vals <- seq(-3, 1, by = 0.01)
b1_vals <- seq(1.1, 1.6, by = 0.01)
sigma_vals <- seq(0.1, 5, by = 0.1)

grid <- expand.grid(b0 = b0_vals, b1 = b1_vals, sigma = sigma_vals)
grid$NLL <- mapply(NLL, grid$b0, grid$b1, grid$sigma, MoreArgs = list(x = x, y = y))

best_grid <- grid[which.min(grid$NLL), ]
best_grid
# Gives the MLE estimates of beta0, beta1, and sigma

# ---- Objective 3b: optim() MLE ----

NLL_opt <- function(par, x, y) {
  b0 <- par[1]
  b1 <- par[2]
  sigma <- par[3]
  if(sigma <= 0) return(Inf)
  -sum(dnorm(y, mean = b0 + b1 * x, sd = sigma, log = TRUE))
}

start <- c(0, 1.3, 1)  # initial guesses for b0, b1, sigma
fit_MLE <- optim(start, NLL_opt, x = x, y = y, method = "L-BFGS-B", lower = c(-Inf, -Inf, 1e-6))

fit_MLE$par
fit_MLE$convergence  # 0 = success

# ---- Objective 3c: multiple starting values ----

starts <- list(c(0,1,1), c(0.5,1.2,0.5), c(-1,1.5,2), c(0,1.4,0.1))

results <- lapply(starts, function(s) {
  optim(s, NLL_opt, x = x, y = y, method = "L-BFGS-B", lower = c(-Inf,-Inf,1e-6))$par
})

names(results) <- c("start1","start2","start3","start4")
results
# All starting values should converge to the same MLE solution

# ==============================
# Objective 4 — Interpretation
# ==============================

# The slope and intercept estimates from grid search, optim(), 
# and the analytical OLS solution should all be very similar.
