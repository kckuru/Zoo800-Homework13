###################
### Homework 13 ###
###################

# Group members: Keeley Kuru
# Date: 11/25/25

# Data from classmate (Homework 11)
# In this simulated dataset, brook trout activity (movements/hr) declines as stream temperature increases. Streams with cold-water refugia have higher baseline activity and a weaker decline in activity with increasing temperature compared to streams without refugia. The ANCOVA model tests whether the slopes of activity–temperature relationships differ between the two stream types, answering this ecological question: Do brook trout with cold-water refugia maintain higher activity levels as temperatures rise compared to those without refugia?
  
set.seed(123)

# intercept for streams with cold refugia
alpha_refugia <- 10     
# lower intercept for streams without refugia
alpha_no_ref  <- 6  
# slope for refugia streams
beta_refugia  <- -0.4   
#slope for no-refugia streams
beta_no_ref   <- -0.8   

n_per_group   <- 50    
sdlog         <- 0.5

# Create the two-level categorical variable
group <- rep(c("ColdRefugia", "NoRefugia"), each = n_per_group)

# Continuous predictor: temperature (°C)
temp <- runif(2 * n_per_group, 10, 22)  

# Simulate lognormal errors 
err_raw <- rlnorm(2 * n_per_group, meanlog = 0, sdlog = sdlog)
err <- err_raw - mean(err_raw)

# Generate activity data (movements per hour)
activity <- ifelse(group == "ColdRefugia",
                   alpha_refugia + beta_refugia * temp + err,
                   alpha_no_ref  + beta_no_ref  * temp + err)

# Combine into dataframe
data <- data.frame(temp, group, activity)

# Fit ANCOVA model (interaction between temperature and group)
fit <- lm(activity ~ temp * group, data = data)
summary(fit)

# ===== Objective 1 ===== #
df <- subset(data, group == "ColdRefugia")

x <- df$temp
y <- df$activity

beta1_hat <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
beta0_hat <- mean(y) - beta1_hat * mean(x)

beta0_hat
beta1_hat

# ===== Objective 2 ===== #
# Objective 2a
# Define RSS function
RSS <- function(b0, b1, x, y) {
  sum((y - (b0 + b1 * x))^2)
}

b0_vals <- seq(-10, 25, by = 0.1)
b1_vals <- seq(-3, 1, by = 0.1)

grid <- expand.grid(b0 = b0_vals, b1 = b1_vals)

grid$RSS <- mapply(RSS, grid$b0, grid$b1, MoreArgs = list(x = x, y = y))

best_grid <- grid[which.min(grid$RSS), ]
best_grid

# Objective 2b
RSS_opt <- function(par, x, y) {
  b0 <- par[1]
  b1 <- par[2]
  sum((y - (b0 + b1 * x))^2)
}

start <- c(0, 0)

fit_optim <- optim(start, RSS_opt, x = x, y = y)

fit_optim$par          # b0 and b1
fit_optim$convergence  # 0 = good

# Objective 2c
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

# ===== Objective 3 ===== #
# Objective 3a
b0_vals <- seq(-10, 25, by = 0.1)
b1_vals <- seq(-3, 1, by = 0.1)
grid <- expand.grid(b0 = b0_vals, b1 = b1_vals)
grid$RSS <- mapply(RSS, grid$b0, grid$b1, MoreArgs = list(x = x, y = y))
best_grid <- grid[which.min(grid$RSS), ]
best_grid

# Objective 3b
fit_optim <- optim(c(0, 0), RSS_opt, x = x, y = y)
fit_optim$par
fit_optim$convergence

# Objective 3c
starts <- list(c(0,0), c(20,-2), c(-10,1), c(5,5))

results <- lapply(starts, function(s) {
  optim(par = s, fn = RSS_opt, x = x, y = y)$par
})

names(results) <- c("start1","start2","start3","start4")
results

# ===== Objective 4 ===== #
# Objective 4a
# The slope and intercept estimates obtained were all very similar.
# The grid search returned an intercept of approximately 10.1 and a slope of –0.4, which is close to the other methods but slightly coarser due to
#    the 0.1 step size used in the parameter grid. In contrast, both the optim() routine and the analytical OLS solution produced nearly identical estimates (intercept ≈ 10.29, slope ≈ –0.412), 
#    differing only in the sixth decimal place due to numerical precision.
# Overall, the three approaches yield consistent results, confirming that the linear regression parameters are stable and that the optimization successfully located the same minimum identified by the analytical solution.
