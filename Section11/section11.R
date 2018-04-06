
# Clustering simulation ----

# General setup ----
# Options
options(stringsAsFactors = F)
# Packages
library(data.table)
library(magrittr)
library(MASS)
library(lfe)
library(parallel)
library(ggplot2)
library(viridis)
# My ggplot2 theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  # panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey95", size = 0.3),
  panel.grid.major = element_line(color = "grey95", size = 0.3),
  panel.grid.minor = element_line(color = "grey95", size = 0.3),
  legend.key = element_blank())

# Function: Generate data ----
sim_fun <- function(i, n_g, g, var_cov, beta) {
  # Start creating the dataset: generate X
  sim_dt <- data.table(x = rnorm(n_g * g))
  # Generate the disturbances (in vector form)
  sim_dt[, v := mvrnorm(n = g, mu = rep(0, n_g), Sigma = var_cov) %>%
    # Rows are for a single cluster, so we transpose
    t() %>% c()]
  # Calcualte y; add group ID
  sim_dt[, `:=`(
    y = beta[1] + beta[2] * x + v,
    group_id = rep(1:g, each = n_g)
    )]
  # Spherical-error inference
  est_sph <- felm(y ~ x, data = sim_dt) %>%
    summary() %>% coef() %>% extract(2, 1:3)
  #  inference
  est_het <- felm(y ~ x, data = sim_dt) %>%
    summary(robust = T) %>% coef() %>% extract(2, 1:3)
  # Cluster-robust inference
  est_cl <- felm(y ~ x | 0 | 0 | group_id, data = sim_dt) %>%
    summary() %>% coef() %>% extract(2, 1:3)
  # Results data.table
  res_dt <- data.table(rbind(est_sph, est_het, est_cl))
  setnames(res_dt, c("est", "se", "t_stat"))
  res_dt[, `:=`(
    method = c("spherical", "het. robust", "cluster robust"),
    iter = i)]
  # Return results
  return(res_dt)
}

# Simulation parameters ----
# Observations per group
n_g <- 30
# Number of groups
g <- 50
# Variance-covariance matrix (within a cluster)
var_cov <- diag(n_g)
# Define beta
beta <- c(12, 0)
# Set seed
set.seed(12345)

# Run the simulation ----
# sim_dt <- mclapply(X = 1:1e4, FUN = sim_fun,
#   n_g, g, var_cov, beta,
#   mc.cores = 4) %>% rbindlist()

# Summary stats ----
sim_dt[, mean(se), by = method]
sim_dt[, median(se), by = method]

# Plot results ----
# Distribution of standard errors
ggplot(data = sim_dt, aes(x = se, fill = method)) +
  geom_density(alpha = 0.6, size = 0.1, color = "grey50") +
  xlab("Standard error") +
  ylab("Density") +
  ggtitle("Distributions of standard errors by method",
    subtitle = "Truth: spherical disturbances") +
  scale_fill_viridis("Std. error method:",
    discrete = T, direction = -1) +
  theme_ed

# Pairs of spherical and cluster-robust SE
pair_dt <- merge(
  x = sim_dt[method == "spherical", list(se_sp = se, iter)],
  y = sim_dt[method == "cluster robust", .(se_cl = se, iter)],
  by = "iter")
# The plot
ggplot(data = pair_dt, aes(x = se_sp, y = se_cl)) +
  geom_point(alpha = 0.3, size = 0.4) +
  stat_smooth(method = "lm", se = F) +
  xlab("Spherical S.E. (correct)") +
  ylab("Cluster-robust S.E. (arbitrary clusters)") +
  ggtitle("Comparing spherical and cluster-robust standard errors",
    subtitle = "When disturbances are spherical") +
  scale_fill_viridis("Std. error method:",
    discrete = T, direction = -1) +
  theme_ed +
  coord_equal()

# Simulation parameters, part 2 ----
# Observations per group
n_g <- 30
# Number of groups
g <- 50
# Variance-covariance matrix (within a cluster)
var_cov <- matrix(data = 0.9, nrow = n_g, ncol = n_g)
diag(var_cov) <- 1
# Define beta
beta <- c(12, 0)
# Set seed
set.seed(12345)

# Run the simulation, part 2 ----
sim_dt <- mclapply(X = 1:1e4, FUN = sim_fun,
  n_g, g, var_cov, beta,
  mc.cores = 4) %>% rbindlist()

# Summary stats, part 2 ----
sim_dt[, mean(se), by = method]
sim_dt[, median(se), by = method]

# Plot results, part 2 ----
# Distribution of standard errors
ggplot(data = sim_dt, aes(x = se, fill = method)) +
  geom_density(alpha = 0.6, size = 0.1, color = "grey50") +
  xlab("Standard error") +
  ylab("Density") +
  ggtitle("Distributions of standard errors by method",
    subtitle = "Truth: cluster-correlated disturbances") +
  scale_fill_viridis("Std. error method:",
    discrete = T, direction = -1) +
  theme_ed

# More setup ----
library(dplyr)

# Functions ----
# Function to convert tibble, data.frame, or tbl_df to matrix
to_matrix <- function(the_df, vars) {
  # Create a matrix from variables in var
  new_mat <- the_df %>%
    # Select the columns given in 'vars'
    select_(.dots = vars) %>%
    # Convert to matrix
    as.matrix()
  # Return 'new_mat'
  return(new_mat)
}
# Function for OLS coefficient estimates
b_ols <- function(y, X) {
  # Calculate beta hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  # Return beta_hat
  return(beta_hat)
}
# Function for OLS coef., SE, t-stat, and p-value
ols <- function(data, y_var, X_vars, intercept = T) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Add intercept
  if (intercept == T) X <- cbind(1, X)
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Estimate coefficients
  b <- b_ols(y, X)
  # Update names
  if (intercept == T) rownames(b)[1] <- "Intercept"
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n-k)
  s2 %<>% as.numeric()
  # Inverse of X'X
  XX_inv <- solve(t(X) %*% X)
  # Standard error
  se <- sqrt(s2 * diag(XX_inv))
  # Vector of _t_ statistics
  t_stats <- (b - 0) / se
  # Calculate the p-values
  p_values = pt(q = abs(t_stats), df = n-k, lower.tail = F) * 2
  # Nice table (data.frame) of results
  results <- data.frame(
    # The rows have the coef. names
    effect = rownames(b),
    # Estimated coefficients
    coef = as.vector(b),
    # Standard errors
    std_error = as.vector(se),
    # t statistics
    t_stat = as.vector(t_stats),
    # p-values
    p_value = as.vector(p_values)
    )
  # Return the results
  return(results)
}
# Function that demeans the columns of Z
demeaner <- function(N) {
  # Create an N-by-1 column of 1s
  i <- matrix(data = 1, nrow = N)
  # Create the demeaning matrix
  A <- diag(N) - (1/N) * i %*% t(i)
  # Return A
  return(A)
}
# Function to return OLS residuals
resid_ols <- function(data, y_var, X_vars, intercept = T) {
  # Require the 'dplyr' package
  require(dplyr)
  # Create the y matrix
  y <- to_matrix(the_df = data, vars = y_var)
  # Create the X matrix
  X <- to_matrix(the_df = data, vars = X_vars)
  # Bind a column of ones to X
  if (intercept == T) X <- cbind(1, X)
  # Calculate the sample size, n
  n <- nrow(X)
  # Calculate the residuals
  resids <- y - X %*% b_ols(y, X)
  # Return 'resids'
  return(resids)
}
# Function for OLS coef., SE, t-stat, and p-value
vcov_ols <- function(data, y_var, X_vars, intercept = T) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Add intercept
  if (intercept == T) X <- cbind(1, X)
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Estimate coefficients
  b <- b_ols(y, X)
  # Update names
  if (intercept == T) rownames(b)[1] <- "Intercept"
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s2
  s2 <- (t(e) %*% e) / (n-k)
  s2 %<>% as.numeric()
  # Inverse of X'X
  XX_inv <- solve(t(X) %*% X)
  # Return the results
  return(as.numeric(s2) * XX_inv)
}

# Generate data ----
# Set the seed
set.seed(12345)
# Define our sample size
n <- 1e4
# Define beta
beta <- c(5, 2, -3)
# Define the means of x1, x2, and z
mean_vec <- c(5, 10, -5)
# Define the var-cov matrix
vcov_mat <- matrix(data =
  c(1, 0.75, 0.25, 0.75, 1, 0, 0.25, 0, 1),
  nrow = 3)
# Generate the data for x1, x2, and z
gen_df <- mvrnorm(n = n, mu = mean_vec, Sigma = vcov_mat,
  empirical = T) %>% tbl_df()
# Change names
names(gen_df) <- c("x1", "x2", "z")
# Generate the error term and calculate y
gen_df %<>% mutate(
  e = rnorm(n),
  y = beta[1] + beta[2] * x1 + beta[3] * x2 + e)

select(gen_df, x1, x2, z, e) %>% cor()

ols(data = gen_df, y_var = "y", X_vars = c("x1", "x2"))

# Omitting x2
ols(data = gen_df, y_var = "y", X_vars = "x1")
# Omitting x1
ols(data = gen_df, y_var = "y", X_vars = "x2")

# Function: IV ----
# Function for IV coefficient estimates
b_iv <- function(data, y_var, X_vars, Z_vars, intercept = T) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  Z <- to_matrix(data, Z_vars)
  # Add intercept
  if (intercept == T) X <- cbind(1, X)
  if (intercept == T) Z <- cbind(1, Z)
  # Calculate beta hat
  beta_hat <- solve(t(Z) %*% X) %*% t(Z) %*% y
  # Update names
  if (intercept == T) rownames(beta_hat)[1] <- "Intercept"
  # Return beta_hat
  return(beta_hat)
}

b_iv(data = gen_df, y_var = "y", X_vars = "x1", Z_vars = "z")

# Checking our work with 'felm'
felm(y ~ 1 | 0 | (x1 ~ z) | 0, data = gen_df) %>%
  summary() %>% coef()

# Function: 2SLS ----
# Function for IV coefficient estimates
b_2sls <- function(data, y_var, X_vars, Z_vars, intercept = T) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  Z <- to_matrix(data, Z_vars)
  # Add intercept
  if (intercept == T) X <- cbind(1, X)
  if (intercept == T) Z <- cbind(1, Z)
  # Estimate the first stage
  b_stage1 <- solve(t(Z) %*% Z) %*% t(Z) %*% X
  # Fit the first stage values
  X_hat <- Z %*% b_stage1
  # Estimate the second stage
  b_stage2 <- solve(t(X_hat) %*% X_hat) %*% t(X_hat) %*% y
  # Update names
  if (intercept == T) rownames(b_stage2)[1] <- "Intercept"
  # Return beta_hat
  return(b_stage2)
}

# Run the function
b_2sls(data = gen_df, y_var = "y", X_vars = "x1", Z_vars = "z")

# The first stage
b_fs <- ols(gen_df, "x1", "z") %$% coef[2]
# The reduced form
b_rf <- ols(gen_df, "y", "z") %$% coef[2]
# Calculate the ratio
b_rf / b_fs
# Compare to beta-hat IV
b_iv(gen_df, "y", "x1", "z")

# Updating 2SLS function ----
# Function for IV coefficient estimates
b_2sls <- function(data, y_var, X_vars, Z_vars, intercept = T) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  Z <- to_matrix(data, Z_vars)
  # Add intercept
  if (intercept == T) X <- cbind(1, X)
  if (intercept == T) Z <- cbind(1, Z)
  # Calculate P_Z
  P_Z <- Z %*% solve(t(Z) %*% Z) %*% t(Z)
  # Calculate b_2sls
  b <- solve(t(X) %*% P_Z %*% X) %*% t(X) %*% P_Z %*% y
  # Update names
  if (intercept == T) rownames(b)[1] <- "Intercept"
  # Return b
  return(b)
}

# Run it
b_2sls(gen_df, "y", "x1", "z")

# Update 2SLS function with std. errors ----
# Function for IV coefficient estimates
b_2sls <- function(data, y_var, X_vars, Z_vars, intercept = T) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  Z <- to_matrix(data, Z_vars)
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Add intercept
  if (intercept == T) X <- cbind(1, X)
  if (intercept == T) Z <- cbind(1, Z)
  # Calculate P_Z
  P_Z <- Z %*% solve(t(Z) %*% Z) %*% t(Z)
  # Calculate b_2sls
  b <- solve(t(X) %*% P_Z %*% X) %*% t(X) %*% P_Z %*% y
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s2
  s2 <- (t(e) %*% e) / (n - k)
  # Inverse of X' Pz X
  XX_inv <- solve(t(X) %*% P_Z %*% X)
  # Standard error
  se <- sqrt(s2 * diag(XX_inv))
  # Vector of _t_ statistics
  t_stats <- (b - 0) / se
  # Calculate the p-values
  p_values = pt(q = abs(t_stats), df = n-k, lower.tail = F) * 2
  # Update names
  if (intercept == T) rownames(b)[1] <- "Intercept"
  # Nice table (data.frame) of results
  results <- data.frame(
    # The rows have the coef. names
    effect = rownames(b),
    # Estimated coefficients
    coef = as.vector(b),
    # Standard errors
    std_error = as.vector(se),
    # t statistics
    t_stat = as.vector(t_stats),
    # p-values
    p_value = as.vector(p_values)
    )
  # Return the results
  return(results)
}

# Our function
b_2sls(gen_df, "y", "x1", "z")
# And felm
felm(y ~ 1 | 0 | (x1 ~ z), data = gen_df) %>%
  summary()

# Generate measurement error data ----
# Sample size
n <- 1e4
# Set seed
set.seed(12345)
# Generate data
temp_df <- data.frame(
  true_temp = rnorm(n),
  # Disturbance
  e = rnorm(n),
  # Measurement error, station A
  e_a = rnorm(n),
  # Measurement error, station A
  e_b = rnorm(n)
  )
# Add more variables
temp_df %<>% mutate(
  temp_a = true_temp + e_a,
  temp_b = 3 + true_temp + e_b,
  income = 50 + 3 * true_temp + e
  )

# OLS for DGP
ols(temp_df, "income", "true_temp")
# OLS for DGP
ols(temp_df, "income", "temp_a")
# 2SLS
b_2sls(temp_df, "income", "temp_a", "temp_b")
