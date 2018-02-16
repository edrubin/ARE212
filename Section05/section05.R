
# Setup ----
# Settings
options(stringsAsFactors = F)
# Packages
library(pacman)
p_load(dplyr, lfe, magrittr, readr)
# Directories
dir_data <- "/Users/edwardarubin/Dropbox/Teaching/ARE212/Section05/"
# Load the dataset from CSV
cars <- paste0(dir_data, "auto.csv") %>% read_csv()

# Functions ----

# Function to convert tibble, data.frame, or tbl_df to matrix ----
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

# Function for OLS coefficient estimates ----
b_ols <- function(data, y_var, X_vars, intercept = TRUE) {
  # Require the 'dplyr' package
  require(dplyr)
  # Create the y matrix
  y <- to_matrix(the_df = data, vars = y_var)
  # Create the X matrix
  X <- to_matrix(the_df = data, vars = X_vars)
  # If 'intercept' is TRUE, then add a column of ones
  if (intercept == T) {
    # Bind a column of ones to X
    X <- cbind(1, X)
    # Name the new column "intercept"
    colnames(X) <- c("intercept", X_vars)
  }
  # Calculate beta hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  # Return beta_hat
  return(beta_hat)
}

# Function for t statistics ----
t_stat <- function(data, y_var, X_vars, gamma, intercept = T) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Add intercept if requested
  if (intercept == T) X <- cbind(1, X)
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Estimate coefficients
  b <- b_ols(data, y_var, X_vars, intercept)
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n-k)
  # Force s2 to numeric
  s2 %<>% as.numeric()
  # Inverse of X'X
  XX_inv <- solve(t(X) %*% X)
  # Standard error
  se <- sqrt(s2 * diag(XX_inv))
  # Vector of _t_ statistics
  t_stats <- (b - gamma) / se
  # Return the _t_ statistics
  return(t_stats)
}

# magrittr ----
cor(cars$price, cars$weight)
cars %$% cor(price, weight)

# Check our t-stat function ----
# Our function
t_stat(cars,
  y_var = "price", X_vars = c("mpg", "weight"),
  gamma = 0, intercept = T)
# felm
felm(price ~ mpg + weight, cars) %>%
  summary() %$% (coefficients)[,3]

# t distribution ----
# The default: lower.tail = TRUE
pt(q = 2, df = 15)
# Setting lower.tail = TRUE
pt(q = 2, df = 15, lower.tail = T)
# Setting lower.tail = FALSE
pt(q = 2, df = 15, lower.tail = F)

# Expanding the ols function with t tests ----
ols <- function(data, y_var, X_vars, intercept = T) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Add intercept if requested
  if (intercept == T) X <- cbind(1, X)
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Estimate coefficients
  b <- b_ols(data, y_var, X_vars, intercept)
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n-k)
  # Update s2 to numeric
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
    coef = as.vector(b) %>% round(3),
    # Standard errors
    std_error = as.vector(se) %>% round(3),
    # t statistics
    t_stat = as.vector(t_stats) %>% round(3),
    # p-values
    p_value = as.vector(p_values) %>% round(4)
    )
  # Return the results
  return(results)
}

# Check our function
ols(data = cars,
  y_var = "price",
  X_vars = c("mpg", "weight"),
  intercept = T)

# Try felm
felm(price ~ mpg + weight, cars) %>%
  summary() %$% coefficients

# Joint tests with F ----
joint_test <- function(data, y_var, X_vars) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Add intercept
  X <- cbind(1, X)
  # Name the new column "intercept"
  colnames(X) <- c("intercept", X_vars)
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # J is k-1
  J <- k - 1
  # Create the R matrix: bind a column of zeros
  # onto a J-by-J identity matrix
  R <- cbind(0, diag(J))

  # Estimate coefficients
  b <- b_ols(data, y_var, X_vars)
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n-k)
  # Force s2 to numeric
  s2 %<>% as.numeric()

  # Create the inner matrix R(X'X)^(-1)R'
  RXXR <- R %*% solve(t(X) %*% X) %*% t(R)
  # Calculate the F stat
  f_stat <- t(R %*% b) %*% solve(RXXR) %*% (R %*% b) / J / s2
  # Calculate the p-value
  p_value <- pf(q = f_stat, df1 = J, df2 = n-k, lower.tail = F)
  # Create a data.frame of the f stat. and p-value
  results <- data.frame(
    f_stat = f_stat %>% as.vector(),
    p_value = p_value %>% as.vector())
  return(results)
}

# Run our function
joint_test(data = cars,
  y_var = "price", X_vars = c("mpg", "weight"))

# Compare our function to felm
felm(price ~ mpg + weight, cars) %>%
  summary() %$% F.fstat

# OLS function with F statistics ----
ols_joint <- function(data, y_var, X_vars, intercept = T) {
  # Run the ols() function
  ols_results <- ols(data, y_var, X_vars, intercept)
  # If intercept is T, run the joint_test() function
  # Otherwise, define joint_results to be NA and
  # issue a warning
  if (intercept == T) {
    joint_results <- joint_test(data, y_var, X_vars)
  } else {
    warning("No intercept: will not perform F test.")
    joint_results <- data.frame(
      f_stat = NA,
      p_value = NA)
  }
  # Create the results list
  results <- list(ols_results, joint_results)
  # Return the results list
  return(results)
}

# Test our function with an intercept
ols_joint(data = cars,
  y_var = "price",
  X_vars = c("mpg", "weight"),
  intercept = T)

# Test our function without an intercept
ols_joint(data = cars,
  y_var = "price",
  X_vars = c("mpg", "weight"),
  intercept = F)

# Simulation functions ----

# Function: generating data
gen_data <- function(sample_size) {
  # Create data.frame with random x and error
  data_df <- data.frame(
    x = rnorm(sample_size),
    e = rnorm(sample_size))
  # Calculate y = 7 + 0.5 x + e; drop 'e'
  data_df %<>% mutate(y = 7 + 0.5 * x + e) %>%
    select(-e)
  # Return data_df
  return(data_df)
}

# Function: single instance of simulation
one_sim <- function(sample_size) {
  # Estimate via OLS
  ols_est <- ols(data = gen_data(sample_size),
    y_var = "y", X_vars = "x")
  # Grab the estimated coefficient on x
  # (the second element of 'coef')
  b1 <- ols_est %$% coef[2]
  # Grab the second p-value
  # (the first p-value is for the intercept)
  p_value <- ols_est %$% p_value[2]
  # Return a data.frame with b1 and p_value
  return(data.frame(b1, p_value))
}

# Function: full simulation
ols_sim <- function(n_sims, sample_size, seed = 12345) {
  # Set the seed
  set.seed(seed)
  # Run one_sim n_sims times; convert results to data.frame
  sim_df <- replicate(
    n = n_sims,
    expr = one_sim(sample_size),
    simplify = F
    ) %>% bind_rows()
  # Return sim_df
  return(sim_df)
}

# Plot the simulation results ----
# Load ggplot2 and ggthemes packages
library(ggplot2)
library(ggthemes)
# Plot distribution of p-values, n = 10
ggplot(data = sim10, aes(x = p_value)) +
  stat_density(fill = "grey20", alpha = 0.9) +
  geom_vline(xintercept = 0.05, color = "deeppink2", size = 1) +
  theme_pander() +
  xlab(expression(paste(italic("p"), "-Value"))) +
  ylab("Density") +
  ggtitle(expression(paste("Distribution of ", italic(p),
    "-Values from 1,000 simulations with sample size 10")))
# Plot distribution of p-values n = 100
ggplot(data = sim100, aes(x = p_value)) +
  stat_density(fill = "grey20", alpha = 0.9) +
  geom_vline(xintercept = 0.05, color = "deeppink2", size = 1) +
  xlim(0, 0.1) +
  theme_pander() +
  xlab(expression(paste(italic("p"), "-Value"))) +
  ylab("Density") +
  ggtitle(expression(paste("Distribution of ", italic(p),
    "-Values from 1,000 simulations with sample size 100")))
# Plot distribution of coefficients, n = 10
ggplot(data = sim10, aes(x = b1)) +
  stat_density(fill = "grey70") +
  geom_vline(xintercept = 0.5, color = "darkviolet", size = 1) +
  theme_pander() +
  xlab(expression(paste(beta[1], " estimate"))) +
  ylab("Density") +
  ggtitle(expression(paste("Distribution of ", beta[1],
    " estimates from 1,000 simulations with sample size 10")))
# Plot distribution of coefficients, n = 100
ggplot(data = sim100, aes(x = b1)) +
  stat_density(fill = "grey70") +
  geom_vline(xintercept = 0.5, color = "darkviolet", size = 1) +
  theme_pander() +
  xlab(expression(paste(beta[1], " estimate"))) +
  ylab("Density") +
  ggtitle(expression(paste("Distribution of ", beta[1],
    " estimates from 1,000 simulations with sample size 100")))

# Parallelization ----
library(parallel)
detectCores()

# Parallelized ols_sim function
ols_sim_mc <- function(n_sims, sample_size, seed = 12345) {
  # Require the parallel package
  require(parallel)
  # Set the seed
  set.seed(seed)
  # Run one_sim n_sims times; convert results to data.frame
  sim_df <- mclapply(
    X = rep(x = sample_size, times = n_sims),
    FUN = one_sim,
    # Specify that we want 4 cores
    mc.cores = 4
    ) %>% bind_rows()
  # Return sim_df
  return(sim_df)
}

# Time the non-parallelized ----
# Run ols_sim for sample size of 10
start1 <- proc.time()
sim10 <- ols_sim(n_sims = 1e4, sample_size = 10)
stop1 <- proc.time()
# Run ols_sim for sample size of 100
start2 <- proc.time()
sim100 <- ols_sim(n_sims = 1e4, sample_size = 100)
stop2 <- proc.time()

# Time the parallelized ----
# Run ols_sim_mc for sample size of 10
start3 <- proc.time()
sim10_mc <- ols_sim_mc(n_sims = 1e4, sample_size = 10)
stop3 <- proc.time()
# Run ols_sim_mc for sample size of 100
start4 <- proc.time()
sim100_mc <- ols_sim_mc(n_sims = 1e4, sample_size = 100)
stop4 <- proc.time()

# Times ----
stop1 - start1
stop2 - start2
stop3 - start3
stop4 - start4
