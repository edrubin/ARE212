
# From last week ----
# Make a quick plot with 'qplot'
ggplot2::qplot(x = rnorm(100), y = rnorm(100), geom = "point")

# Setup ----
# Options
options(stringsAsFactors = F)
# Packages
library(pacman)
p_load(dplyr, magrittr, parallel, ggplot2, ggthemes, viridis, lfe)
# Directory
setwd("/Users/edwardarubin/Dropbox/Teaching/ARE212/Section07")
# My ggplot2 theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey85"),
  panel.grid.major = element_line(color = "grey95", size = 0.2),
  panel.grid.minor = element_line(color = "grey95", size = 0.2),
  legend.key = element_blank())

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

# Simulation population ----
# Set the seed
set.seed(12345)
# Set population size, N
N <- 1e5
# Set alpha and beta
alpha <- 0.5
beta <- 1.5
# Create the population data: intercept (i) and X
pop_df <- data.frame(
  i = 1,
  x = runif(n = N, min = 0, max = 2000)
  ) %>% tbl_df()
# Generate error term, e
pop_df %<>% mutate(
  e = rnorm(N, mean = 0, sd = sqrt(4 * x^2)))
# Calculate y
pop_df %<>% mutate(y = alpha + beta * x + e)

# Add weights
pop_df %<>% mutate(w = 10/x)

# Apply weights
pop_df %<>% mutate(
  y_w = y * w,
  i_w = i * w,
  x_w = x * w)

# Single iteration function ----
# Function for a single iteration of the simulation
one_run <- function(iter, population) {
  # Sample 1000 rows from the population
  sample_df <- sample_n(tbl = population, size = 1000)
  # Calculate the OLS coef. (using unweighted variables)
  coef_ols <- b_ols(
    y = to_matrix(sample_df, "y"),
    X = to_matrix(sample_df, c("i", "x")))
  # Calculate the WLS coef. (using weighted variables)
  coef_wls <- b_ols(
    y = to_matrix(sample_df, "y_w"),
    X = to_matrix(sample_df, c("i_w", "x_w")))
  # Create a data.frame to return
  coef_df <- data.frame(
    est    = as.vector(c(coef_ols, coef_wls)),
    param  = rep(c("int", "coef"), 2),
    method = rep(c("ols", "wls"), each = 2),
    iter   = iter
    )
  # Return the data.frame
  return(coef_df)
}

# Run the simulation (parallelized) ----
# Make the cluster
cl <- makeCluster(4)
# Load functions on the cluster
clusterEvalQ(cl, {
  library(dplyr)
  library(magrittr)
  })
# Export our data and functions to the cluster
clusterExport(cl, "pop_df")
clusterExport(cl, c("to_matrix", "b_ols", "one_run"))
# Set seed in parallel
clusterSetRNGStream(cl, 12345)

# Run the simulation 10K times
sim_df <- parLapply(
  cl = cl,
  X = 1:1e4,
  fun = one_run,
  population = pop_df) %>% bind_rows() %>% tbl_df()

# Stop the cluster
stopCluster(cl)

# Run the simulation (non-parallelized) ----
sim_df <- lapply(
  X = 1:1e4,
  FUN = one_run,
  population = pop_df) %>% bind_rows() %>% tbl_df()

# Plot simulation results ----
# The plot
ggplot(data = filter(sim_df, param == "coef"), aes(x = est)) +
  geom_vline(xintercept = 1.5, color = "grey70", size = 0.75) +
  geom_density(aes(fill = method, color = method), alpha = 0.7) +
  xlab(expression(paste("Estimate for ", beta))) +
  ylab("Density") +
  ggtitle("Simulation comparing coefficients from OLS and WLS") +
  scale_fill_viridis("Method", labels = c("OLS", "WLS"),
    discrete = T, end = 0.95) +
  scale_color_viridis("Method", labels = c("OLS", "WLS"),
    discrete = T, end = 0.95) +
  theme_ed

# Summarize results
sim_df %>%
  group_by(param, method) %>%
  summarize(mean(est), sd(est)) %>%
  knitr::kable(digits = 4,
    col.names = c("Parameter", "Method", "Mean", "Std. Dev."))

# Add bad weights ----
# Add the bad weights
pop_df %<>% mutate(v = 10/x^2)
# Weight the observations with the bad weights
pop_df %<>% mutate(
  y_v = y * v,
  i_v = i * v,
  x_v = x * v)

# Update simulation function ----
# Function for a single iteration of the simulation
one_run <- function(iter, population) {
  # Sample 1000 rows from the population
  sample_df <- sample_n(tbl = population, size = 1000)
  # Calculate the OLS coef. (using unweighted variables)
  coef_ols <- b_ols(
    y = to_matrix(sample_df, "y"),
    X = to_matrix(sample_df, c("i", "x")))
  # Calculate the WLS coef. (using correctly weighted variables)
  coef_wls <- b_ols(
    y = to_matrix(sample_df, "y_w"),
    X = to_matrix(sample_df, c("i_w", "x_w")))
  # Calculate the WLS coef. (using incorrectly weighted variables)
  coef_wls_bad <- b_ols(
    y = to_matrix(sample_df, "y_v"),
    X = to_matrix(sample_df, c("i_v", "x_v")))
  # Create a data.frame to return
  coef_df <- data.frame(
    est    = as.vector(c(coef_ols, coef_wls, coef_wls_bad)),
    param  = rep(c("int", "coef"), 3),
    method = rep(c("ols", "wls", "wls bad"), each = 2),
    iter   = iter
    )
  # Return the data.frame
  return(coef_df)
}

# Run simulation (parallelized) ----
# Make the cluster
cl <- makeCluster(4)
# Load functions on the cluster
clusterEvalQ(cl, {
  library(dplyr)
  library(magrittr)
  })
# Export our data and functions to the cluster
clusterExport(cl, "pop_df")
clusterExport(cl, c("to_matrix", "b_ols", "one_run"))
# Set seed in parallel
clusterSetRNGStream(cl, 12345)
# Run the simulation 10,000 times
miss_df <- parLapply(
  cl = cl,
  X = 1:1e4,
  fun = one_run,
  population = pop_df) %>% bind_rows() %>% tbl_df()
# Stop the cluster
stopCluster(cl)

# Run simulation (non-parallelized) ----
# Run the simulation 10,000 times
miss_df <- lapply(
  X = 1:1e4,
  FUN = one_run,
  population = pop_df) %>% bind_rows() %>% tbl_df()

# Plot results ----
# Plot results
ggplot(data = filter(miss_df, param == "coef"), aes(x = est)) +
  geom_vline(xintercept = 1.5, color = "grey70", size = 0.75) +
  geom_density(aes(fill = method, color = method), alpha = 0.7) +
  xlab(expression(paste("Estimate for ", beta))) +
  ylab("Density") +
  ggtitle("Simulation comparing coefficients from OLS and WLS",
    subtitle = "Allowing for misspecification in WLS") +
  scale_fill_viridis("Method",
    labels = c("OLS", "WLS", "WLS misspecified"),
    discrete = T, end = 0.95, direction = -1) +
  scale_color_viridis("Method",
    labels = c("OLS", "WLS", "WLS misspecified"),
    discrete = T, end = 0.95, direction = -1) +
  theme_ed

# Zoom in
ggplot(data = filter(miss_df, param == "coef"), aes(x = est)) +
  geom_vline(xintercept = 1.5, color = "grey70", size = 0.75) +
  geom_density(aes(fill = method, color = method), alpha = 0.65) +
  xlab(expression(paste("Estimate for ", beta))) +
  ylab("Density") +
  ggtitle("Simulation comparing coefficients from OLS and WLS",
    subtitle = "Allowing for misspecification in WLS") +
  xlim(1.5 + c(-1,1) * 2) +
  scale_fill_viridis("Method",
    labels = c("OLS", "WLS", "WLS misspecified"),
    discrete = T, end = 0.95, direction = -1) +
  scale_color_viridis("Method",
    labels = c("OLS", "WLS", "WLS misspecified"),
    discrete = T, end = 0.95, direction = -1) +
  theme_ed

# Numerical summary
miss_df %>%
  group_by(param, method) %>%
  summarize(mean(est), sd(est)) %>%
  knitr::kable(digits = 4,
    col.names = c("Parameter", "Method", "Mean", "Std. Dev."))

# Weighting in 'felm' ----
# Check out 'weights' for 'felm'
?felm
# 1. 'felm' with our squared weights
felm(y ~ x, data = pop_df, weights = pop_df$w^2)
# 2. 'felm', re-defining our weights
felm(y ~ x, data = pop_df, weights = (10/pop_df$x)^2)
# 3. 'felm' with our transformed variables
felm(y_w ~ -1 + i_w + x_w, data = pop_df)
# 4. Using our 'b_ols' function on the transformed variables
# (As we did in the simulation)
b_ols(y = to_matrix(pop_df, "y_w"),
  X = to_matrix(pop_df, c("i_w", "x_w")))
