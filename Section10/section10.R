
# Setup ----
# Options
options(stringsAsFactors = F)
options(scipen = 10)
# Packages
library(pacman)
p_load(dplyr, lfe, magrittr, ggplot2, viridis, sandwich)
# My ggplot2 theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  # panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey95", size = 0.3),
  panel.grid.major = element_line(color = "grey95", size = 0.3),
  panel.grid.minor = element_line(color = "grey95", size = 0.3),
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
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n-k)
  s2 %<>% as.numeric()
  # Inverse of X'X
  XX_inv <- solve(t(X) %*% X)
  # Return the results
  return(as.numeric(s2) * XX_inv)
}

# Diamonds regression ----
# The estimated coefficients
ols(data = diamonds,
  y_var = "price",
  X_vars = c("carat", "depth"))[,1:2]
# The estimated variance-covariance matrix
vcov_spherical <- vcov_ols(data = diamonds,
  y_var = "price",
  X_vars = c("carat", "depth"))
# Get the standard errors
vcov_spherical %>% diag() %>% sqrt()

felm(price ~ carat + depth, data = diamonds) %>% summary()

# Add OLS residuals to the diamonds dataset
diamonds %<>% mutate(e_ols =
  resid_ols(data = diamonds,
    y_var = "price",
    X_vars = c("carat", "depth"))
  )

# Residuals against carat
ggplot(data = diamonds, aes(x = carat, y = e_ols)) +
  geom_point(color = viridis(1), shape = 21, alpha = 0.3) +
  xlab("Carat (covariate #1)") +
  ylab("OLS residual") +
  ggtitle("Residuals and carat") +
  theme_ed
# Residuals against depth
ggplot(data = diamonds, aes(x = depth, y = e_ols)) +
  geom_point(color = viridis(1), shape = 21, alpha = 0.3) +
  xlab("Depth (covariate #2)") +
  ylab("OLS residual") +
  ggtitle("Residuals and depth") +
  theme_ed

# White vcov ----
vcov_white <- function(data, y_var, X_vars, intercept = T) {
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
  # Inverse of X'X
  XX_inv <- solve(t(X) %*% X)
  # For each row, calculate x_i' x_i e_i^2; then sum
  sigma_hat <- lapply(X = 1:n, FUN = function(i) {
    # Define x_i
    x_i <- matrix(as.vector(X[i,]), nrow = 1)
    # Return x_i' x_i e_i^2
    return(t(x_i) %*% x_i * e[i]^2)
  }) %>% Reduce(f = "+", x = .)
  # Return the results
  return(XX_inv %*% sigma_hat %*% XX_inv)
}

# Spherical:
vcov_ols(data = diamonds,
  y_var = "price",
  X_vars = c("carat", "depth")) %>%
  diag() %>% sqrt()
# Het. robust:
vcov_white(data = diamonds,
  y_var = "price",
  X_vars = c("carat", "depth")) %>%
  diag() %>% sqrt()

felm(price ~ carat + depth, data = diamonds) %>%
  summary(robust = T) %>% coef() %>% extract(., 1:3, 2)

# Playfair's wheat data ----
# Load the 'HistData' package
library(HistData)
# Load Playfair's wheat data
wheat_df <- Wheat %>% tbl_df()
# Drop the rows missing a value
wheat_df %<>% na.omit()
# Long to wide table
wheat_gg <- wheat_df %>% tidyr::gather(Series, Price, Wheat:Wages)
# Playfair's graph
ggplot(data = wheat_gg, aes(x = Year, y = Price, color = Series, linetype = Series)) +
  geom_point() +
  geom_line() +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Wage/Price (Shillings)") +
  ggtitle("Playfair's wheat and wages time series") +
  theme_ed +
  scale_linetype_manual("Series:",
    values = c(1, 3),
    labels = c("Wages", "Price of wheat")
  ) +
  scale_color_viridis("Series:",
    option = "B",
    discrete = T, end = .8, direction = -1,
    labels = c("Wages", "Price of wheat")
  )

# HAC function ----
vcov_hac <- function(data, y_var, X_vars, L, intercept = T) {
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
  # Inverse of X'X
  XX_inv <- solve(t(X) %*% X)
  # The first term
  S_o <- lapply(X = 1:n, FUN = function(i) {
    # Define x_i
    x_i <- matrix(as.vector(X[i,]), nrow = 1)
    # Return x_i' x_i e_i^2
    return(t(x_i) %*% x_i * e[i]^2)
  }) %>% Reduce(f = "+", x = .)
  S_o <- S_o / n
  # The second term
  S_more <- lapply(X = 1:L, FUN = function(j) {
    lapply(X = (j+1):n, FUN = function(t) {
      # Grab the rows of X that we need
      x_t <- matrix(X[t,], nrow = 1)
      x_tj <- matrix(X[t-j,], nrow = 1)
      # The calculation
      (1 - j / (L + 1)) * e[t] * e[t-j] * (
        t(x_t) %*% x_tj + t(x_tj) %*% x_t)
      }) %>% Reduce(f = "+", x = .)
    }) %>% Reduce(f = "+", x = .)
  S_more <- S_more / n
  # The full sandwich
  S_star <- S_o + S_more
  # Return the results
  return(n * XX_inv %*% S_star %*% XX_inv)
}

# Choose a lag
the_lag <- ceiling(nrow(wheat_df) / 4)
# The spherical standard errors
vcov_ols(wheat_df, "Wheat", "Wages") %>%
  diag() %>% sqrt()
# The standard errors from our HAC robust function
vcov_hac(wheat_df, "Wheat", "Wages", the_lag) %>%
  diag() %>% sqrt()
# Estimate the model using 'lm'
wheat_reg <- lm(Wheat ~ Wages, data = wheat_df)
# Use the NeweyWest function
NeweyWest(wheat_reg, lag = the_lag, prewhite = F) %>%
  diag() %>% sqrt()


# Earthquakes plot ----
ggplot(datasets::quakes, aes(x = long, y = lat)) +
  geom_point(aes(size = mag), shape = 21,
    fill = "#AA5585", color = "#661141", alpha = 0.25) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Earthquakes off Fiji, since 1964") +
  scale_size_continuous("Magnitude") +
  theme_ed

# Cluster-robust estimator ----
vcov_cluster <- function(data, y_var, X_vars,
  cluster_var, intercept = T) {
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
    # Inverse of X'X
    XX_inv <- solve(t(X) %*% X)
    # Find the levels of the variable on which we are clustering
    cl_levels <- data[, cluster_var] %>% unique() %>% unlist()
    # Calculate the meat, iterating over the clusters
    meat_hat <- lapply(X = cl_levels, FUN = function(g) {
      # Find the row indices for the current cluster
      indices <- which(unlist(data[, cluster_var]) == g)
      # Grab the current cluster's rows from X and e
      X_g <- X[indices,]
      e_g <- e[indices] %>% matrix(ncol = 1)
      # Calculate this cluster's part of the meat estimate
      return(t(X_g) %*% e_g %*% t(e_g) %*% X_g)
      }) %>% Reduce(f = "+", x = .) / n
    # Find the number of clusters
    G <- length(cl_levels)
    # Degrees-of-freedom correction
    df_c <- G/(G-1) * (n-1)/(n-k)
    # Return the results
    return(df_c * n * XX_inv %*% meat_hat %*% XX_inv)
  }

# Load the 'robustbase' package
library(robustbase)
# Load the 'NOxEmissions' dataset
nox_df <- NOxEmissions %>% tbl_df()
# Change names
names(nox_df) <- c("date", "log_nox", "log_nox_cars", "wind")

vcov_cluster(
  data = nox_df,
  y_var = "log_nox",
  X_vars = "wind",
  cluster_var = "date") %>%
  diag() %>% sqrt()

felm(log_nox ~ wind | 0 | 0 | date, data = nox_df) %>%
  summary()

# The results under spherical errors
our_table <- ols(nox_df, "log_nox", "wind")[, 1:3]
our_table$effect <- c("Intercept", "Wind")
# Heteroskedasticity-robust
se_white <- vcov_white(nox_df, "log_nox", "wind") %>%
  diag() %>% sqrt()
# Cluster-robust
se_cluster <- vcov_cluster(nox_df, "log_nox", "wind", "date") %>%
  diag() %>% sqrt()
# Add new columns to the table
our_table %<>% mutate(se_white, se_cluster)
# Print results table
knitr::kable(our_table,
  col.names = c("Effect", "Coef.", "S.E. (Sph. Errors)",
    "S.E. (Het. Robust)", "S.E. (Cluster Robust)"),
  digits = c(3, 3, 4, 4, 4),
  align = c("l", rep("r", 4)),
  caption = "Comparing standard errors")

# Duplicated data ----
# Sample size
N <- 100
# Set seed
set.seed(12345)
# The dataset
one_df <- data.frame(x = rnorm(N)) %>%
  mutate(id = 1:N, e = rnorm(N), y = 3 + 5 * x + e) %>%
  tbl_df()
# The duplicated version of the dataset
two_df <- bind_rows(one_df, one_df)
# Regression on the dataset
lm(y ~ x, one_df) %>% summary() %>% coef()
# Regression on the duplicated dataset
lm(y ~ x, two_df) %>% summary() %>% coef()

# Regression on the datset; het.-robust est.
felm(y ~ x | 0 | 0 | 0, one_df) %>% summary(robust = T) %>% coef()
# Regression on the duplicated datset; cluster-robust est.
felm(y ~ x | 0 | 0 | id, two_df) %>% summary() %>% coef()
