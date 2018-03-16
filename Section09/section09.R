
# Setup ----
# Options
options(stringsAsFactors = F)
options(scipen = 10)
# Packages
library(pacman)
p_load(readr, lfe, dplyr, magrittr, parallel, ggplot2, viridis, gmodels, msm)
# Directory
dir_section <- "/Users/edwardarubin/Dropbox/Teaching/ARE212/Section09/"
# My ggplot2 theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey85"),
  panel.grid.major = element_line(color = "grey95", size = 0.2),
  panel.grid.minor = element_line(color = "grey95", size = 0.2),
  legend.key = element_blank())

# Load data ----
cars <- read_csv(paste0(dir_section, "auto.csv"))

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
ols <- function(data, y_var, X_vars) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Add intercept
  X <- cbind(1, X)
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Estimate coefficients
  b <- b_ols(y, X)
  # Update names
  rownames(b)[1] <- "Intercept"
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n-k)
  # Convert s2 to numeric
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

# Tables ----
# Regress price on MPG and weight
ols(cars, "price", c("mpg", "weight"))

# Regress price on MPG and weight
ols(cars, "price", c("mpg", "weight")) %>%
  knitr::kable()

# Regress price on MPG and weight
tmp_results <- ols(cars, "price", c("mpg", "weight"))[,2:5]
row.names(tmp_results) <- c("Intercept", "MPG", "Weight")
knitr::kable(tmp_results,
  digits = c(2, 2, 2, 3),
  col.names = c("$\\widehat{\\boldsymbol{\\beta}}$", "S.E.",
    "___t___ stat", "___p___-Value"),
  row.names = T,
  caption = "Regressing price on mileage and weight"
  )

# Regress price on MPG and weight
tmp_results <- ols(cars, "price", c("mpg", "weight"))[,2:5]
row.names(tmp_results) <- c("Intercept", "MPG", "Weight")
knitr::kable(tmp_results,
  format = "latex",
  digits = c(2, 2, 2, 3),
  col.names = c("$\\widehat{\\boldsymbol{\\beta}}$", "S.E.",
    "$t$ stat", "$p$-Value"),
  escape = F,
  row.names = T,
  caption = "Regressing price on mileage and weight",
  booktabs = T
  ) %>% print()

# Linear combinations ----
# Regress price on mpg and weight
reg1 <- ols(cars, "price", c("mpg", "weight"))
# lc = 20 * b1 + 3000 * b2
(lc <- 20 * reg1[2,2] + 3000 * reg1[3,2])


# A new vcov function ----
# Variance-covariance function for OLS beta hat
vcov_ols <- function(data, y_var, X_vars) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Add intercept
  X <- cbind(1, X)
  # Label intercept
  colnames(X)[1] <- "intercept"
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Estimate coefficients
  b <- b_ols(y, X)
  # Calculate residuals
  e <- y - X %*% b
  # Calculate s2 and convert to scalar
  s2 <- t(e) %*% e / (n - k)
  # Convert s2 to numeric
  s2 %<>% as.numeric()
  # Calculate the variance-covariance matrix
  vcov_mat <- s2 * solve(t(X) %*% X)
  # Return the variance-covariance matrix
  return(vcov_mat)
}

# Run the vcov_ols() function
vcov_ols(cars, "price", c("mpg", "weight"))

# Analytical standard error ----
# Regress price on mpg and weight
reg1 <- ols(cars, "price", c("mpg", "weight"))
# lc = 20 * b1 + 3000 * b2
(lc <- 20 * reg1[2,2] + 3000 * reg1[3,2])
# The variance-covariance matrix
vcov1 <- vcov_ols(cars, "price", c("mpg", "weight"))
# The standard error for 'lc'
(lc_se <- sqrt(20^2 * vcov1[2,2] + 3000^2 * vcov1[3,3] +
  2 * 20 * 3000 * vcov1[2,3]))

# Canned check ----
# Estimate the model with 'lm'
lm_est <- lm(price ~ mpg + weight, data = cars)
# Estimate the linear combination
estimable(obj = lm_est, cm = c(0, 20, 3000))
# Alternative test (no standard errors, though)
waldtest(lm_est, ~ 20 * mpg + 3000 * weight)

# Our t statistic
lc / lc_se

# Delta method ----
# Remind ourselves of LC and its var-cov matrix
lc <- 20 * reg1[2,2] + 3000 * reg1[3,2]
vcov1 <- vcov_ols(cars, "price", c("mpg", "weight"))
# Define our derivative matrix
deriv_mat <- matrix(c(0, 20, 3000), nrow = 1)
# Calculate the standard error of 'lc' via delta method
lc_dm <- sqrt(deriv_mat %*% vcov1 %*% t(deriv_mat))

# Analytical s.e.
lc_se
# Delta Method s.e.
lc_dm

# Nonlinear combination ----
# Set the seed
set.seed(12345)
# Set the size
n <- 50
# Generate data
fake_df <- data.frame(
  x = runif(n = n, min = -2, max = 3),
  e = rnorm(n = n, mean = 0, sd = sqrt(10))
  ) %>% tbl_df()
# Calculate y = 4 + 4x - 2x^2 + e
fake_df %<>% mutate(
  x2 = x^2,
  y = 4 + 4 * x - 2 * x^2 + e)

# Estimate coefficients
(b_fake <- ols(fake_df, "y", c("x", "x2")) %$% coef)
# Estimate var-cov matrix
v_fake <- vcov_ols(fake_df, "y", c("x", "x2"))

# Create the A matrix
A_fake <- matrix(data = c(
  # The first entry of A()
  0,
  # The second entry of A()
  -1/(2 * b_fake[3]),
  # The third entry of A()
  b_fake[2]/(2 * b_fake[3]^2)),
  nrow = 1)

# Our estimate for the x that maximizes y
(x_m <- - b_fake[2] / (2 * b_fake[3]))
# Our estimate for the standard error
(se_m <- sqrt(A_fake %*% v_fake %*% t(A_fake)))

# Estimate the equation
felm_fake <- felm(y ~ x + x2, data = fake_df)
# Use the 'deltamethod' function
deltamethod(g = ~ - x2 / (2 * x3),
  mean = coef(felm_fake),
  cov = vcov(felm_fake))
# Print the value we caluclated above
se_m

# Plot the maximization ----
# Our plot
ggplot(data = fake_df) +
  # 95% confidence interval for maximal x
  geom_rect(aes(ymin = -Inf, ymax = Inf,
    xmin = x_m - 1.96 * se_m, xmax = x_m + 1.96 * se_m),
    fill = "grey90", alpha = 0.05) +
  # Plot the points
  geom_point(aes(x = x, y = y)) +
  # Plot the predicted function
  stat_function(fun = function(x) {
    b_fake[1] + b_fake[2] * x + b_fake[3] * x^2
    }, color = "grey65") +
  # Vertical line at the predicted max.
  geom_vline(xintercept = x_m, color = "grey65", linetype = 2) +
  # Vertical line at the true max.
  geom_vline(xintercept = 1, color = "blue") +
  # Title
  ggtitle("Estimating the maximum of a quadratic") +
  # Theme
  theme_ed
