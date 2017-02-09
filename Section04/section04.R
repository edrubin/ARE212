

# Saving plots --------------------

# Start the .jpeg driver
jpeg("your_plot.jpeg")
# Make the plot
plot(x = 1:10, y = 1:10)
# Turn off the driver
dev.off()

# Start the .pdf driver
pdf("your_plot.pdf")
# Make the plot
plot(x = 1:10, y = 1:10)
# Turn off the driver
dev.off()

# Overfull \hbox --------------------
# Original line
library(magrittr)
x <- rnorm(100) %>% matrix(ncol = 4) %>% tbl_df() %>% mutate(V5 = V1 * V2)
# Bad job splitting line
library(magrittr)
x <- rnorm(100) %>% matrix(ncol = 4)
  %>% tbl_df() %>% mutate(V5 = V1 * V2)
# Good job splitting line #1
library(magrittr)
x <- rnorm(100) %>% matrix(ncol = 4) %>%
  tbl_df() %>% mutate(V5 = V1 * V2)
# Good job splitting line #2
library(magrittr)
x <- rnorm(100) %>%
  matrix(ncol = 4) %>%
  tbl_df() %>%
  mutate(V5 = V1 * V2)

# Logical operators --------------------
# I'm not lying
T == TRUE
# Greater/less than
1 > 3
1 < 1
1 >= 3
1 <= 1
# Alphabetization
"Ed" < "Everyone" # :(
"A" < "B"
# NA is weird
NA > 3
NA == T
NA == F
is.na(NA)
# Equals
T == F
(pi > 1) == T
# And
(3 > 2) & (2 > 3)
# Or
(3 > 2) | (2 > 3)
# Not (gives the opposite)
! T

# Optional arguments --------------------
b_ols <- function(data, y_var, X_vars, intercept = TRUE) {
  # Require the 'dplyr' package
  require(dplyr)

  # Create the y matrix
  y <- data %>%
    # Select y variable data from 'data'
    select_(.dots = y_var) %>%
    # Convert y_data to matrices
    as.matrix()

  # Create the X matrix
  X <- data %>%
    # Select X variable data from 'data'
    select_(.dots = X_vars)

  # If 'intercept' is TRUE, then add a column of ones
  # and move the column of ones to the front of the matrix
  if (intercept == T) {
    X <- data %>%
      # Add a column of ones to X_data
      mutate_("ones" = 1) %>%
      # Move the intercept column to the front
      select_("ones", .dots = X_vars)
    }

  # Convert X_data to a matrix
  X <- as.matrix(X)

  # Calculate beta hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

  # If 'intercept' is TRUE:
  # change the name of 'ones' to 'intercept'
  if (intercept == T) rownames(beta_hat) <- c("intercept", X_vars)

  # Return beta_hat
  return(beta_hat)
}

# Load the 'dplyr' package
library(dplyr)
# Set the seed
set.seed(12345)
# Set the sample size
n <- 100
# Generate the x and error data from N(0,1)
the_data <- tibble(
  x = rnorm(n),
  e = rnorm(n))
# Calculate y = 3 + 1.5 x + e
the_data <- mutate(the_data, y = 3 + 1.5 * x + e)
# Plot to make sure things are going well.
plot(
  # The variables for the plot
  x = the_data$x, y = the_data$y,
  # Labels and title
  xlab = "x", ylab = "y", main = "Our generated data")

# Run b_ols with and intercept
b_ols(data = the_data, y_var = "y", X_vars = "x", intercept = T)
b_ols(data = the_data, y_var = "y", X_vars = "x", intercept = F)
library(lfe)
# felm with an intercept:
felm(y ~ x, data = the_data) %>% summary()
# felm without an intercept:
felm(y ~ x - 1, data = the_data) %>% summary()

# The estimates
b_w <- b_ols(data = the_data, y_var = "y", X_vars = "x", intercept = T)
b_wo <- b_ols(data = the_data, y_var = "y", X_vars = "x", intercept = F)
# Plot the points
plot(
  # The variables for the plot
  x = the_data$x, y = the_data$y,
  # Labels and title
  xlab = "x", ylab = "y", main = "Our generated data")
# Plot the line from the 'with intercept' regression in yellow
abline(a = b_w[1], b = b_w[2], col = "lightblue", lwd = 3)
# Plot the line from the 'without intercept' regression in purple
abline(a = 0, b = b_w[1], col = "purple4", lwd = 2)
# Add a legend
legend(x = min(the_data$x), y = max(the_data$y),
  legend = c("with intercept", "w/o intercept"),
  # Line widths
  lwd = c(3, 2),
  # Colors
  col = c("lightblue", "purple4"),
  # No box around the legend
  bty = "n")

# FWL --------------------
# Options
options(stringsAsFactors = F)
# Load the packages
library(dplyr)
library(lfe)
library(readr)
library(MASS)
# Set the working directory
setwd("/Users/edwardarubin/Dropbox/Teaching/ARE212/Section04/")
# Load the dataset from CSV
cars <- read_csv("auto.csv")

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

b_ols(the_data, y_var = "y", X_vars = "x", intercept = T)
b_ols(the_data, y_var = "y", X_vars = "x", intercept = F)

resid_ols <- function(data, y_var, X_vars, intercept = TRUE) {
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
  # Calculate the sample size, n
  n <- nrow(X)
  # Calculate the residuals
  resids <- (diag(n) - X %*% solve(t(X) %*% X) %*% t(X)) %*% y
  # Return 'resids'
  return(resids)
}

# Working through FWL --------------------
# Steps 1 and 2: Residualize 'price' on 'weight' and an intercept
e_yx <- resid_ols(data = cars, y_var = "price",
  X_vars = "weight", intercept = T)
# Steps 3 and 4: Residualize 'mpg' on 'weight' and an intercept
e_xx <- resid_ols(data = cars, y_var = "mpg",
  X_vars = "weight", intercept = T)
# Combine the two sets of residuals into a data.frame
e_df <- data.frame(e_yx = e_yx[,1], e_xx = e_xx[,1])
# Step 5: Regress e_yx on e_xx without an intercept
b_ols(data = e_df, y_var = "e_yx",
  X_vars = "e_xx", intercept = F)

b_ols(data = cars, y_var = "price", X_vars = c("mpg", "weight"))

# Orthogonal covariates --------------------
# Set the seed
set.seed(12345)
# Set the sample size
n <- 1e5
# Generate x1, x2, and error from ind. N(0,1)
the_data <- tibble(
  x1 = rnorm(n),
  x2 = rnorm(n),
  e = rnorm(n))
# Calculate y = 1.5 x1 + 3 x2 + e
the_data <- mutate(the_data,
  y = 1.5 * x1 + 3 * x2 + e)

# Regression omitting 'x2'
b_ols(the_data, y_var = "y", X_vars = "x1", intercept = F)
# Regression including 'x2'
b_ols(the_data, y_var = "y", X_vars = c("x1", "x2"), intercept = F)

# Orthogonal covariates --------------------
# Load the MASS package
library(MASS)
# Create a var-covar matrix
v_cov <- matrix(data = c(1, 0.95, 0.95, 1), nrow = 2)
# Create the means vector
means <- c(5, 10)
# Define our sample size
n <- 1e5
# Set the seed
set.seed(12345)
# Generate x1 and x2
X <- mvrnorm(n = n, mu = means, Sigma = v_cov, empirical = T)
# Create a tibble for our data, add generate error from N(0,1)
the_data <- tbl_df(X) %>% mutate(e = rnorm(n))
# Set the names
names(the_data) <- c("x1", "x2", "e")
# The data-generating process
the_data <- the_data %>% mutate(y = 1 + 2 * x1 + 3 * x2 + e)

# Regression 1: y on int, x1, and x2
b_ols(the_data, y_var = "y", X_vars = c("x1", "x2"), intercept = T)
# Regression 2: y on int and x1
b_ols(the_data, y_var = "y", X_vars = "x1", intercept = T)
# Regression 3: y on int and x2
b_ols(the_data, y_var = "y", X_vars = "x2", intercept = T)

# Bad controls --------------------
# Create a var-covar matrix
v_cov <- matrix(data = c(1, 1 - 1e-6, 1 - 1e-6, 1), nrow = 2)
# Create the means vector
means <- c(5, 5)
# Define our sample size
n <- 1e5
# Set the seed
set.seed(12345)
# Generate x1 and x2
X <- mvrnorm(n = n, mu = means, Sigma = v_cov, empirical = T)
# Create a tibble for our data, add generate error from N(0,1)
the_data <- tbl_df(X) %>% mutate(e = rnorm(n))
# Set the names
names(the_data) <- c("x1", "x2", "e")
# The data-generating process
the_data <- the_data %>% mutate(y = 1 + 2 * x1 + e)

# Regression 1: y on int and x1 (DGP)
b_ols(the_data, y_var = "y", X_vars = "x1", intercept = T)
# Regression 2: y on int, x1, and x2
b_ols(the_data, y_var = "y", X_vars = c("x1", "x2"), intercept = T)
# Regression 3: y on int and x2
b_ols(the_data, y_var = "y", X_vars = "x2", intercept = T)

# R2 family --------------------
# Start the function
r2_ols <- function(data, y_var, X_vars) {
  # Create y and X matrices
  y <- to_matrix(data, vars = y_var)
  X <- to_matrix(data, vars = X_vars)
  # Add intercept column to X
  X <- cbind(1, X)
  # Find N and K (dimensions of X)
  N <- nrow(X)
  K <- ncol(X)
  # Calculate the OLS residuals
  e <- resid_ols(data, y_var, X_vars)
  # Calculate the y_star (demeaned y)
  y_star <- demeaner(N) %*% y
  # Calculate r-squared values
  r2_uc  <- 1 - t(e) %*% e / (t(y) %*% y)
  r2     <- 1 - t(e) %*% e / (t(y_star) %*% y_star)
  r2_adj <- 1 - (N-1) / (N-K) * (1 - r2)
  # Return a vector of the r-squared measures
  return(c("r2_uc" = r2_uc, "r2" = r2, "r2_adj" = r2_adj))
}

# Our r-squared function
r2_ols(data = cars, y_var = "price", X_vars = c("mpg", "weight"))
# Using 'felm'
felm(price ~ mpg + weight, cars) %>% summary()

# Overfitting --------------------
# Find the number of observations in cars
n <- nrow(cars)
# Fill an n-by-50 matrix with random numbers from N(0,1)
set.seed(12345)
rand_mat <- matrix(data = rnorm(n * 50, sd = 10), ncol = 50)
# Convert rand_mat to tbl_df
rand_df <- tbl_df(rand_mat)
# Change names to x1 to x50
names(rand_df) <- paste0("x", 1:50)
# Bind cars and rand_df; convert to tbl_df; call it 'cars_rand'
cars_rand <- cbind(cars, rand_df) %>% tbl_df()
# Drop the variable 'rep78' (it has NAs)
cars_rand <- dplyr::select(cars_rand, -rep78, -make)

result_df <- lapply(
  X = 2:ncol(cars_rand),
  FUN = function(k) {
    # Apply the 'r2_ols' function to columns 2:k
    r2_df <- r2_ols(
      data = cars_rand,
      y_var = "price",
      X_vars = names(cars_rand)[2:k]) %>%
      matrix(ncol = 3) %>% tbl_df()
    # Set names
    names(r2_df) <- c("r2_uc", "r2", "r2_adj")
    # Add a column for k
    r2_df$k <- k
    # Return r2_df
    return(r2_df)
  }) %>% bind_rows()

# Plot unadjusted r-squared
plot(x = result_df$k, y = result_df$r2,
  type = "l", lwd = 3, col = "lightblue",
  xlab = expression(paste("k: number of columns in ", bold("X"))),
  ylab = expression('R' ^ 2))
# Plot adjusted r-squared
lines(x = result_df$k, y = result_df$r2_adj,
  type = "l", lwd = 2, col = "purple4")
# The legend
legend(x = 0, y = max(result_df$r2),
  legend = c(expression('R' ^ 2),
    expression('Adjusted R' ^ 2)),
  # Line widths
  lwd = c(3, 2),
  # Colors
  col = c("lightblue", "purple4"),
  # No box around the legend
  bty = "n")
