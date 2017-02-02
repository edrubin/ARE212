
# Setup ------------------------------------------------------------------------
# Options
options(stringsAsFactors = F)
# Packages
library(haven)
library(dplyr)
# Set the directory
setwd("/Users/edwardarubin/Dropbox/Teaching/ARE212/Section03/")

# Simple sample function -------------------------------------------------------
# Define the function (named 'triple_prod')
  triple_prod <- function(x, y, z) {
    # Take the product of the three arguments
    tmp_prod <- x * y * z
    # Return 'tmp_prod'
    return(tmp_prod)
  }

# Test the function
triple_prod(x = 2, y = 3, z = 5)

# Load the data ----------------------------------------------------------------
cars <- read_dta(file = "auto.dta")

# Our OLS function -------------------------------------------------------------
# Define the function
b_ols <- function(data, y, X) {
  # Require the 'dplyr' package
  require(dplyr)
  
  # Select y variable data from 'data'
  y_data <- select_(data, .dots = y)
  # Convert y_data to matrices
  y_data <- as.matrix(y_data)
  
  # Select X variable data from 'data'
  X_data <- select_(data, .dots = X)
  # Add a column of ones to X_data
  X_data <- mutate_(X_data, "ones" = 1)
  # Move the intercept column to the front
  X_data <- select_(X_data, "ones", .dots = X)
  # Convert X_data to matrices
  X_data <- as.matrix(X_data)
  
  # Calculate beta hat
  beta_hat <- solve(t(X_data) %*% X_data) %*% t(X_data) %*% y_data
  # Change the name of 'ones' to 'intercept'
  rownames(beta_hat) <- c("intercept", X)
  # Return beta_hat
  return(beta_hat)
}

# Revised OLS function ---------------------------------------------------------
# Revise OLS function using pipes
b_ols <- function(data, y, X) {
  # Require the 'dplyr' package
  require(dplyr)
  
  # Create the y matrix
  y_data <- data %>%
    # Select y variable data from 'data'
    select_(.dots = y) %>%
    # Convert y_data to matrices
    as.matrix()
  
  # Create the X matrix
  X_data <- data %>%
    # Select X variable data from 'data'
    select_(.dots = X) %>%
    # Add a column of ones to X_data
    mutate_("ones" = 1) %>%
    # Move the intercept column to the front
    select_("ones", .dots = X) %>%
    # Convert X_data to matrices
    as.matrix()
  
  # Calculate beta hat
  beta_hat <- solve(t(X_data) %*% X_data) %*% t(X_data) %*% y_data
  # Change the name of 'ones' to 'intercept'
  rownames(beta_hat) <- c("intercept", X)
  # Return beta_hat
  return(beta_hat)
}

# lapply meets b_ols -----------------------------------------------------------
# Define the target outcome variables
target_vars <- c("price", "headroom", "trunk", "length", "turn",
  "displacement", "gear_ratio", "foreign")
# The 'lapply' call
results_list <- lapply(
  X = target_vars,
  FUN = function(i) b_ols(data = cars, y = i, X = c("mpg", "weight"))
)
# The results
results_list
# Cleaning up the results list
results_df <- lapply(X = results_list, FUN = data.frame) %>% bind_cols()
# We lose the row names in the process; add them back
rownames(results_df) <- c("intercept", "mpg", "weight")
# Check out results_df
results_df

# A function to calculate bias in one draw -------------------------------------
data_baker <- function(sample_n, true_beta) {
  # First generate x from N(0,1)
  x <- rnorm(sample_n)
  # Now the error from N(0,1)
  e <- rnorm(sample_n)
  # Now combine true_beta, x, and e to get y
  y <- true_beta[1] + true_beta[2] * x + e
  # Define the data matrix of independent vars.
  X <- cbind(1, x)
  # Force y to be a matrix
  y <- matrix(y, ncol = 1)
  # Calculate the OLS estimates
  b_ols <- solve(t(X) %*% X) %*% t(X) %*% y
  # Convert b_ols to vector
  b_ols <- b_ols %>% as.vector()
  # Calculate bias, force to 2x1 data.frame()
  the_bias <- (true_beta - b_ols) %>%
    matrix(ncol = 2) %>% data.frame()
  # Set names
  names(the_bias) <- c("bias_intercept", "bias_x")
  # Return the bias
  return(the_bias)
}
# Set seed
set.seed(12345)
# Run once
data_baker(sample_n = 100, true_beta = c(1, 3))


# A function to run a full simulation ------------------------------------------
bias_simulator <- function(n_sims, sample_n, true_beta) {
  
  # A function to calculate bias
  data_baker <- function(sample_n, true_beta) {
    # First generate x from N(0,1)
    x <- rnorm(sample_n)
    # Now the error from N(0,1)
    e <- rnorm(sample_n)
    # Now combine true_beta, x, and e to get y
    y <- true_beta[1] + true_beta[2] * x + e
    # Define the data matrix of independent vars.
    X <- cbind(1, x)
    # Force y to be a matrix
    y <- matrix(y, ncol = 1)
    # Calculate the OLS estimates
    b_ols <- solve(t(X) %*% X) %*% t(X) %*% y
    # Convert b_ols to vector
    b_ols <- b_ols %>% as.vector()
    # Calculate bias, force to 2x1 data.frame()
    the_bias <- (true_beta - b_ols) %>%
      matrix(ncol = 2) %>% data.frame()
    # Set names
    names(the_bias) <- c("bias_intercept", "bias_x")
    # Return the bias
    return(the_bias)
  }
  
  # Run data_baker() n_sims times with given parameters
  sims_dt <- lapply(
    X = 1:n_sims,
    FUN = function(i) data_baker(sample_n, true_beta)) %>%
    # Bind the rows together to output a nice data.frame
    bind_rows()
  
  # Return sim_dt
  return(sims_dt)
}

# Set seed
set.seed(12345)
# Run it
sim_dt <- bias_simulator(n_sims = 1e4, sample_n = 100, true_beta = c(1,3))
# Check the results with a histogram
hist(sim_dt[,2],
  breaks = 30,
  main = "Is OLS biased for x's coefficient?",
  xlab = "Bias")
# Emphasize the zero line
abline(v = 0, col = "blue", lwd = 3)

