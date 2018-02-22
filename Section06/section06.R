
# Initial setup ----
# Options
options(stringsAsFactors = F)
# Packages
library(pacman)
p_load(dplyr, magrittr, ggplot2, ggthemes)

# Anscombe's quartet ----
# Reformat Anscombe's dataset
a_df <- bind_rows(
  dplyr::select(anscombe, x = x1, y = y1),
  dplyr::select(anscombe, x = x2, y = y2),
  dplyr::select(anscombe, x = x3, y = y3),
  dplyr::select(anscombe, x = x4, y = y4))
# Add group identifier
a_df %<>% mutate(group = rep(paste0("Dataset ", 1:4), each = 11))
# The plot
ggplot(data = a_df, aes(x, y)) +
  # Plot the points
  geom_point() +
  # Add the regression line (without S.E. shading)
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  # Change the theme
  theme_pander() +
  theme(panel.border = element_rect(color = "grey90", fill=NA, size=1)) +
  # Plot by group
  facet_wrap(~group, nrow = 2, ncol = 2) +
  ggtitle("Illustrating Anscombe's Quartet",
    subtitle = "Four very different datasets with the same regression line")

# Setup again ----
# Options
options(stringsAsFactors = F)
# Packages
p_load(readr, dplyr, magrittr, ggplot2, ggthemes, viridis)
# Set working directory
dir_sec6 <- "/Users/edwardarubin/Dropbox/Teaching/ARE212/Spring2017/Section06/"
# Load data
cars <- paste0(dir_sec6, "auto.csv") %>% read_csv()

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
# Function for OLS table
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

# Our first plot ----
# A blank plot
ggplot(data = cars, aes(x = weight, y = price))
# Add points
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point()
# Add lines
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point() +
  geom_line()

# Adding regression lines ----
# Regress price on weight (with an intercept)
b <- ols(data = cars, y_var = "price", X_vars = "weight") %$% coef
# Estimated prices
price_hat <- function(weight) b[1] + b[2] * weight
# Add the line of predictions
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point() +
  stat_function(fun = price_hat)
# Change the color
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point() +
  stat_function(fun = price_hat, color = "blue")
# Change sizes
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(size = 2) +
  stat_function(fun = price_hat, color = "blue", size = 1.5)

# Using geom_smooth ----
# Add regression line
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")
# Quadratic
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))
# Change confidence interval
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
    level = 0.99)
# Remove confidence intervals
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)

# Color aesthetic ----
# Purple points
ggplot(data = cars, aes(x = weight, y = price, group = foreign)) +
  geom_point(color = "purple3")
# Change 'foreign' variable to logical
cars %<>% mutate(foreign = foreign == 1)
# Color by 'foreign'
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign))
# WRONG quadratic by 'foreign'
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))
# CORRECT quadratic by 'foreign'
ggplot(data = cars,
  aes(x = weight, y = price, color = foreign)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))
# Color by 'length'
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = length))

# Size aesthetic ----
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = length))

# Other aesthetics ----
# Shape
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), shape = 1)
# Alpha
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5)

# Labels ----
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset")

# Themes ----
# Pander theme
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  theme_pander()
# Stata theme
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  theme_stata() +
  theme(panel.ontop = F)
# Legend position
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  theme(legend.position = "bottom")
# Background coloring
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = NA))
# Draw a box
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA, color = "grey75"))
# Tick colors
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA, color = "grey75"),
    axis.ticks = element_line(color = "grey85"))
# Add a subtle grid
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA, color = "grey75"),
    axis.ticks = element_line(color = "grey85"),
    panel.grid.major = element_line(color = "grey90", size = 0.2),
    panel.grid.minor = element_line(color = "grey90", size = 0.2))
# Remove grey legend boxes
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA, color = "grey75"),
    axis.ticks = element_line(color = "grey85"),
    panel.grid.major = element_line(color = "grey95", size = 0.2),
    panel.grid.minor = element_line(color = "grey95", size = 0.2),
    legend.key = element_blank())
# Make our own theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey85"),
  panel.grid.major = element_line(color = "grey95", size = 0.2),
  panel.grid.minor = element_line(color = "grey95", size = 0.2),
  legend.key = element_blank())
# Apply our own theme
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  theme_ed

# More control ----
# Manual colors
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.5) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  scale_color_manual("Origin",
    values = c("grey70", "midnightblue"),
    labels = c("Domestic", "Foreign")) +
  theme_ed
# Manual colors from viridis()
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.65) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  scale_color_manual("Origin",
    values = viridis(2, end = 0.96),
    labels = c("Domestic", "Foreign")) +
  scale_size_continuous("Mileage") +
  theme_ed
# Title size in legend
ggplot(data = cars, aes(x = weight, y = price)) +
  geom_point(aes(color = foreign, size = mpg), alpha = 0.65) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  scale_color_manual("Origin",
    values = viridis(2, end = 0.96),
    labels = c("Domestic", "Foreign")) +
  scale_size_continuous("Mileage") +
  theme_ed
# Add quadratics with specific linetypes
ggplot(data = cars,
  aes(x = weight, y = price, color = foreign)) +
  geom_point(alpha = 0.65, aes(size = mpg)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
    se = F, size = 0.5, linetype = 2) +
  xlab("Weight (lbs)") +
  ylab("Price (USD)") +
  ggtitle("Trends in cars sold on the US market",
    subtitle = "From the world-famous autos dataset") +
  scale_color_manual("Origin",
    values = viridis(2, end = 0.96),
    labels = c("Domestic", "Foreign")) +
  scale_size_continuous("Mileage") +
  theme_ed

# Histograms and density plots ----
# Basic histogram
ggplot(data = cars, aes(x = weight)) +
  geom_histogram() +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  theme_ed
# Try 'color' aesthetic (and limit to 15 bins)
ggplot(data = cars, aes(x = weight)) +
  geom_histogram(bins = 15, color = "seagreen3") +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  theme_ed
# Try 'fill' aesthetic
ggplot(data = cars, aes(x = weight)) +
  geom_histogram(bins = 15, color = "seagreen3", fill = "grey90") +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  theme_ed
# Try filling by 'foreign'
ggplot(data = cars, aes(x = weight)) +
  geom_histogram(bins = 15, aes(fill = foreign)) +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  theme_ed
# Force the overlap
ggplot(data = cars, aes(x = weight)) +
  geom_histogram(aes(fill = foreign),
    bins = 15, position = "identity", alpha = 0.75) +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  theme_ed
# Clean up the overlap
ggplot(data = cars, aes(x = weight)) +
  geom_histogram(aes(color = foreign, fill = foreign),
    bins = 15, position = "identity", alpha = 0.4) +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  scale_color_manual("Origin",
    values = c("grey70", "seagreen3"),
    labels = c("Domestic", "Foreign")) +
  scale_fill_manual("Origin",
    values = c("grey60", "seagreen3"),
    labels = c("Domestic", "Foreign")) +
  theme_ed
# Try transparency
ggplot(data = cars, aes(x = weight)) +
  geom_histogram(aes(color = foreign, fill = foreign),
    bins = 15, position = "identity", alpha = 0.4) +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  scale_color_manual("Origin",
    values = c("seagreen3", "grey70"),
    labels = c("Domestic", "Foreign")) +
  scale_fill_manual("Origin",
    values = c(NA, "grey60"),
    labels = c("Domestic", "Foreign")) +
  theme_ed
# Try a density plot
ggplot(data = cars, aes(x = weight)) +
  geom_density(aes(color = foreign, fill = foreign),
    alpha = 0.4) +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  scale_color_manual("Origin",
    values = c("seagreen3", "grey70"),
    labels = c("Domestic", "Foreign")) +
  scale_fill_manual("Origin",
    values = c(NA, "grey60"),
    labels = c("Domestic", "Foreign")) +
  theme_ed
# Combine density and histogram
ggplot(data = cars, aes(x = weight)) +
  geom_histogram(aes(y = ..density..),
    bins = 15, color = NA, fill = rev(viridis(15))) +
  geom_density(fill = "grey55", color = "grey80", alpha = 0.2) +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  theme_ed

# Saving ----
# Save the last (printed) figure to pdf in current working directory
ggsave(filename = "ourNewFigure.pdf", width = 16, height = 10)
# Assign a figure to a name
our_histogram <- ggplot(data = cars, aes(x = weight)) +
  geom_histogram(aes(y = ..density..),
    bins = 15, color = NA, fill = rev(viridis(15))) +
  geom_density(fill = "grey55", color = "grey80", alpha = 0.2) +
  xlab("Weight (lbs)") +
  ggtitle("The distribution of weight for cars sold in the US",
    subtitle = "From the world-famous autos dataset") +
  theme_ed
# Save named plot as png to desktop
ggsave(filename = "anotherHistogram.png", plot = our_histogram,
  path = "/Users/edwardarubin/Desktop", width = 10, height = 8)

# Plotting functions ----
# Define the inverse demand and supply functions
inv_demand <- function(q) 10 - q
inv_supply <- function(q) 1.5 * q

# Plot the inverse demand and supply functions
curve(expr = inv_demand, from = 0, 10,
  xlab = "Q", ylab = "P", n = 100)
curve(expr = inv_supply,
  from = 0, 10, add = T)
# Shift the supply back
curve(expr = inv_supply(x) + 3,
  from = 0, 10, add = T, col = "red")

# Plot supply and demand with ggplot2
ggplot(data = data.frame(x = c(0, 10)), aes(x)) +
  # The inverse demand
  stat_function(fun = inv_demand, geom = "line") +
  # The inverse supply
  stat_function(fun = inv_supply, geom = "line") +
  # The shifted inverse supply curve
  stat_function(fun = function(x) inv_supply(x) + 3, geom = "line",
    linetype = 2) +
  # Labels and themes
  xlab("Quantity") +
  ylab("Price") +
  ggtitle("Classic economics figure") +
  theme_ed

# Solve for equilibria
# The first equilibrium quantity
q1 <- uniroot(
  f = function(x) inv_supply(x) - inv_demand(x),
  interval = c(0, 10))$root
p1 <- inv_demand(q1)
# The second equilibrium equantity
q2 <- uniroot(
  f = function(x) inv_supply(x) + 3 - inv_demand(x),
  interval = c(0, 10))$root
p2 <- inv_demand(q2)

# Plot supply and demand with annotations in ggplot2
ggplot(data = data.frame(x = c(0, 10)), aes(x)) +
  # The inverse demand
  stat_function(fun = inv_demand, geom = "line") +
  # The inverse supply
  stat_function(fun = inv_supply, geom = "line") +
  # The shifted inverse supply curve
  stat_function(fun = function(x) inv_supply(x) + 3, geom = "line",
    linetype = 2) +
  # Annotate!
  annotate(geom = "point", x = c(q1, q2), y = c(p1, p2), size = 2.5) +
  annotate(geom = "text", x = c(q1, q2), y = c(p1, p2) + 1,
    label = c("EQ[0]", "EQ[1]"), parse = T) +
  annotate(geom = "text", x = 9.5,
    y = c(inv_supply(9.5)-0.7, inv_supply(9.5)+3-0.7, inv_demand(9.5)+0.7),
    label = c("S[0]", "S[1]", "D"), parse = T) +
  # Labels and themes
  xlab("Quantity") +
  ylab("Price") +
  ggtitle("Classic economics figure") +
  theme_ed

# Plot density of t distribution with 29 df
ggplot(data = data.frame(x = c(-4,4)), aes(x)) +
  # Plot the pdf
  stat_function(fun = function(x) dt(x, df = 29),
    color = "grey75") +
  ggtitle(expression(paste("The beautiful ", italic(t),
    " distribution"))) +
  xlab(expression(italic(t))) +
  ylab("Density") +
  theme_ed
# Shade in the distribution
ggplot(data = data.frame(x = c(-4,4)), aes(x)) +
  # Plot the pdf
  stat_function(fun = function(x) dt(x, df = 29),
    geom = "area", color = "grey65", fill = "grey65", alpha = 0.4) +
  ggtitle(expression(paste("The beautiful ", italic(t),
    " distribution"))) +
  xlab(expression(italic(t))) +
  ylab("Density") +
  theme_ed
# Shade the rejection region
ggplot(data.frame(x = c(-4,4))) +
  # Plot the pdf
  stat_function(
    fun = function(x) dt(x, df = 29),
    aes(x),
    geom = "area", color = "grey75", fill = "grey75", alpha = 0.4) +
  # Shade below -2
  stat_function(
    fun = function(x) ifelse(x <= -2, dt(x, df = 29), NA),
    aes(x),
    geom = "area", color = NA, fill = "grey40", alpha = 0.7) +
  # Shade above 2
  stat_function(
    fun = function(x) ifelse(x >= 2, dt(x, df = 29), NA),
    aes(x),
    geom = "area", color = NA, fill = "grey40", alpha = 0.7) +
  ggtitle(expression(paste("The beautiful ", italic(t),
    " distribution"))) +
  xlab(expression(italic(t))) +
  ylab(expression(paste("Density, ", italic(f(t))))) +
  theme_ed
