

# Between ----
# Define x
x <- 3
# The "normal" way
(x >= 1) & (x <= 5)
# The "between" way
dplyr::between(x, 1, 5)

# Setup ----
# Options
options(stringsAsFactors = F)
options(scipen = 10)
# Packages
p_load(dplyr, magrittr, parallel,
  ggplot2, ggthemes, viridis, grid, gtable, gridExtra)
# Directory
setwd("/Users/edwardarubin/Dropbox/Teaching/ARE212/Section08")
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

# Bernoulli-Gamma ----
rbigamma <- function(n, prob = c(0.5, 0.5)) {
  # Draw n samples of T or F (our 'coin flip') with replacement
  flips <- base::sample(x = c(T, F), size = n,
    replace = T, prob = prob)
  # Draw n samples from Gamma with shape=5 and scale=1 (mean=7)
  # substract the mean (7) from the draws
  gamma_7 <- rgamma(n = n, shape = 7, scale = 1)
  # Draw n samples from Gamma with shape=1 and scale=1 (mean=1)
  # substract the mean (1) from the draws
  gamma_1 <- rgamma(n = n, shape = 1, scale = 1)
  # Combine the flips and the two gammas' draws
  bi_gamma <- flips * gamma_7 + (!flips) * gamma_1
  # Demean the bimodal variables (weighting by 'prob')
  bi_gamma <- bi_gamma - 7 * prob[1] - 1 * prob[2]
}

# Histogram
set.seed(12345)
qplot(rbigamma(1e5), geom = "histogram")
# Summary
set.seed(12345)
summary(rbigamma(1e5))

# Simulation population ----
# Set the seed
set.seed(12345)
# Define the population size
N <- 1e5
# Define the true beta
beta <- 5
# Start the population
pop_df <- data.frame(
  ones   = 1,
  x      = runif(N, min = 0, max = 100),
  e_norm = rnorm(n = N),
  e_unif = runif(n = N, min = -5, max = 5),
  e_pois = rpois(n = N, lambda = 1) - 1,
  e_bg   = rbigamma(n = N)
  ) %>% tbl_df()
# Calculate the outcome variable: y = 1 + beat * x + error
pop_df %<>% mutate(
  y_norm = 1 + beta * x + e_norm,
  y_unif = 1 + beta * x + e_unif,
  y_pois = 1 + beta * x + e_pois,
  y_bg   = 1 + beta * x + e_bg
  )

# Disturbance plot ----
# Generate 4 colors from 'viridis'
c4 <- viridis(n = 4, begin = 0.1, end = 0.8)
# Generate 4 slightly lighter colors from 'viridis'
c4_l <- viridis(n = 4, begin = 0.4, end = 1)
# Make a plot for the normal disturbances
gg_norm <- ggplot(data = pop_df, aes(x = e_norm)) +
  geom_histogram(fill = c4[1], color = c4_l[1],
    size = 0.1, bins = 30) +
  xlab("Normal") +
  ylab("Count") +
  theme_ed
# Make a plot for the uniform disturbances
gg_unif <- ggplot(data = pop_df, aes(x = e_unif)) +
  geom_histogram(fill = c4[2], color = c4_l[2],
    size = 0.1, bins = 30) +
  xlab("Uniform") +
  ylab("Count") +
  theme_ed
# Make a plot for the poisson disturbances
gg_pois <- ggplot(data = pop_df, aes(x = e_pois)) +
  geom_histogram(fill = c4[3], color = c4_l[3],
    size = 0.1, bins = 30) +
  xlab("Poisson") +
  ylab("Count") +
  theme_ed
# Make a plot for the bimodal gamma disturbances
gg_bg <- ggplot(data = pop_df, aes(x = e_bg)) +
  geom_histogram(fill = c4[4], color = c4_l[4],
    size = 0.1, bins = 30) +
  xlab("Bernoulli-Gamma") +
  ylab("Count") +
  theme_ed
# Combine the plots
gg_errors <- arrangeGrob(gg_norm, gg_unif, gg_pois, gg_bg,
  # Two columns
  ncol = 2,
  # Title on top
  top = "Distributions of our disturbances")
# Print the grid the screen
grid.draw(gg_errors)

pop_df %>% select(starts_with("e_")) %>% summarize_each(funs = "mean")

# Iteration function ----
one_iter <- function(n, data) {
  # Draw 'n' observations from 'data'
  tmp_df <- sample_n(tbl = data, size = n)
  # Define the X matrix (same across regressions)
  x_mat <- to_matrix(tmp_df, c("ones", "x"))
  # Estimate OLS for each 'y'
  b_norm = b_ols(
    y = to_matrix(tmp_df, "y_norm"),
    X = x_mat)[2]
  b_unif = b_ols(
    y = to_matrix(tmp_df, "y_unif"),
    X = x_mat)[2]
  b_pois = b_ols(
    y = to_matrix(tmp_df, "y_pois"),
    X = x_mat)[2]
  b_bg = b_ols(
    y = to_matrix(tmp_df, "y_bg"),
    X = x_mat)[2]
  # Create a data.frame to return
  coef_df <- data.frame(
    # The estimates
    est = c(b_norm, b_unif, b_pois, b_bg),
    # The distributions
    dist = c("normal", "uniform", "poisson", "bi-gamma"),
    # The sample size
    n = n
    )
  # Return coef_df
  return(coef_df)
}

# Simulation function ----
run_sim <- function(n, n_iter, data, n_cores = 4) {
  # Required packages
  require(dplyr)
  require(parallel)
  require(magrittr)
  # Run 'one_iter' 'n_iter' times with sample size 'n'
  run_df <- mclapply(
    X = rep(n, n_iter),
    FUN = one_iter,
    data = data,
    mc.cores = n_cores) %>% bind_rows() %>% tbl_df()
  # Return run_df
  return(run_df)
}

# 'rep' example ----
rep(c(10, 25, 1000, 10000), 3)
rep(c(10, 25, 1000, 10000), each = 3)

# Update simulation function ----
run_sim <- function(n, n_iter, data, n_cores = 4) {
  # Required packages
  require(dplyr)
  require(parallel)
  require(magrittr)
  # Run 'one_iter' 'n_iter' times with sample size 'n'
  run_df <- mclapply(
    X = rep(n, each = n_iter),
    FUN = one_iter,
    data = data,
    mc.cores = n_cores) %>% bind_rows() %>% tbl_df()
  # Return run_df
  return(run_df)
}

# Run the simulation ----
# Set the seed (again)
set.seed(12345)
# Define our desired sample sizes
sizes <- c(25, 100, 1e3, 1e4)
# Run the simulation function 'run_sim()'
sim_df <- run_sim(n = sizes, n_iter = 1e4,
  data = select(pop_df, -starts_with("e")))

# Plot normal simulation results ----
ggplot(data = filter(sim_df, dist == "normal"),
  aes(x = est, fill = as.character(n))) +
  geom_density(stat = "density", alpha = 0.6, size = 0.05) +
  xlab(expression(b[OLS])) +
  ylab("Density") +
  ggtitle(paste("Distribution of OLS coefficients",
    "with normal disturbances")) +
  scale_fill_viridis("Sample size:", discrete = T,
    direction = -1) +
  theme_ed

# Characters can be weird ----
# Numeric:
100 < 23
# Characters:
"100" < "23"

# Factor variables ----
# Add a factor version of 'n'
sim_df %<>% mutate(
  n_fac = factor(
    x       = n,
    levels  = sizes,
    labels  = prettyNum(sizes, big.mark=",", scientific = F),
    ordered = T)
  )
# Check our new variable
sim_df$n_fac %>% head()
sim_df$n_fac %>% tail()

# Plot again ----
ggplot(data = filter(sim_df, dist == "normal"),
  aes(x = est, fill = n_fac)) +
  geom_density(stat = "density", alpha = 0.6, size = 0.05) +
  xlab(expression(b[OLS])) +
  ylab("Density") +
  ggtitle(paste("Distribution of OLS coefficients",
    "with normal disturbances")) +
  scale_fill_viridis("Sample size:", discrete = T,
    direction = -1) +
  theme_ed

# More factors: dist ----
sim_df %<>% mutate(dist_fac = factor(
  x = dist,
  levels = c("bi-gamma", "normal", "poisson", "uniform"),
  labels = c("Bernoulli-Gamma", "Normal", "Poisson", "Uniform")
  ))

# Facet plot ----
ggplot(data = sim_df,
  aes(x = est, fill = n_fac)) +
  geom_density(stat = "density", alpha = 0.6, size = 0.05) +
  xlab(expression(b[OLS])) +
  ylab("Density") +
  ggtitle(paste("Distribution of OLS coefficients",
    "with normal disturbances")) +
  scale_fill_viridis("Sample size:", discrete = T,
    direction = -1) +
  facet_grid(dist_fac ~ ., scales = "free_y") +
  theme_ed

# Summarize ----
sim_df %>% group_by(dist_fac, n_fac) %>%
  summarize(mean = mean(est), std_dev = sd(est)) %>%
  knitr::kable(digits = 4,
    col.names = c("Distribution", "N", "Mean", "Std. Dev."))

# Summaries of our point estimates
est_df <- sim_df %>%
  filter(n == 1e4) %>%
  group_by(dist) %>%
  summarize(mean_est = mean(est), sd_est = sd(est))

# Plot results ----
# Normal disturbances
sim_norm <- ggplot(aes(x = est),
  data = filter(sim_df, dist == "normal", n == 1e4)) +
  geom_histogram(aes(y = ..density..),
    fill = "grey90", color = "grey65",
    size = 0.1, bins = 50) +
  geom_line(stat = "density",
    color = "violetred2", size = 0.6) +
  stat_function(
    geom = "line", fun = dnorm,
    color = "slateblue4", linetype = "dashed", size = 0.8,
    args = list(
      mean = filter(est_df, dist == "normal")$mean_est,
      sd = filter(est_df, dist == "normal")$sd_est)
    ) +
  ggtitle("Normal") +
  xlab("") + ylab("") +
  theme_ed
# Uniform disturbances
sim_unif <- ggplot(aes(x = est),
  data = filter(sim_df, dist == "uniform", n == 1e4)) +
  geom_histogram(aes(y = ..density..),
    fill = "grey90", color = "grey65",
    size = 0.1, bins = 50) +
  geom_line(stat = "density",
    color = "violetred2", size = 0.6) +
  stat_function(
    geom = "line", fun = dnorm,
    color = "slateblue4", linetype = "dashed", size = 0.8,
    args = list(
      mean = filter(est_df, dist == "uniform")$mean_est,
      sd = filter(est_df, dist == "uniform")$sd_est)
    ) +
  ggtitle("Uniform") +
  xlab("") + ylab("") +
  theme_ed
# Uniform disturbances
sim_pois <- ggplot(aes(x = est),
  data = filter(sim_df, dist == "poisson", n == 1e4)) +
  geom_histogram(aes(y = ..density..),
    fill = "grey90", color = "grey65",
    size = 0.1, bins = 50) +
  geom_line(stat = "density",
    color = "violetred2", size = 0.6) +
  stat_function(
    geom = "line", fun = dnorm,
    color = "slateblue4", linetype = "dashed", size = 0.8,
    args = list(
      mean = filter(est_df, dist == "poisson")$mean_est,
      sd = filter(est_df, dist == "poisson")$sd_est)
    ) +
  ggtitle("Poisson") +
  xlab("") + ylab("") +
  theme_ed
# Uniform disturbances
sim_bg <- ggplot(aes(x = est),
  data = filter(sim_df, dist == "bi-gamma", n == 1e4)) +
  geom_histogram(aes(y = ..density..),
    fill = "grey90", color = "grey65",
    size = 0.1, bins = 50) +
  geom_line(stat = "density",
    color = "violetred2", size = 0.6) +
  stat_function(
    geom = "line", fun = dnorm,
    color = "slateblue4", linetype = "dashed", size = 0.8,
    args = list(
      mean = filter(est_df, dist == "bi-gamma")$mean_est,
      sd = filter(est_df, dist == "bi-gamma")$sd_est)
    ) +
  ggtitle("Bernoulli-Gamma") +
  xlab("") + ylab("") +
  theme_ed

# 'Join' the plots
gg_sim <- arrangeGrob(
  sim_norm, sim_unif, sim_pois, sim_bg,
  # Two columns
  ncol = 2,
  # Title on top
  top = textGrob(expression(paste(
    "Asymptotic distributions of ", b[OLS])),
    gp = gpar(fontsize = 16, font = 3), check = T),
  left = textGrob("Density", vjust = 2, rot = 90),
  bottom = textGrob(expression(b[OLS]), vjust = -1)
  )
# Print the grid the screen
grid.draw(gg_sim)

# Shapiro-Wilk test ----
# Set the seed
set.seed(12345)
# The Shapiro-Wilk test
sim_df %>%
  # Subset for Poisson and n = 10,000
  filter(dist == "poisson", n == 1e4) %>%
  # Sample 5,000 of the rows
  sample_n(size = 5e3) %$%
  # Grab the variable 'est'
  est %>%
  # The actual Shaprio-Wilk test function
  shapiro.test()

# K-S test ----
ks.test(
  x = filter(sim_df, dist == "poisson", n == 1e4)$est,
  y = "pnorm",
  mean = filter(est_df, dist == "poisson")$mean_est,
  sd = filter(est_df, dist == "poisson")$sd_est)

# Pareto simulation ----
# Load the VGAM package
p_load(VGAM)
# Set the seed
set.seed(12345)
# Define the population size
N_par <- 1e6
# Create a population
par_df <- data.frame(e = rpareto(N_par, scale = 2, shape = 1))
# Demean
par_df %<>% mutate(e = e - mean(e))
# Add x and ones
par_df %<>% mutate(
  ones = 1,
  x    = runif(N_par, 0, 100),
  y    = 1 + 5 * x + e
  ) %>% tbl_df()

quantile(par_df$e, probs = seq(0, 1, 0.1))

one_par <- function(n, data) {
  # Draw 'n' observations from 'data'
  tmp_df <- sample_n(tbl = data, size = n)
  # Estimate OLS
  b_par = b_ols(
    y   = to_matrix(tmp_df, "y"),
    X   = to_matrix(tmp_df, c("ones", "x")))[2]
  # Create a data.frame to return
  coef_df <- data.frame(
    # The estimates
    est = b_par,
    # The sample size
    n = n
    )
  # Return coef_df
  return(coef_df)
}

run_par <- function(n, n_iter, data, n_cores = 4) {
  # Required packages
  require(dplyr)
  require(parallel)
  require(magrittr)
  # Run 'one_par' 'n_iter' times with sample size 'n'
  run_df <- mclapply(
    X = rep(n, each = n_iter),
    FUN = one_par,
    data = data,
    mc.cores = n_cores) %>% bind_rows() %>% tbl_df()
  # Return run_df
  return(run_df)
}

par_10k <- run_par(n = 1e4, n_iter = 1e4, data = par_df)

# Pareto disturbances
ggplot(data = par_10k, aes(x = est)) +
  geom_histogram(aes(y = ..density..),
    fill = "grey90", color = "grey65",
    size = 0.1, bins = 50) +
  geom_line(stat = "density",
    color = "violetred2", size = 0.6) +
  stat_function(
    geom = "line", fun = dnorm,
    color = "slateblue4", linetype = "dashed", size = 0.8,
    args = list(
      mean = mean(par_10k$est),
      sd = sd(par_10k$est))
    ) +
  ggtitle("Distribution of coefficients from Pareto disturbances",
    subtitle = "Sample size of 10,000") +
  xlab(expression(b[OLS])) +
  ylab("Density") +
  theme_ed

ks.test(
  x    = par_10k$est,
  y    = "pnorm",
  mean = mean(par_10k$est),
  sd   = sd(par_10k$est))

par_100k <- run_par(n = 1e5, n_iter = 1e4, data = par_df)

# Pareto disturbances
ggplot(data = par_100k, aes(x = est)) +
  geom_histogram(aes(y = ..density..),
    fill = "grey90", color = "grey65",
    size = 0.1, bins = 50) +
  geom_line(stat = "density",
    color = "violetred2", size = 0.6) +
  stat_function(
    geom = "line", fun = dnorm,
    color = "slateblue4", linetype = "dashed", size = 0.8,
    args = list(
      mean = mean(par_100k$est),
      sd = sd(par_10k$est))
    ) +
  ggtitle("Distribution of coefficients from Pareto disturbances",
    subtitle = "Sample size of 100,000") +
  xlab(expression(b[OLS])) +
  ylab("Density") +
  theme_ed

ks.test(
  x    = par_100k$est,
  y    = "pnorm",
  mean = mean(par_100k$est),
  sd   = sd(par_100k$est))
