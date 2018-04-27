
# General R setup ----
# Options
options(stringsAsFactors = F)
# Load new packages
library(pacman)
p_load(ggmap, leaflet)
# Load old packages
p_load(dplyr, ggplot2, ggthemes, parallel, magrittr, viridis)
# My ggplot2 theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  # panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey95", size = 0.3),
  panel.grid.major = element_line(color = "grey95", size = 0.3),
  panel.grid.minor = element_line(color = "grey95", size = 0.3),
  legend.key = element_blank())
# My directories
dir_14 <- "/Users/edwardarubin/Dropbox/Teaching/ARE212/Section14/"

# Define the structural parameters ----
# Gamma
Gamma <- matrix(
  data = c(-1, 0, 1, 2, -1, 0, 0, 3, -1),
  nrow = 3,
  byrow = T
)
# Beta
Beta <- matrix(
  data = c(0, 1, 2, 3, 4, 0, 5, 0, 0),
  nrow = 3,
  byrow = T
)

# Generate data ----
# Set the sample size
N <- 1e3
# Set a seed
set.seed(12345)
# Generate the data for X and E
x_df <- data_frame(
  x1 = rnorm(N),
  x2 = rnorm(N),
  x3 = rnorm(N)
)
e_df <- data_frame(
  e1 = rnorm(N),
  e2 = rnorm(N),
  e3 = rnorm(N)
)
# Generate the y variables
y_mat <- (-1) * as.matrix(x_df) %*% Beta %*% solve(Gamma) -
  as.matrix(e_df) %*% solve(Gamma)
# Force to data frame
y_df <- y_mat %>% as_data_frame()
names(y_df) <- c("y1", "y2", "y3")
# Join the data together
the_df <- bind_cols(y_df, x_df)

# Estimation ----
# The LHS variable in eq. 1
yi <- y_mat[,1]
# The RHS variables in eq. 1
Zi <- bind_cols(y_df[,2], x_df[,2:3]) %>% as.matrix()
# OLS estimates
solve(t(Zi) %*% Zi) %*% t(Zi) %*% yi
# Matrix of all exogenous variables in the system
X <- x_df %>% as.matrix()
# First-stage fitted values
Zi_hat <- X %*% solve(t(X) %*% X) %*% t(X) %*% Zi
# 2SLS estimates
solve(t(Zi_hat) %*% Zi_hat) %*% t(Zi_hat) %*% yi

# Simulation ----
# Clean up from the other 'simulation'
rm(N, x_df, e_df, y_mat, y_df, the_df, yi, Zi, X, Zi_hat); gc()
# Generate population data:
# Set the population size
N <- 1e5
# Set a seed
set.seed(12345)
# Generate the data for X and E
pop_x <- data_frame(
  x1 = rnorm(N),
  x2 = rnorm(N),
  x3 = rnorm(N)
)
pop_e <- data_frame(
  e1 = rnorm(N),
  e2 = rnorm(N),
  e3 = rnorm(N)
)
# Generate the y variables
pop_y <- (-1) * as.matrix(pop_x) %*% Beta %*% solve(Gamma) -
  as.matrix(pop_e) %*% solve(Gamma)
# Force to data frame
y_df <- pop_y %>% as_data_frame()
names(y_df) <- c("y1", "y2", "y3")
# Join the data together
pop_df <- bind_cols(y_df, pop_x)
# Clean up
rm(pop_x, pop_e, pop_y, y_df); gc()

# Function: Run a single iteration of the simulation ----
fun_iter <- function(i, data, sample_size) {
  # Sample from the population
  smpl_df <- sample_n(tbl = data, size = sample_size)
  # The LHS variable in eq.1
  yi <- smpl_df %>% select(y1) %>% as.matrix()
  # The RHS variables in eq. 1
  Zi <- smpl_df %>% select(y2, x2, x3) %>% as.matrix()
  # OLS estimates
  d_ols <- solve(t(Zi) %*% Zi) %*% t(Zi) %*% yi
  # Matrix of all exogenous variables in the system
  X <- smpl_df %>% select(starts_with("x")) %>% as.matrix()
  # First-stage fitted values
  Zi_hat <- X %*% solve(t(X) %*% X) %*% t(X) %*% Zi
  # 2SLS estimates
  d_2sls <- solve(t(Zi_hat) %*% Zi_hat) %*% t(Zi_hat) %*% yi
  # Create results data frame
  result_df <- rbind(t(d_ols), t(d_2sls)) %>% as_data_frame()
  # Add method and iteration variables
  result_df %<>% mutate(
    method = c("OLS", "2SLS"),
    iter = i
  )
  # Return the results
  return(result_df)
}

# Run the simulation ----
t1 <- proc.time()
# Run fun_iter() 10,000 times
sim_df <- mclapply(
  X = 1:1e4,
  FUN = fun_iter,
  mc.cores = 3,
  data = pop_df,
  sample_size = 1e3
) %>% bind_rows()
t2 <- proc.time()
t2 - t1
```

# Plot simulation results ----
# gamma 21
ggplot(data = sim_df, aes(x = y2, fill = method)) +
  geom_density(color = NA, alpha = 0.9) +
  geom_vline(xintercept = Gamma[2,1], color = viridis(3, option = "C")[2]) +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    x = expression(Parameter~estimates:~gamma[21]),
    y = "Density"
  ) +
  ggtitle(
    expression(paste("Simultaneity bias: Comparing OLS and 2SLS in estimating equation 1 parameter ",
      gamma[21])),
    subtitle = "10,000 iterations with sample size 1,000"
  ) +
  scale_fill_viridis(
    "Estimation method:",
    discrete = T, option = "C", begin = 0.15, end = 0.85
  ) +
  theme_ed
# beta 21
ggplot(data = sim_df, aes(x = x2, fill = method)) +
  geom_density(color = NA, alpha = 0.9) +
  geom_vline(xintercept = Beta[2,1], color = viridis(3, option = "C")[2]) +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    x = expression(Parameter~estimates:~beta[21]),
    y = "Density"
  ) +
  ggtitle(
    expression(paste("Simultaneity bias: Comparing OLS and 2SLS in estimating equation 1 parameter ",
      beta[21])),
    subtitle = "10,000 iterations with sample size 1,000"
  ) +
  scale_fill_viridis(
    "Estimation method:",
    discrete = T, option = "C", begin = 0.15, end = 0.85
  ) +
  theme_ed
# beta 31
ggplot(data = sim_df, aes(x = x3, fill = method)) +
  geom_density(color = NA, alpha = 0.9) +
  geom_vline(xintercept = Beta[3,1], color = viridis(3, option = "C")[2]) +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    x = expression(Parameter~estimates:~beta[31]),
    y = "Density"
  ) +
  ggtitle(
    expression(paste("Simultaneity bias: Comparing OLS and 2SLS in estimating equation 1 parameter ",
      beta[31])),
    subtitle = "10,000 iterations with sample size 1,000"
  ) +
  scale_fill_viridis(
    "Estimation method:",
    discrete = T, option = "C", begin = 0.15, end = 0.85
  ) +
  theme_ed
