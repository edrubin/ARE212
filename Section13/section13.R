# General R setup ----
# Options
options(stringsAsFactors = F)
# Load packages
pacman::p_load(data.table, magrittr)
# Add an option for data.table
options(datatable.print.nrows = 20)

# Creating a data table ----
# Set the seed
set.seed(12345)
# Create our data table
our_dt <- data.table(
  a = 101:130,
  b = rnorm(30),
  c = runif(30),
  d = rep(1:5))

# Check the class
our_dt %>% class()
# Check the dimensions, length, and names
our_dt %>% dim()
our_dt %>% length()
our_dt %>% names()

# Print our_dt to screen
our_dt

# Indexing columns ----
our_dt$b
our_dt[, b]
our_dt[, "b"]
our_dt[, 2]
our_dt[["b"]]
our_dt[[2]]

# Define 'my_col' as "b"
my_col <- "b"
# The erroneous use
our_dt[, my_col]
# The list use
our_dt[[my_col]]
# The 'with = F' option
our_dt[, my_col, with = F]
# The double-period option
our_dt[, ..my_col]

# Accessing multiple columns ----
# Using 'list'
our_dt[, list(a, b)]
# Using the period
our_dt[, .(a, b)]

# List with numbers? No.
our_dt[, list(1, 2)]
# c() with numbers and 'with = F'? Yes.
our_dt[, c(1, 2), with = F]

# Define the object
my_cols <- c("a", "b")
# Access the columns using the object
our_dt[, my_cols, with = F]

# Indexing rows ----
# Grab some rows
our_dt[c(1, 5, 7, 10), ]

# Grab b < 0 and c > 0.5
our_dt[(b < 0) & (c > 0.5),]

# Grab b in [-0.25,0.25] and c > 0.5
our_dt[between(b, -0.25, 0.25) & (c > 0.5), .(a, c)]

# Set the seed
set.seed(12345)
# Sample
our_dt[sample(10),]

# Adding/manipulating columns ----
# Define a column of ones
our_dt[, ones := 1]
# Check our data frame
our_dt

# Define abc
our_dt[, abc := a * b * c]
# Check our data frame
our_dt

# Re-define the column
our_dt[, ones := 111]
# Check our data frame
our_dt

# Delete 'ones'
our_dt[, ones := NULL]
our_dt

our_dt[, `:=`(
  abc = NULL,
  ab = a * b,
  ac = a *c
  )]
our_dt

# Add column of ones using 'set'
set(x = our_dt, j = "ones", value = 1)
# Delete column of ones using 'set'
set(x = our_dt, j = "ones", value = NULL)

# Setting names ----
# Change names to uppercase
setnames(our_dt, toupper(names(our_dt)))
# Change 'AB' to 'ab'
setnames(our_dt, old = "AB", new = "ab")
# Check our data table
our_dt
# Change names back to lowercase
setnames(our_dt, tolower(names(our_dt)))

# Summarizing columns ----
# Mean of 'a' and 'b'
our_dt[, .(mean(a), mean(b))]

# Mean of 'a' and 'b', named
our_dt[, .(mean_a = mean(a), mean_b = mean(b))]

# Mean of 'a' and 'b' grouped by 'd'
our_dt[, .(mean_a = mean(a), mean_b = mean(b)), by = d]

# Last observation
our_dt[.N,]

our_dt[, .N, by = d]

# Add means of 'a', grouped by 'd'
our_dt[, mean_a_by_d := mean(a), by = d]

# Count the unique values of 'd'
our_dt[,d] %>% uniqueN()

# Ordering ----
# Order by 'd' and then reversed 'a'
setorder(our_dt, d, -a)
# Check work
our_dt

# Change column order
setcolorder(our_dt,
  c("a", "ab", "ac", "mean_a_by_d", "b", "c", "d"))
# Check work
our_dt
