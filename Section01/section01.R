
# Packages in R ----------------------------------------------------------------
# Install the package named "dplyr"
install.packages("dplyr")
# Install the packages named "haven" and "readr"
install.packages(c("haven", "readr"))
# Loading packages
library(dplyr)
library(haven)
library(readr)

# Package management with 'pacman' ---------------------------------------------
# Install the 'pacman' package
install.packages("pacman")
# Load the 'pacman' package
library(pacman)
# p_load examples
p_load(dplyr, haven, readr)
p_load(ggplot2)

# File directories -------------------------------------------------------------
# File paths and navigation
setwd("/Users/edwardarubin/Dropbox/Teaching/ARE212")
getwd()
# The current directory
getwd()
# Move up one level
setwd("..")
# Check the directory now
getwd()
# Go home
setwd("~")
# Check the new working directory
getwd()
# Now return to my ARE212 folder
setwd("Dropbox/Teaching/ARE212")
# Check the working directory
getwd()
# The path to my ARE 212 folder (ARE212)
dir_class <- "/Users/edwardarubin/Dropbox/Teaching/ARE212/"
# The path to my section 1 folder (Section01), which is inside my ARE 212 folder
dir_section1 <- paste0(dir_class, "Section01/")
# Default use of paste0()
paste0(1, 2, 3)
# Default use of paste()
paste(1, 2, 3)
# Setting the separation parameter to " " (the default)
paste(1, 2, 3, sep = " ")
# Changing the separation parameter to "+"
paste(1, 2, 3, sep = "+")
# The object 'dir_class'
dir_class
# The character vector
"Section01/"
# Paste them together
paste0(dir_class, "Section01/")
# Look inside my ARE212 folder (dir_class stores the path)
dir(dir_class)
# Look inside my section 1 folder (dir_section1 stores the path)
dir(dir_section1)
dir_section1
# The object
dir(dir_section1)
# The object's value
dir("/Users/edwardarubin/Dropbox/Teaching/ARE212/Section01/")

# Loading files ----------------------------------------------------------------
# Load the .dta file
car_data <- read_dta(paste0(dir_section1, "auto.dta"))
# Check the loaded data
car_data
# Load the .csv file
car_data <- read_csv(paste0(dir_section1, "auto.csv"))
# See that it looks the same as above
car_data
# Load the CSV
read_csv("Section01/auto.csv")

# Playing with data ------------------------------------------------------------
# Print the data into the console again
car_data
# Variable names
names(car_data)
# First six observations
head(car_data)
# First eleven observations
head(car_data, n = 11)
# Last seven observations
tail(car_data, n = 7)

# Summarizing the data ---------------------------------------------------------
# The 'summary' function
summary(car_data)
# Summarize only price
summary(car_data$price)
# Select our desired variables; define as car_sub
car_sub <- select(car_data, price, mpg, weight, length)
# Print the dataset
car_sub
select(car_data, -price, -mpg, -weight, -length)
# Arrange by price and mpg
arrange(car_sub, price, mpg)
car_sub
# Revese the price ordering
arrange(car_sub, desc(price), mpg)
# The 'summarize' function
summarize(car_sub, mean(price), sd(price))
summarize(car_sub, price_mean = mean(price), price_sd = sd(price))
# Mean and standard deviation functions
mean(car_sub$price)
sd(car_sub$price)

# Plotting the data ------------------------------------------------------------
# Histogram
hist(car_sub$mpg)
# The histogram function
hist(
  # The variable for the histogram
  x = car_sub$mpg,
  # The main title
  main = "Distribution of fuel economy",
  # The x-axis label
  xlab = "MPG (miles per gallon)")
# The blue vertical line at the median MPG (lwd is line width)
abline(v = median(car_sub$mpg), col = "blue", lwd = 3)
# Scatter plot
plot(
  x = car_sub$mpg,
  y = car_sub$price,
  xlab = "Fuel economy (MPG)",
  ylab = "Price")

# Indexing ---------------------------------------------------------------------
# Create a vector
x <- c(3, 5, 7, 9)
# Grab the second element of x
x[2]
# Grab the second and third elements of x
x[c(2, 3)]
# Grab the second and third elements of x
x[2:3]
# See what 2:3 does
2:3
# Indexing the car subset
car_sub[1, ]
car_sub[, 1]
# Index using the name of a column as its index
car_sub[, "price"]
