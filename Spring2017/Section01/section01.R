
# Packages in R ----------------------------------------------------------------
# Install packages
install.packages("dplyr")
install.packages(c("haven", "readr"))
# Load the packages
library(dplyr)
library(haven)
library(readr)

# File paths -------------------------------------------------------------------
# Check the current working directory
getwd()
# Set the working directory
setwd("/Users/edwardarubin/Dropbox/Teaching/ARE212/Section01/")
# Check the working directory
getwd()
# Store a directory
dir_class <- "/Users/edwardarubin/Dropbox/Teaching/ARE212/"
# Add a subfolder to the directory
dir_section1 <- paste0(dir_class, "Section01/")
# Look at the files in the current directory
dir()
# Should be the same as
dir(getwd())
# Look at the files in the directory stored in dir_class
dir(dir_class)

# Examples with paste functions ------------------------------------------------
# Default use of paste0()
paste0(1, 2, 3)
# Default use of paste()
paste(1, 2, 3)
# Setting the separation parameter to " " (the default)
paste(1, 2, 3, sep = " ")
# Changing the separation parameter to "+"
paste(1, 2, 3, sep = "+")

# Load data files --------------------------------------------------------------
# Load the .dta file
car_data <- read_dta(paste0(dir_section1, "auto.dta"))
# Print the saved dataset in the console
car_data
# Load the .csv file
car_data <- read_csv(paste0(dir_section1, "auto.csv"))
# Print the saved dataset in the console
car_data

# Examining the data -----------------------------------------------------------
# Names of the variables
names(car_data)
# Head of the dataset (first six lines)
head(car_data)
# First 11 lines
head(car_data, n = 11)
# Last 7 lines
tail(car_data, n = 7)
# View the data in RStudio viewer
View(car_data)
# Summarize all variables
summary(car_data)
# Summarize price variable
summary(car_data$price)

# select-ing -------------------------------------------------------------------
# Select our desired variables; define as car_sub
car_sub <- select(car_data, price, mpg, weight, length)
# Print the dataset
car_sub

# arrange-ing ------------------------------------------------------------------
# Arrange by price and mpg (both ascending)
arrange(car_sub, price, mpg)
# Arrange by price (descending) and mpg (ascending)
arrange(car_sub, desc(price), mpg)

# summarize-ing ----------------------------------------------------------------
# Using summarize() to find the mean and standard deviation of price
summarize(car_sub, mean(price), sd(price))
# Same thing but naming the outputs
summarize(car_sub, price_mean = mean(price), price_sd = sd(price))
# Mean and standard deviation somewhat manually
mean(car_sub$price)
sd(car_sub$price)

# Plotting the data ------------------------------------------------------------
# A simple histogram of mpg
hist(car_sub$mpg)
# A prettier histogram of mpg
hist(
  # The variable for the histogram
  x = car_sub$mpg,
  # The main title
  main = "Distribution of fuel economy",
  # The x-axis label
  xlab = "MPG (miles per gallon)")
# The blue vertical line at the median MPG (lwd is line width)
abline(v = median(car_sub$mpg), col = "blue", lwd = 3)
# Scatterplot of mpg and price
plot(
  x = car_sub$mpg,
  y = car_sub$price,
  xlab = "Fuel economy (MPG)",
  ylab = "Price")
