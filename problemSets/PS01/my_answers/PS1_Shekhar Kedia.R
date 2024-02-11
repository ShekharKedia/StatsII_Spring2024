######    Applied Statistical Analysis II   ######     
######    Problem Set 1                     ######
######    Shekhar Kedia - 23351315          ######

## Preparing the environment ##

# Getting working directory
getwd()

# Setting working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Clearing global environment and removing objects
rm(list=ls())

# Detaching all library packages and loading relevant package(s)
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

# Generating 1000 cauchy random variables
set.seed(123)
n <- 1000
empirical <- rcauchy(n, location = 0, scale = 1)

?ecdf() #Trying to understand more about the function

# Creating function to perform the K-S test
k_s_test <- function (data){
  # creating empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  # generating test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  cat("D =", D)  
  temp <- c() #Empty vector to store the summation result after each iteration
  for(i in 1:n){
    temp <- c(temp, exp((-(2 * i - 1)^2 * pi^2) / ((8 * D)^2)))
  }
  p_value <- sqrt(2 * pi)/D * sum(temp)
  cat("\np-value =", p_value)
}

# Running the K-S test function
k_s_test(empirical)

# Comparing the output with in-built R-function
ks.test(empirical, pnorm)


#####################
# Problem 2
#####################

# Creating the data
set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Defining the function to calculate the sum of squared residuals
residuals_ols <- function(coef) {
  sum((data$y - (coef[1] + coef[2] * data$x))^2)
}

# Estimating coefficients using BFGS algorithm
bfgs_ols <- optim(par = c(intercept = 0, slope = 0), fn = residuals_ols, method = "BFGS")
bfgs_ols$par #Displaying the intercept and slope

# Plotting the graph using output from BFGS algorithm
pdf("plot_Y_X_bfgs.pdf")
plot(data$x, data$y, ylab = 'Y', xlab = 'X')
abline(bfgs_ols$par, col = "blue")
dev.off()

# Confirming the same result with lm()
lm_coef <- coef(lm(y ~ x, data = data))
lm_coef #Displaying the intercept and slope

# Plotting the graph using output from lm()
pdf("plot_Y_X_lm.pdf")
plot(data$x, data$y, ylab = 'Y', xlab = 'X')
abline(coef(lm(y ~ x, data = data)), col = "red")
dev.off()