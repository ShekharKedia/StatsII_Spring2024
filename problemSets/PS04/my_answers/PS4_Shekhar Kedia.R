######    Applied Statistical Analysis II   ######     
######    Problem Set 4                     ######
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

lapply(c('stargazer', 'nnet', 'MASS', 'AER', 'pscl', 'ggplot2'),  pkgTest)

#####################
# Problem 1
#####################

# Loading the dataset in the environment
install.packages("eha")
library(eha)
data(child)
View(child)

# Estimating Cox P-H model
install.packages("survival")
library(survival)

child_mortality <- coxph(Surv(enter, exit, event) ~ m.age +
                         sex, data = child)
summary(child_mortality)

library(stargazer)
stargazer(child_mortality) #Producing the LaTeX output