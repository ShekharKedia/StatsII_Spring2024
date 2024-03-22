######    Applied Statistical Analysis II   ######     
######    Problem Set 3                     ######
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

# 1.a
# Loading the dataset to the environment
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# Creating levels for factor variable and setting the reference (base) category
gdp_data$GDPWdiff_l <- ifelse(gdp_data$GDPWdiff > 0, "positive",
                ifelse(gdp_data$GDPWdiff < 0, "negative", "no change"))
gdp_data$GDPWdiff_l <- relevel(factor(gdp_data$GDPWdiff_l, ordered=F), 
                            ref="no change")

# Running the unordered multinomial model
multinom_unor <- multinom(GDPWdiff_l ~ REG + OIL, data = gdp_data)
summary(multinom_unor)
stargazer(multinom_unor) #Exporting results to LaTeX

# 1.b
# Setting the order of the factor variable
gdp_data$GDPWdiff_l <- relevel(gdp_data$GDPWdiff_l, ref="negative")
levels(gdp_data$GDPWdiff_l) #Checking the levels are in order: negative, no change, positive

# Running the ordered multinomial model
multinom_or <- polr(GDPWdiff_l ~ REG + OIL, data = gdp_data, Hess = T)
summary(multinom_or)

# Calculating the p-value
ctable <- coef(summary(multinom_or))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))


#####################
# Problem 2
#####################

# 2.a
# Loading the dataset to the environment
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# Running the poisson regression model
mexico_poisson <- glm(PAN.visits.06 ~ competitive.district + PAN.governor.06 + marginality.06, data = mexico_elections, family = poisson)
summary(mexico_poisson)

# Running the overdispersion test to check over-dispersion
dispersiontest(mexico_poisson)

# 2.c
# Making the prediction
predict(mexico_poisson, newdata = data.frame(competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1), type = "response")
