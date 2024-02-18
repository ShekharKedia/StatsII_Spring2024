######    Applied Statistical Analysis II   ######     
######    Problem Set 2                     ######
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

lapply(c('stargazer'),  pkgTest)

#####################
# Problem 1
#####################

# Loading the data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

# Creating levels for factor variables and setting the reference (base) category
climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, ordered=F), ref="None")
climateSupport$countries <- relevel(factor(climateSupport$countries, ordered=F), ref="20 of 192")

# Logit model (additive)
logit_add <- glm(choice ~ countries + sanctions , data = climateSupport, 
                  family = binomial (link = "logit" ))

summary(logit_add) #Displaying output of the model

logit_null <- glm(choice ~ 1 , data = climateSupport, 
                  family = binomial (link = "logit" ))
anova(logit_null, logit_add, test = "LRT")

#####################
# Problem 2
#####################

# 2(a).
# Changing the reference level (base) to 5%
climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, ordered=F), ref="5%")

# Logit model (additive)
logit_add_new <- glm(choice ~ countries + sanctions , data = climateSupport, 
                 family = binomial (link = "logit" ))

summary(logit_add_new) #Displaying output of the model

# 2(b).
# Predicting probability
predict(logit_add, newdata = data.frame(countries="80 of 192", sanctions="None"), type="response")

# 2(c).
# Creating interactive model
logit_mul <- glm(choice ~ countries * sanctions , data = climateSupport, 
                     family = binomial (link = "logit" ))
summary(logit_mul) #Displaying the model output

# Performing test to compare additive and interactive models
anova(logit_add, logit_mul, test = "LRT")