#### Shekhar Kedia ####
#### Replication ####
#### Applied Statistics - II ####

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

lapply(c('tidyverse', 'dplyr', 'scales', 'modelsummary', 'srvyr', 'nnet', 'MASS',
         'AER', 'pscl', 'margins', 'haven', 'xtable', 'ggplot2'),  pkgTest)


# Loading the STATA file
data <- read_dta("HMS BJPS replication data.dta")

# Viewing the structure of the loaded data
str(data)



## Descriptive statistics ##
# Oct '12 - Jan '13
subset_data_w6 <- subset(data, wave == 6 & dep_d < Inf & !is.na(pre_party1))

options(scipen=999)
desc_w6 <- subset_data_w6 %>%
  transmute(`Education (BA or more)` = ppeducat_4,
            `Female` = female6,
            `Black'12` = black6,
            `Hispanic'12` = hisp6,
            `White'12` = white6,
            `Union'07` = union0,
            `Republican'07` = pre_party1,
            `Age` = AGE,
            `Income` = INCOME)

datasummary(`Education (BA or more)` 
            + `Female` 
            + `Black'12` 
            + `Hispanic'12` 
            + `White'12` 
            + `Union'07` 
            + `Republican'07` 
            + `Age` 
            + `Income`
            ~ (`Unique (#)` = NUnique)
            + Min * Arguments(fmt = "%.0f")
            + Max * Arguments(fmt = "%.0f")
            + (Median = median)
            + (Mean = mean)
            + (SD = sd),
            fmt = 4,
            data = desc_w6,
            title = "Descriptive statistics (Oct '12 - Jan '13)",
            output = "descriptives_w6.docx")

# Oct '20
subset_data_w15 <- subset(data, wave == 15 & dep_d < Inf & !is.na(pre_party1))

options(scipen=999)
desc_w15 <- subset_data_w15 %>%
  transmute(`Education (BA or more)` = ppeducat_4,
            `Female` = female6,
            `Black'12` = black6,
            `Hispanic'12` = hisp6,
            `White'12` = white6,
            `Union'07` = union0,
            `Republican'07` = pre_party1,
            `Age` = AGE,
            `Income` = INCOME)

datasummary(`Education (BA or more)` 
            + `Female` 
            + `Black'12` 
            + `Hispanic'12` 
            + `White'12` 
            + `Union'07` 
            + `Republican'07` 
            + `Age` 
            + `Income`
            ~ (`Unique (#)` = NUnique)
            + Min * Arguments(fmt = "%.0f")
            + Max * Arguments(fmt = "%.0f")
            + (Median = median)
            + (Mean = mean)
            + (SD = sd),
            fmt = 4,
            data = desc_w15,
            title = "Descriptive statistics (Oct '20)",
            output = "descriptives_w15.docx")

## Main results ##

# Table 1 (author)
models <- list(
  "(1)" = lm(dep_d ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + union0 + white6 + male + AGE + pre_party1 + INCOME + factor(state),
            data = data,
            subset = (wave == 7)),
  "(2)" = lm(dep_d ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + union0 + white6 + male + AGE + pre_party1 + INCOME + factor(state),
            data = data,
            subset = (wave == 8)),
  "(3)" = lm(dep_d ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + union0 + white6 + male + AGE + pre_party1 + INCOME + factor(state),
             data = data,
             subset = (wave == 10)),
  "(4)" = lm(dep_d ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + union0 + white6 + male + AGE + pre_party1 + INCOME + factor(state),
             data = data,
             subset = (wave == 11)),
  "(5)" = lm(dep_d ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + union0 + white6 + male + AGE + pre_party1 + INCOME + factor(state),
             data = data,
             subset = (wave == 13)),
  "(6)" = lm(dep_d ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + union0 + white6 + male + AGE + pre_party1 + INCOME + factor(state),
             data = data,
             subset = (wave == 14)),
  "(7)" = lm(dep_d ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + union0 + white6 + male + AGE + pre_party1 + INCOME + factor(state),
             data = data,
             subset = (wave == 15)),
  "(8)" = lm(dep_d ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + union0 + white6 + male + AGE + pre_party1 + INCOME + factor(state),
             data = data),
  "(9)" = lm(dep_d ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + union0 + white6 + male + AGE + pre_party1 + INCOME + factor(Year) + mno + factor(state),
             data = data,
             subset = (samp1011 == 1)))

gm <- modelsummary::gof_map
gm$clean[gm$clean == 'R2 Adj.'] <- 'Adjusted R-squared'
gm$clean[gm$clean == 'Num.Obs.'] <- 'Observations'

modelsummary(models,
             vcov = "HC2",
             fmt = "%.4f",
             stars = TRUE,
             gof_map = gm,
             gof_omit = "AIC|BIC|Log.Lik.|F|Deviance|Sigma|Statistics|p|R2|Std. Errors",
             coef_map = c("UNEMPLOYED"="Respondent is unemployed"),
             title = "Contemporaneous unemployment and support for deportation of unauthorized migrants",
             output = "table1.docx")

# Replication
# Using the 7-level dependent variable as ordered outcome and setting "1" as the reference
data$path <- relevel(factor(data$path), ref="1")
levels(data$path) #Ensuring the levels are in order: 1, 2, 3, 4, 5, 6, 7

# Running the ordered multinomial model
multinom_or <- polr(path ~ UNEMPLOYED + RETIRED + DISABLED + OTHER_EMP + ppeducat + union0 + white6 + male + AGE + pre_party1 + INCOME + Year + state,
                    data = data, Hess = T)

summary(multinom_or) #Producing the output

# Calculating the p−value
ctable <- coef(summary(multinom_or))
p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, digits = 4)
(ctable <- cbind(ctable, "p value" = p))


# Table 2 (author)
models_2 <- list(
  "(1)" = lm(dep_d ~ LOST_JOB, 
             data = data,  subset = (samp == 1)),
  "(2)" = lm(dep_d ~ LOST_JOB + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + logINCOME + state, 
             data = data,  subset = (samp == 1)),
  "(3)" = lm(dep_d ~ LOST_JOB + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + logINCOME + Year + state, 
             data = data,  subset = (samp == 1)),
  "(4)" = lm(dep_d ~ LOST_JOB + income_shock2 + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + logINCOME + state, 
             data = data,  subset = (samp == 1)),
  "(5)" = lm(dep_d ~ LOST_JOB + income_shock2 + FOUND_JOB + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + logINCOME + state, 
             data = data,  subset = (samp == 1)))

gm <- modelsummary::gof_map
gm$clean[gm$clean == 'R2 Adj.'] <- 'Adjusted R-squared'
gm$clean[gm$clean == 'Num.Obs.'] <- 'Observations'

modelsummary(models_2,
             vcov = "HC2",
             fmt = "%.4f",
             stars = TRUE,
             gof_map = gm,
             gof_omit = "AIC|BIC|Log.Lik.|F|Deviance|Sigma|Statistics|p|R2|Std. Errors",
             coef_map = c("LOST_JOB"="Lost job",
                          "income_shock2"="Income drop",
                          "FOUND_JOB"="Found job"),
             title = "Effect of economic shocks on voters’ support for the deportation of unauthorized immigrants",
             output = "table2.docx")

# Replication
# Running the ordered multinomial model
multinom_or_2 <- polr(path ~ LOST_JOB + income_shock2 + FOUND_JOB + RETIRED + DISABLED + OTHER_EMP + factor(ppeducat) + logINCOME + state, 
                      data = data,  subset = (samp == 1), Hess = T)

summary(multinom_or_2) #Producing the output

# Calculating the p−value
ctable <- coef(summary(multinom_or_2))
p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, digits = 4)
(ctable <- cbind(ctable, "p value" = p))


# Table 3 (author)
models_3 <- list(
  "(1)" = lm(dep_d ~ LOST_JOB + low_educ +  union0 + white6 + male + copcimm10M + 
               Unemployment_rateM + pre_pid7 + AGE + INCOME + FOUND_JOB + RETIRED + 
               DISABLED + OTHER_EMP + factor(Year) + factor(countyfips), data = data),
  "(2)" = lm(dep_d ~ LOST_JOB*low_educ + union0 + white6 + male + copcimm10M + 
               Unemployment_rateM + pre_pid7 + AGE + INCOME + FOUND_JOB + RETIRED + 
               DISABLED + OTHER_EMP + factor(Year) + factor(countyfips), data = data),
  "(3)" = lm(dep_d ~ LOST_JOB*union0 + low_educ +  white6 + male + copcimm10M + 
               Unemployment_rateM + pre_pid7 + AGE + INCOME + FOUND_JOB + RETIRED + 
               DISABLED + OTHER_EMP + factor(Year) + factor(countyfips), data = data),
  "(4)" = lm(dep_d ~ LOST_JOB*copcimm10M + low_educ +  union0 + white6 + male + 
               Unemployment_rateM + pre_pid7 + AGE + INCOME + FOUND_JOB + RETIRED + 
               DISABLED + OTHER_EMP + factor(Year) + factor(countyfips), data = data),
  "(5)" = lm(dep_d ~ LOST_JOB*Unemployment_rateM + low_educ +  union0 + white6 + male + copcimm10M + 
                pre_pid7 + AGE + INCOME + FOUND_JOB + RETIRED + 
               DISABLED + OTHER_EMP + factor(Year) + factor(countyfips), data = data),
  "(6)" = lm(dep_d ~ LOST_JOB*white6 + low_educ +  union0 + male + copcimm10M + 
               Unemployment_rateM + pre_pid7 + AGE + INCOME + FOUND_JOB + RETIRED + 
               DISABLED + OTHER_EMP + factor(Year) + factor(countyfips), data = data),
  "(7)" = lm(dep_d ~ LOST_JOB*male + low_educ +  union0 + white6 + copcimm10M + 
               Unemployment_rateM + pre_pid7 + AGE + INCOME + FOUND_JOB + RETIRED + 
               DISABLED + OTHER_EMP + factor(Year) + factor(countyfips), data = data))

gm <- modelsummary::gof_map
gm$clean[gm$clean == 'R2 Adj.'] <- 'Adjusted R-squared'
gm$clean[gm$clean == 'Num.Obs.'] <- 'Observations'

modelsummary(models_3,
             vcov = "HC2",
             fmt = "%.4f",
             stars = TRUE,
             gof_map = gm,
             gof_omit = "AIC|BIC|Log.Lik.|F|Deviance|Sigma|Statistics|p|R2|Std. Errors",
             coef_map = c("LOST_JOB"="Lost job",
                          "LOST_JOB:low_educ"="Lost jobxlow-skilled",
                          "LOST_JOB:union0"="Lost jobxUnion member",
                          "LOST_JOB:copcimm10M"="Lost jobxHigh % of foreign-born",
                          "LOST_JOB:Unemployment_rateM"="Lost jobxHigh unemployment rate",
                          "LOST_JOB:white6"="Lost jobxWhite",
                          "LOST_JOB:male"="Lost jobxMale"),
             title = "Effect heterogeneity by respondent characteristics",
             output = "table3.docx")
