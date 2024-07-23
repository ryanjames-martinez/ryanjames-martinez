# install.packages("boot")
library(boot)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(brant)
library(GGally)
library(gridExtra)
library(rcompanion)
library(ggcorrplot)
library(ordinal)
library(generalhoslem)
library(caret)
library(effects)
library(rsample)
setwd("D:/Ryan/College/MS/1st Year/2nd Sem/STT251/Report/Written Report")

training_data <- read.csv("training_data.csv")
testing_data <- read.csv("testing_data.csv")
training_data$apply <- ifelse(training_data$apply == "unlikely", 1, 
                              ifelse(training_data$apply == "somewhat likely", 2, 3))
training_data$apply <- as.factor(training_data$apply)
training_data$pared <- as.factor(training_data$pared)
training_data$public <- as.factor(training_data$public)


original_model <- polr(apply ~ pared + public + gpa, data = training_data, Hess = TRUE)
remodeled_model <- polr(apply ~ pared + gpa, data = training_data, Hess = TRUE)


boot_stats <- function(data, indices) {
  # Resample the data
  boot_data <- data[indices, ]
  
  original_model <- polr(apply ~ pared + public + gpa, data = boot_data, Hess = TRUE)
  remodeled_model <- polr(apply ~ pared + gpa, data = boot_data, Hess = TRUE)
  
  # Calculate differences in AIC and Residual Deviance
  aic_diff <- AIC(original_model) - AIC(remodeled_model)
  deviance_diff <- original_model$deviance - remodeled_model$deviance
  
  return(c(aic_diff, deviance_diff))
}



# Set the number of bootstrap replicates
n_bootstraps <- 1000  # Adjust based on computational power and desired accuracy

set.seed(2024)
results <- boot(data = training_data, statistic = boot_stats, R = 1000)
results
boot.ci(results, type = c("basic", "perc"), index = 1)  # Confidence intervals for AIC differences
boot.ci(results, type = c("basic", "perc"), index = 2)  # Confidence intervals for Residual Deviance differences

## original bias std. error
## t1* 1.96108366 -1.243752 1.737844
## t2* -0.03891634 -1.243752 1.737844

## Level Basic Percentile
## 95% ( 1.923, 8.295 ) (-4.373, 1.999 )
## Calculations and Intervals on Original Scale

## Intervals :
## Level Basic Percentile
## 95% (-0.0766, 6.2952 ) (-6.3731, -0.0012 )
## Calculations and Intervals on Original Scale

# Bootstrap Confidence Intervals
# boot.ci(results, type = "perc", index = 1)  # For Residual Deviance Difference
# boot.ci(results, type = "perc", index = 2)  # For AIC Difference


# Print results summary
print(results)

plot(results)


