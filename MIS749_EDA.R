install.packages("dplyr")
install.packages("caret")
install.packages("fastDummies")
install.packages("corrplot")
install.packages("tidyverse")
library(pROC)
require(dplyr)
require(caret)
library(pROC)
require(dplyr)
require(caret)
require(fastDummies)
require(ggplot2)
require(tidyverse)

data <- read.csv("~/mis749_cleaned.csv")
colnames(data)
data$X <- NULL

# Descriptive Statistics
summary(data)

# Demographic Information
table(data$Age) #
table(data$Gender_Male) # 0 = Female, 1 = Male

# Ratio of satisfaction
dim(data)
rows = dim(data)
rows = rows[1]
unsat_ratio = table(data$satisfaction_satisfied)
unsat_ratio = table[1] 
unsat_ratio = unsat_ratio/rows
sat_ratio = 1 - unsat_ratio 

# Visualizations
age = data$Age
distance = data$Flight.Distance
inflight = data$Inflight.service
gender = data$Gender_Male
ggplot(data, aes(x=age)) + geom_density()
ggplot(data, aes(x=distance)) + geom_density()
d = data

d %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_bar() 







