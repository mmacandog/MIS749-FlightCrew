### Importing and Pre-processing the customer satisfaction data set

setwd("~/Documents/MIS 749 - Business Analytics/Final Project ")

Airline.train <- read.csv("train.csv")
head(Airline.train)
summary(Airline.train)
length(Airline.train)

library(caret)
library(MASS)
library(e1071)
library(psych)
library(corrplot)
library(corrgram)
library(ISLR)

str(Airline.train$Arrival.Delay.in.Minutes)

# remove ID column 


Airline.train$id <- NULL

# convert categorical values to numeric with dummyVars

Airline.dmodel <- dummyVars(~., data = Airline.train, fullRank = T)


# apply the model to the Airline satisfaction data to create the new variables

Airline.d = as.data.frame(predict(Airline.dmodel, Airline.train))


# identify columns with missing values and impute(populate) with the mean

list_na <- colnames(Airline.d)[apply(Airline.d, 2, anyNA)]
list_na


# create mean and replace missing values

mean(Airline.d$Arrival.Delay.in.Minutes)
mean.Airline.d <- mean(Airline.d$Arrival.Delay.in.Minutes, na.rm = TRUE)
print(mean.Airline.d)
Airline.d[is.na(Airline.d$Arrival.Delay.in.Minutes), "Arrival.Delay.in.Minutes"] <- mean.Airline.d
View(Airline.d)


# failed attempt at imputing mean on missing values

library(dplyr)
library(magrittr)
Airline.d[!complete.cases(Airline.d),]
average_missing <- apply(Airline.d[,colnames(Airline.d) %n% list_na], 2, mean, na.rm = TRUE)


# Identify skewed values skewValues<- apply(credit.train, 2, skew)

head(Airline.d)
skewValues <- apply(Airline.d, 2, skew)
skewValues    # Departure and arrival delay in minutes appear to be skewed values
skewSE <- sqrt()

# identify correlated predictors

Airline.cor <- cor(Airline.d[,-25])

corrplot(Airline.d, order = "hclust")

print(Airline.d)

# basic lm for Airline.d

Airline.lm <- lm(satisfactionsatisfied~., data = Airline.d)
Airline.lm
summary(Airline.lm)
pairs.panels(Airline.d)
