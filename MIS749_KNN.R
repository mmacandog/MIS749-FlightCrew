library(ISLR)
library(gam)
library(visreg)

install.packages(c("gam","visreg"))

data <- read.csv("~/mis749_cleaned.csv")
data$X <- NULL
data$satisfaction_satisfied <- factor(data$satisfaction_satisfied)
levels(data$satisfaction_satisfied) <- c('satisfied', 'unsatisfied')

# Create training and test data set
set.seed(199)
trainIndex <- createDataPartition(data$satisfaction_satisfied, p=.7, list=F)

d.train <- data[trainIndex,]
d.test <- data[-trainIndex,]

#setup control function for resampling and binary classification performance
#using 10 fold cross validation
ctrl <- trainControl(method = "cv", number=10, summaryFunction=twoClassSummary,
                     classProbs=T, savePredictions=T) #saving predictions from each resample fold

##logistic regression
set.seed(199) #ALWAYS USE same SEED ACROSS trains to ensure identical cv folds
d.log <-  train(satisfaction_satisfied ~ ., data=data, method="glm", family="binomial", metric="ROC", trControl=ctrl)
summary(d.log)
varImp(d.log)
d.log

#calculate resampled accuracy/confusion matrix using extracted predictions from resampling
confusionMatrix(d.log$pred$pred, d.log$pred$obs) #take averages


# KNN Classifier
set.seed(199) 
d.knn <-  train(satisfaction_satisfied ~ ., data=data, method="knn", metric="ROC", trControl=ctrl, tuneLength=10) #let caret decide 10 best parameters to search
 d.knn
plot(d.knn)
getTrainPerf(d.knn)

confusionMatrix(d.knn$pred$pred, d.knn$pred$obs) #make sure to select resamples only for optimal parameter of K
