library(ISLR)
library(gam)
library(visreg)
library(dplyr)
library(caret)
library(pROC)

install.packages(c("gam","visreg"))

data <- read.csv("~/mis749_cleaned.csv")

### Sample only 15k ###
set.seed(199)
data <- sample_n(data, 15000)


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
ctrl <- trainControl(method = "cv", number=5, summaryFunction=twoClassSummary,
                     classProbs=T, savePredictions=T, allowParallel = T) #saving predictions from each resample fold

##logistic regression
set.seed(199) #ALWAYS USE same SEED ACROSS trains to ensure identical cv folds
d.log <-  train(satisfaction_satisfied ~ ., data=data, method="glm", family="binomial", metric="ROC", trControl=ctrl)
summary(d.log)
varImp(d.log)
d.log

#calculate resampled accuracy/confusion matrix using extracted predictions from resampling
confusionMatrix(d.log$pred$pred, d.log$pred$obs) #take averages


####################
##linear discriminant analysis
set.seed(199)
d.lda <-  train(satisfaction_satisfied ~ ., data=data, method="lda", metric="ROC", trControl=ctrl)
d.lda
varImp(d.lda)
confusionMatrix(d.lda$pred$pred, d.lda$pred$obs) #take averages

##quadratic distriminant analysis
set.seed(199)
d.qda <-  train(satisfaction_satisfied ~ ., data=data, method="qda", metric="ROC", trControl=ctrl)
d.qda
getTrainPerf(d.qda)

#k nearest neighbors classification
set.seed(199) 
d.knn <-  train(satisfaction_satisfied ~ ., data=data, method="knn", metric="ROC", trControl=ctrl, tuneLength=10) #let caret decide 10 best parameters to search
d.knn
plot(d.knn)
getTrainPerf(d.knn)

confusionMatrix(d.knn$pred$pred, d.knn$pred$obs) #make sure to select resamples only for optimal parameter of K

#really need test set to get more accurate idea of accuracy when their is a rare class
#can either use model on cross validation of complete training data or hold out test set

#lets compare all resampling approaches
# d.models <- list("logit"=d.log, "lda"=d.lda, "qda"=d.qda,"knn"=d.knn)
d.models <- list("logit"=d.log, "lda"=d.lda, "qda"=d.qda)
d.resamples = resamples(d.models)


#plot performance comparisons
bwplot(d.resamples, metric="ROC") 
bwplot(d.resamples, metric="Sens") #predicting default dependant on threshold
bwplot(d.resamples, metric="Spec") 

#calculate ROC curves on resampled data

d.log.roc<- roc(response=d.log$pred$obs, predictor=d.log$pred$satisfied)
d.lda.roc<- roc(response= d.lda$pred$obs, predictor=d.lda$pred$satisfied)
d.qda.roc<- roc(response= d.qda$pred$obs, predictor=d.qda$pred$satisfied)
#when model has parameters make sure to select final parameter value
d.knn.roc<- roc(response= d.knn$pred[d.knn$pred$k==23,]$obs, predictor=d.knn$pred[d.knn$pred$k==23,]$satisfied) 

#build to combined ROC plot with resampled ROC curves
plot(d.log.roc, legacy.axes=T)
plot(d.lda.roc, add=T, col="Blue")
plot(d.qda.roc, add=T, col="Green")
plot(d.knn.roc, add=T, col="Red")
legend(x=.2, y=.7, legend=c("Logit", "LDA", "QDA", "KNN"), col=c("black","blue","green","red"),lty=1)



