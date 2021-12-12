setwd("~/Desktop/BA-749")

library(rpart) 
library(tree) 
library(rpart.plot)
library(dplyr)
library(caret)

data <- read.csv("mis749_cleaned2.csv")

#remove missing data will leave factors alone (trees can handle them)
data$X <- NULL

### Sample only 15k ###
set.seed(199)
data <- sample_n(data, 15000)

#classification tree
data.ctree <- rpart(satisfaction_satisfied~., data = data, method = 'class')
data.ctree
plot(data.ctree)
text(data.ctree, pretty=0)

#try rpqrt.plot for better visual
rpart.plot(data.ctree)

#display and plot cross-validated error for each tree size
printcp(data.ctree) 
plotcp(data.ctree)

#select CP with lowest cross-validated error 
#we can grab this from the plotcp table automatically with 
opt.cp <- data.ctree$cptable[which.min(data.ctree$cptable[,"xerror"]),"CP"]

#prune tree
rpart.plot(prune(data.ctree,cp=0.012))

#make prediction
predict_data <- predict(data.ctree, data, type = 'class')
predict_data

#confusion matrix
table_data <- table(data$satisfaction_satisfied, predict_data)
confusionMatrix(table_data)

#bagging
library(ipred)
data$satisfaction_satisfied= as.factor(data$satisfaction_satisfied)
data.bag <- bagging(satisfaction_satisfied ~ ., data=data, coob=TRUE)
data.bag

#make prediction
predict_bag <- predict(data.bag, data, type = 'class')
predict_bag

#confusion matrix
table_bag <- table(data$satisfaction_satisfied, predict_bag)
confusionMatrix(table_bag)

#random forest
library(randomForest)
set.seed(199)
data$satisfaction_satisfied= as.factor(data$satisfaction_satisfied)
data.forest <- randomForest(satisfaction_satisfied ~ ., data=data, mtry=5, ntree=500, type= 'class')
data.forest

#make prediction
predict_forest <- predict(data.forest, data, type = 'class')
predict_forest

#confusion matrix
table_forest <- table(data$satisfaction_satisfied, predict_forest)
confusionMatrix(table_forest)

#lets try BOOSTING, creating sequential trees based on residuals
library(gbm)
set.seed(199)

#using default parameters of tree depth, tree size, and slowness lambda
data$satisfaction_satisfied = as.numeric(data$satisfaction_satisfied)
data.boost <- gbm(satisfaction_satisfied ~ ., data=data)
data.boost


#lets use caret to parameter tune and find test performance estimation
library(caret)

#lets use caret, lasso regression with polynomials to find optimal degrees
ctrl <- trainControl(method="cv", number=10)

#smooth spline regression
#gamSpline in caret will expand each predictor with smooth spline searching for df value
set.seed(199)
data$satisfaction_satisfied = as.factor(data$satisfaction_satisfied)
gam.train <- train(satisfaction_satisfied ~ ., data=data, 
                   method="gamSpline",tuneLength=10,
                   trControl=ctrl)
gam.train

#decision tree
set.seed(199)
rpart.train <- train(satisfaction_satisfied ~ ., data=data, 
                   method="rpart",tuneLength=10,
                   trControl=ctrl)

rpart.train

#bagging tree
set.seed(199)
bag.train <- train(satisfaction_satisfied ~ ., data=data, 
                     method="treebag",tuneLength=10,
                     trControl=ctrl)
bag.train

#random forest
set.seed(199)
rf.train <- train(satisfaction_satisfied ~ ., data=data, 
                     method="rf",tuneLength=10,
                     trControl=ctrl)
rf.train

#boosting
set.seed(199)
boost.train <- train(satisfaction_satisfied ~ ., data=data, 
                     method="gbm",tuneLength=10,
                     trControl=ctrl)
boost.train
plot(boost.train)

#boosting tree tuning
plot(boost.train)
boost.grid<- expand.grid(n.trees=seq(5,100,by=5), interaction.depth=7,
                         shrinkage=c(.0001,001,.01, .1, .5),
                         n.minobsinnode=c(1,10,20))

set.seed(199)
boost.train.round2 <- train(satisfaction_satisfied ~ ., data=data, 
                     method="gbm",tuneGrid=boost.grid,
                     trControl=ctrl)
boost.train.round2

getTrainPerf(boost.train.round2)

getTrainPerf(boost.train)


#gam model extra tuning
gam.train
plot(gam.train)
gam.grid <- expand.grid(df=seq(1.7,2.2,by=.01))
set.seed(199)
gam.train.round2 <- train(satisfaction_satisfied ~ ., data=data, 
                   method="gamSpline",tuneGrid=gam.grid,
                   trControl=ctrl)
gam.train.round2

getTrainPerf(gam.train.round2)

getTrainPerf(gam.train)

#lets gather the models
#first lets put all trained models in a list object
models<- list("gam" = gam.train, "DT"=rpart.train,
              "BaggingTree"=bag.train, "RF"=rf.train,
              "BoostingTree" = boost.train, 
              "BoostingTree-tuned"=boost.train.round2,
              "gam-tuned"=gam.train.round2)

data.resamples<- resamples(models)
summary(data.resamples)
