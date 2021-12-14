setwd("~/Desktop/BA-749")

library(rpart) 
library(tree) 
library(rpart.plot)
library(dplyr)
library(caret)

data <- read.csv("mis749_cleaned.csv")

#remove x column
data$X <- NULL

#sample only 15000
set.seed(199)
data <- sample_n(data, 15000)

#classification tree
data.ctree <- rpart(satisfaction_satisfied~., data = data, method = 'class')
data.ctree
plot(data.ctree)
text(data.ctree, pretty=0)

#try rpqrt.plot for better visuals
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

#random forest
library(randomForest)
set.seed(199)
data$satisfaction_satisfied= as.factor(data$satisfaction_satisfied)
data.forest <- randomForest(satisfaction_satisfied ~ ., data=data, mtry=5, ntree=500, type= 'class')
data.forest

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

#visualize the importance of the predictor variables by calculating the total reduction in RSS 
#the larger the value, the more important the predictor
VI <- data.frame(var=names(data[,-1]), imp=varImp(data.bag))

#sort variable importance descending
VI_plot <- VI[order(VI$Overall, decreasing=TRUE),]

#visualize variable importance with horizontal bar plot
barplot(VI_plot$Overall,
        names.arg=rownames(VI_plot),
        horiz=TRUE,
        col='steelblue',
        xlab='Variable Importance')
        
