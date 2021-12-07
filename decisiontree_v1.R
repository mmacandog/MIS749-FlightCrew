setwd("~/Desktop/BA-749")

library(rpart) 
library(tree) 
library(rpart.plot)
library(dplyr)

data <- read.csv("mis749_cleaned2.csv")

### Sample only 15k ###
set.seed(199)
data <- sample_n(data, 15000)


#create a test and train set
create_train_test <- function(data, size = 0.7, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

#test your function and check the dimension
data_train <- create_train_test(data, 0.7, train = TRUE)
data_test <- create_train_test(data, 0.7, train = FALSE)
dim(data_train)

#use function prop.table() combined with table() to verify if the randomization process is correct
prop.table(table(data_train$satisfaction_satisfied))
prop.table(table(data_test$satisfaction_satisfied))

#build classification tree
data.ctree <- rpart(satisfaction_satisfied~., data = data_train, method = 'class')
rpart.plot(data.ctree, extra = 106)

#prune tree
printcp(data.ctree)
rpart.plot(prune(data.ctree,cp=0.016))

#get CP
opt.cp <- data.ctree$cptable[which.min(data.ctree$cptable[,"xerror"]),"CP"] #we get 0.01

#make a prediction
predict(data.ctree, data, type = 'class')
predict_unseen <-predict(data.ctree, data_test, type = 'class')

#create confusion matrix to check accuracy
table_mat <- table(data_test$satisfaction_satisfied, predict_unseen)
table_mat

#measure performance
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


