install.packages("dplyr")
require(dplyr)

test <- read.csv("~/BA749/test.csv")
train <- read.csv("~/BA749/train.csv")

# view column names
colnames(test)
colnames(train)

# combine the datasets together using union
df <- union_all(test, train)


