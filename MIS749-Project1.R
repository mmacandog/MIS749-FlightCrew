install.packages("dplyr")
install.packages("caret")
install.packages("fastDummies")
install.packages("corrplot")
library(pROC)
require(dplyr)
require(caret)
require(fastDummies)

test <- read.csv("~/BA749/test.csv")
train <- read.csv("~/BA749/train.csv")

# view column names
colnames(test)
colnames(train)

# combine the datasets together using union
df <- union_all(test, train)

# view data types of df
str(df)

### pre-processing
# create dummy variables for all categorical/nominal data and remove first dummy variables (to avoid multicollinearity!)
df <- dummy_cols(df, remove_selected_columns = TRUE, remove_first_dummy = TRUE)
head(df)

# identify near zero variance predictors
nzv <- nearZeroVar(df, saveMetrics = TRUE, names = TRUE) # departure delay and arrival delay 
dim(df)
# remove near zero variance predictors
nzv <- nearZeroVar(df)
df <- df[,-nzv]
dim(df)

# identify highly correlated predictors
install.packages("corrgram")
library(corrgram)
descCor <- cor(df)
corrgram(df, lower.panel=panel.shade,
         upper.panel=panel.pie)
library(corrplot)
corr_df <- cor(df)
corrplot(corr_df, method = "color", type = "upper", tl.srt=45, tl.col="black")
# calculate significant correlations with cutoff = .7
hi_corr <- findCorrelation(corr_df, cutoff = .7)
hi_corr
# remove predictor
df <- df[,-hi_corr]


