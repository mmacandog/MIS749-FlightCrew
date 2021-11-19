install.packages("dplyr")
install.packages("caret")
install.packages("fastDummies")
install.packages("corrplot")
library(pROC)
require(dplyr)
require(caret)
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


# view total number of columns
ncol(df)

# remove nominal/id predictors
colnames(df)
df <- df[-c(1:2)]
colnames(df)

# view data types of df
str(df)

### pre-processing
# create dummy variables for all categorical/nominal data and remove first dummy variables (to avoid multicollinearity!)
df <- dummy_cols(df, remove_selected_columns = TRUE, remove_first_dummy = TRUE)
head(df)

# identify near zero variance predictors
nzv <- nearZeroVar(df, saveMetrics = TRUE, names = TRUE) # departure delay and arrival delay 
dim(df)
colnames(df)

# remove near zero variance predictors
nzv <- nearZeroVar(df)
df <- df[,-nzv]
dim(df)

# identify highly correlated predictors
install.packages("corrgram")
library(corrgram)
descCor <- cor(df)
#corrgram(df, lower.panel=panel.shade,
#         upper.panel=panel.pie)

library(corrplot)
corr_df <- cor(df)
#corrplot(corr_df, method = "color", type = "upper", tl.srt=45, tl.col="black")


# calculate significant correlations with cutoff = .7
hi_corr <- findCorrelation(corr_df, cutoff = .7, names = TRUE)
hi_corr

# remove  highly correlated predictor (in.flight.wifi)
df <- select(df, -all_of(hi_corr))
colnames(df)

# linear dependencies
findLinearCombos(df) # none

# scale predictors to normalize
#df[16:21] <- as.factor(df[16:21])
df_proc_values <- preProcess(df[,1:15], method=c("center", "scale"))
df_transformed <- predict(df_proc_values, df[,1:15])
df_transformed
summary(df_transformed)

# normalization
df_proc_norm <- preProcess(df, method=c("range"))
print(df_proc_norm)
df_normalized_post <- predict(df_proc_norm, df)
summary(df_normalized_post)
df_normalized_post

## PCA on predictors
# remove response variable
df1 <- df
df1$satisfaction_satisfied <- NULL

#PCA
df.out <- prcomp(df1, scale=TRUE)
names(df.out)
df.out$center
df.out$scale
df.out$rotation # PCA loadings
df.var <- df.out$sdev^2
df.var
pve <- df.var/sum(df.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained",
     ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Proportion of Variance Explained",
     ylim=c(0,1), type="b")
summary(df.out)



