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
df$X <- NULL
df$id <- NULL

# view total number of columns
ncol(df)
colnames(df)

# remove rows with 0 in "inflight wifi service" variable
df <- df[df$Inflight.wifi.service != 0, ]

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


# remove near zero variance predictors (arrival delay and departure delay)
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
hi_corr # no significant correlations

# linear dependencies
findLinearCombos(df) # none

# scale predictors to normalize
#df[16:21] <- as.factor(df[16:21])
df[1:21]<- preProcess(df[1:21], method=c("center", "scale"))
df[1:21] <- predict(df_proc_values, df[1:21])
df
summary(df)


#PCA
df_PCA <- df[1:15]
df.out <- prcomp(df_PCA)
names(df.out)
df.out$center
df.out$scale
df.out$rotation # PCA loadings
df.var <- df.out$sdev^2
df.var
pve <- df.var/sum(df.var)
pve
cumsum(pve) # cumulative variance

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained",
     ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Proportion of Variance Explained",
     ylim=c(0,1), type="b")
summary(df.out)


write.csv(df, file="mis749_cleaned.csv", row.names=TRUE)




