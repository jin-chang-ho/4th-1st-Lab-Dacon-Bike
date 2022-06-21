library(lubridate)
library(dplyr)
library(xgboost)

# XgBoost를 사용하기 위한 실험
setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
source("../functions.R")
df <- load_df()
test <- load_test()

df$year <- year(df$date)
df$month <- month(df$date)
df$wday <- wday(df$date)
df[,14:19] <- lapply(df[,14:19], as.numeric)

test$year <- year(test$date)
test$month <- month(test$date)
test$wday <- wday(test$date)
test[,13:18] <- lapply(test[,13:18], as.numeric)
str(test)


set.seed(100)
train_set <- df
test_set <- test
x_train <- train_set %>%
  select(-rental, -date) %>%
  as.matrix()

y_train <- train_set$rental

x_test <- test_set %>%
  select(-date) %>%
  as.matrix()

dtrain <- xgb.DMatrix(x_train, label=y_train)

searchGridSubCol <- expand.grid(subsample=c(0.5, 0.6),
                                colsample_bytree=c(0.5, 0.6),
                                max_depth=c(7:15),
                                min_child=seq(1),
                                eta=c(0.05, 0.1, 0.15)
                                )
ntrees <- 10

model <- xgb.train(
  data=dtrain,
  max_depth=10,
  nround=150,
  eta=0.15,
  subsample=0.6,
  colsample_bytree=0.6,
  min_child_weight=1
)

xgb.importance(feature_names=colnames(x_train), model) %>%
  xgb.plot.importance()

pred <- predict(model, x_test)

pred <- round(pred)

submit <- read.csv('sample_submission.csv')
str(submit)
submit$rental <- pred
write.csv(submit, 'sample_submission3.csv', row.names=F)

