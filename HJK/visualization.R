library('tidyverse')
library('lubridate')
library('corrplot')
library('forecast')
library('xts')
library('cvTools')
library(e1071)
library('xgboost')
setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
source("../functions.R")
train <- load_df()
test <- load_test()

train.2018 <- train[year(train$date)==2018,]
train.2019 <- train[year(train$date)==2019,]
train.2020 <- train[year(train$date)==2020,]

cor(train.2018[,-1])
cor(train.2019[,-1])
cor(train.2020[, -1])
cor(train[,-1])

df <- train
df$year <- year(df$date)
df$month <- month(df$date)
df$wday <- wday(df$date)
df[,14:19] <- lapply(df[,14:19], as.numeric)
df$day <- day(df$date)

test$year <- year(test$date)
test$month <- month(test$date)
test$wday <- wday(test$date)
test[,13:18] <- lapply(test[,13:18], as.numeric)


temp.Q1.idx <- which(df$temp_highest < quantile(df$temp_highest, 0.25))
temp.Q2.idx <- which(df$temp_highest < quantile(df$temp_highest, 0.5) & df$temp_highest >quantile(df$temp_highest, 0.25))
temp.Q3.idx <- which(df$temp_highest < quantile(df$temp_highest, 0.75) & df$temp_highest >quantile(df$temp_highest, 0.5))
temp.Q4.idx <- which(df$temp_highest < quantile(df$temp_highest, 1) & df$temp_highest >quantile(df$temp_highest, 0.75))

#df$temp_weight <- df[,'temp_highest']
#df[temp.Q2.idx, 'temp_weight'] <- df[temp.Q2.idx, 'temp_weight'] * 2
#df[temp.Q3.idx, 'temp_weight'] <- df[temp.Q3.idx, 'temp_weight'] * 2

prec.norm <- z_normalize(df$precipitation)
temp.norm <- z_normalize(df$temp_highest)
PM.norm <- z_normalize(df$PM2.5)
sun.norm <- z_normalize(df$sunshine_sum)

new.feature <- prec.norm * temp.norm * PM.norm * sun.norm

df$new.feature <- new.feature

tr <- df %>%
  select(-rental, -date)
cl <- df$rental
set.seed(100)
fold <- cvFolds(nrow(tr), K=5)
res <- kfold('xgb',tr, cl, fold)
res

tr <- df %>%
  select(-rental, -date)
cl <- df$rental
ts <- test %>%
  select(-date)
pred <- XGB.real_test(tr, ts, cl)

gen_submitssion_file(pred)

x_train <- df %>%
  select(-rental, -date) %>%
  as.matrix()

y_train <- df$rental

dtrain <- xgb.DMatrix(x_train, label = y_train)

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6),
                                colsample_bytree = c(0.5, 0.6),
                                max_depth = c(7:15),
                                min_child = seq(1),
                                eta = c(0.05,0.1,0.15)
) # 2 * 2* 9 * 1 * 3 * 150 = ?

ntrees <- 150

rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentDepth <- parameterList[["max_depth"]]
  currentEta <- parameterList[["eta"]]
  currentMinChild <- parameterList[["min_child"]]
  
  xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = ntrees, nfold = 5, showsd = TRUE,
                           metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                           "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                           , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                           early_stopping_rounds = 10)
  
  xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
  rmse <- tail(xvalidationScores$test_rmse_mean, 1)
  trmse <- tail(xvalidationScores$train_rmse_mean,1)
  output <- return(c(rmse, trmse,currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))})


output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames
which.min(output$TrainRMSE)
output[106,]
