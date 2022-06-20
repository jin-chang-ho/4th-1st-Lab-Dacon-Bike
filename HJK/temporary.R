setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
library(MASS)
library(e1071)
library(cvTools)
library(forecast)
library(caret)
library(lubridate)

NMAE <- function(actual, predicted) {
  return(mean(abs(actual - predicted)/actual))
}

df <- read.csv("real_data.csv")
str(df)
df$date <- as.Date(df$date)
df[,14:19] <- lapply(df[,14:19], as.factor)

set.seed(100)
train_idx <- sample(1:nrow(df), size=3*round(nrow(df)/4), replace=F)
train <- df[train_idx,]
test <- df[-train_idx, -13]
test.label <- df[-train_idx, 13]

type.name = c('eps-regression', 'nu-regression')
kernel.name = c('linear', 'polynomial','radial','sigmoid')

for(i in 1:2) {
  for(j in 1:4) {
    model <- svm(rental ~ ., data = train, type = type.name[i], kernel = kernel.name[j])
    result <- predict(model, test)
    result <- round(result)
    cat(type.name[i], '\t-\t', kernel.name[j], '\t:\t', NMAE(test.label, result), '\n')
  }
}

train <- df[train_idx, -2]
test <- df[-train_idx, -c(2,13)]
test.label <- df[-train_idx, 13]

for(i in 1:2) {
  for(j in 1:4) {
    model <- svm(rental ~ ., data = train, type = type.name[i], kernel = kernel.name[j])
    result <- predict(model, test)
    result <- round(result)
    cat(type.name[i], '\t-\t', kernel.name[j], '\t:\t', NMAE(test.label, result), '\n')
  }
}

test <- read.csv('test_filled.csv')
train <- df[, -2]
summary(test)
test <- test[, -2]
test$date <- as.Date(test$date)
test[,12:17] <- lapply(test[,12:17], as.factor)


str(test)
model <- svm(rental~., data=train, type='nu-regression', kernel='radial')
pred <- round(predict(model, test))
pred
write.csv(test.raw, 'test_filled.csv', row.names=F)

submit <- read.csv("sample_submission.csv")
submit$rental <- pred
str(submit)
submit$rental <- pred
write.csv(submit, 'sample_submission2.csv', row.names=F)


kfold <- function(classifier, ds, cl, fold) {
  acc <- c() #fold별acc 저장
  for (i in 1:5) {
    ts.idx<-fold$subset[which(fold$which==i)]
    ds.train <- ds[-ts.idx, ]
    ds.test <-  ds[ts.idx, ]
    cl.train <- cl[-ts.idx]
    cl.test <- cl[ts.idx]
    
    if (classifier == 'svm') {
      model <- svm(rental~., data=ds.train, type='eps-regression', kernel='radial')
      pred <- predict(model, ds.test[, -12])
      acc[i] <- NMAE(ds.test$rental,pred)
    }
    if (classifier == 'rf') {
      model <- svm(rental~., data=ds.train, type='eps-regression', kernel='radial')
      pred <- round(predict(model, ds.test),0)
      acc[i] <- NMAE(ds.test$rental,pred)
    }
    
  }
  return(mean(acc))
  
}
ds[, 12:17] <- lapply(ds[, 12:17], as.numeric)
str(ds)
ds <- train
cl <- train$rental

set.seed(100)
fold <- cvFolds(nrow(ds), K=5) #fold 생성
result <- kfold('svm',ds,cl,fold)
result

