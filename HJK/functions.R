NMAE <- function(actual, predicted) {
  return(mean(abs(actual - predicted)/actual))
}

load_df <- function() {
  setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
  df <- read.csv("real_data.csv")
  df$date <- as.Date(df$date)
  return(df)
}

load_test <- function() {
  setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
  test <- read.csv("test_filled.csv")
  test$date <- as.Date(test$date)
  return(test)
}


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
    else if (classifier == 'xgb') {
      acc[i] <- XgBoost(ds.train, ds.test, cl.train, cl.test)
    }
    
  }
  return(mean(acc))
  
}

XgBoost <- function(ds.tr, ds.ts, cl.tr, cl.ts) {
  set.seed(100)
  x_train <- ds.tr %>%
    as.matrix()
  
  y_train <- cl.tr
  
  x_test <- ds.ts %>%
    as.matrix()
  
  y_test <- cl.ts
  
  dtrain <- xgb.DMatrix(x_train, label=y_train)
  
  model <- xgb.train(
    data=dtrain,
    max_depth=8,
    nround=150,
    eta=0.05,
    subsample=0.5,
    colsample_bytree=0.6,
    min_child_weight=1
  )
  
  xgb.importance(feature_names=colnames(x_train), model) %>%
    xgb.plot.importance()
  
  pred <- predict(model, x_test)
  pred <- round(pred)
  
  return(NMAE(y_test, pred))
}

XGB.real_test <- function(ds.tr, ds.ts, cl.tr) {
  x_train <- ds.tr %>%
    as.matrix()
  y_train <- cl.tr
  y_test <- ds.ts %>%
    as.matrix()
  
  dtrain <- xgb.DMatrix(x_train, label=y_train)
  
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
  
  pred <- predict(model, y_test)
  pred <- round(pred)
  
  return(pred)
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

z_normalize <- function(x) {
  return((x-mean(x)) / sd(x))
}

MAE_FUN <- function(actual, predicted) {
  return(mean(abs(actual - predicted)))
}

gen_submitssion_file <- function(pred) {
  setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data/submit_data")
  submit <- read.csv('sample_submission.csv')
  submit$rental <- pred
  data_dir <- c("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data/submit_data/")
  data_list <- list.files(data_dir)
  l <- length(data_list)
  file_name <- paste0('submission',l,'.csv')
  write.csv(submit, file_name, row.names=F)
}