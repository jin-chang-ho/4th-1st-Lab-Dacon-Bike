NMAE <- function(actual, predicted) {
  return(mean(abs(actual - predicted)/actual))
}

load_df <- function() {
  setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
  df <- read.csv("real_data.csv")
  df$date <- as.Date(df$date)
  df[,14:19] <- lapply(df[,14:19], as.factor)
  return(df)
}

load_test <- function() {
  setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
  test <- read.csv("test_filled.csv")
  test$date <- as.Date(test$date)
  test[,13:18] <- lapply(test[,13:18], as.factor)
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
  
  return(NMAE(y_test, pred))
}