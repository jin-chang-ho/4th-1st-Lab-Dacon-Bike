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


kfold <- function(classifier, ds, fold) {
  acc <- c() #fold별acc 저장
  for (i in 1:5) {
    ts.idx<-fold$subset[which(fold$which==i)]
    ds.train <- ds[-ts.idx, ]
    ds.test <-  ds[ts.idx, ]
    
    if (classifier == 'svm') {
      model <- svm(rental~., data=ds.train, type='eps-regression', kernel='radial')
      pred <- predict(model, ds.test[, -12])
      acc[i] <- NMAE(ds.test$rental,pred)
    }
    
  }
  return(mean(acc))
  
}

XgBoost <- function(df, cl, fold=5) {
  set.seed(100)
  train_set <- df
  test_set <- cl
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
  return(pred)
}