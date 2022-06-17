# 데이터 불러오기
setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
df <- read.csv("new_data.csv")

library(MASS)

NMAE <- function(actual, predicted) {
  return(mean(abs(actual - predicted)/actual))
}


df[,1] <- as.Date(df[,1])

for (i in 1:nrow(df)) {
  if (df[i,'weekday']==1) {
    df[i,'weekday'] = "Weekday"
  }
  else {
    df[i,'weekday'] = "Holiday"
  }
  
  if (df[i,'season']==1) {
    df[i,'season'] = "Spring"
  } else if (df[i,'season']==2) {
    df[i,'season'] = "Summer"
  } else if (df[i,'season']==3) {
    df[i,'season'] = "Fall"
  } else {
    df[i,'season'] = "Winter"
  }
  
}
df[,14] <- factor(df[,14], levels=c("Weekday","Holiday"))
df[,15] <- factor(df[,15], levels=c("Spring","Summer","Fall","Winter"))

# train-test 분리
train_idx <- sample(1:nrow(df), size=round(nrow(df)/4), replace=F)
train <- df[train_idx,]
test <- df[-train_idx,]

# 학습
model <- lm(rental~., data=train)
model2 <- stepAIC(model)

# 학습 결과 확인
summary(model)
summary(model2)

# 예측 및 결과 확인
pred <- predict(model2, test)
pred <- round(abs(pred))
NMAE(test$rental, pred)

# 파일 저장
write.csv(df, file="new_data.csv", row.names=F)



# min-max 정규화 이후
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
str(df.norm)
df.norm <- as.data.frame(lapply(df[,-c(1, 13, 14, 15)], normalize))
df.norm$date <- df[,1]
df.norm$rental <- df[,13]
df.norm$weekday <- df[,14]
df.norm$season <- df[,15]

# train-test 분리
train_idx <- sample(1:nrow(df), size=round(nrow(df.norm)/4), replace=F)
train.norm <- df.norm[train_idx,]
test.norm <- df.norm[-train_idx,]

# 학습
model <- lm(rental~., data=train.norm)
model2 <- stepAIC(model)

# 학습 결과 확인
summary(model)
summary(model2)

# 예측 및 결과 확인
pred <- predict(model2, test.norm)
pred <- round(abs(pred))
NMAE(test$rental, pred)


# z-정규화 이후
str(df.norm)
df.norm <- as.data.frame(scale(df[,-c(1, 13, 14, 15)]))
df.norm$date <- df[,1]
df.norm$rental <- df[,13]
df.norm$weekday <- df[,14]
df.norm$season <- df[,15]

# train-test 분리
train_idx <- sample(1:nrow(df), size=round(nrow(df.norm)/4), replace=F)
train.norm <- df.norm[train_idx,]
test.norm <- df.norm[-train_idx,]

# 학습
model <- lm(rental~., data=train.norm)
model2 <- stepAIC(model)

# 학습 결과 확인
summary(model)
summary(model2)

# 예측 및 결과 확인
pred <- predict(model2, test.norm)
pred <- round(abs(pred))
NMAE(test$rental, pred)


# precipitation 예측해서 넣는 것 (비선형)
# 온도를 섭씨에서 화씨로 변환
