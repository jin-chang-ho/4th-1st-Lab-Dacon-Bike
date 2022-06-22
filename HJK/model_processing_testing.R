# 데이터 불러오기
setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
df <- read.csv("new_data.csv")
df.raw <- read.csv("train.csv")
df.complete <- new.df[complete.cases(df.raw),]

df.complete[,1] <- as.Date(df.complete[,1])
df.complete$weekday <- df[complete.cases(df.raw), 14]
df.complete$season <- df[complete.cases(df.raw), 15]
df.complete$weekday <- factor(df.complete[,14], levels=c("Weekday","Holiday"))
df.complete$season <- factor(df.complete[,15], levels=c("Spring","Summer","Fall","Winter"))

df.raw[,1] <- as.Date(df.raw[,1])
df.raw$weekday <- df[, 14]
df.raw$season <- df[, 15]
df.raw$weekday <- factor(df.raw[,14], levels=c("Weekday","Holiday"))
df.raw$season <- factor(df.raw[,15], levels=c("Spring","Summer","Fall","Winter"))
df.raw <- df.raw[!is.na(df.raw$sunshine_sum),] # sunshine_sum의 결측치는 5개 그냥 날림

library(MASS)
library(e1071)
library(cvTools)
library(forecast)
library(caret)
library(lubridate)

MAE_FUN <- function(actual, predicted) {
  return(mean(abs(actual - predicted)))
}

NMAE <- function(actual, predicted) {
  return(mean(abs(actual - predicted)/actual))
}


df[,1] <- as.Date(df[,1])
df[,14] <- factor(df[,14], levels=c("Weekday","Holiday"))
df[,15] <- factor(df[,15], levels=c("Spring","Summer","Fall","Winter"))



dummy <- dummyVars("~.", data=df.raw)
new.df <- data.frame(predict(dummy, newdata=df.raw))
new.df[,14:19] <- lapply(new.df[,14:19], as.factor)
new.df$date <- df.raw$date

train_idx <- sample(1:nrow(df), size=3*round(nrow(df)/4), replace=F)
train <- df[train_idx,]
test <- df[-train_idx, -13]
test.label <- df[-train_idx, 13]

# 내 데이터셋으로 svm 실험
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



# svm으로 PM10 예측
df.complete <- new.df[complete.cases(df.raw),]
ts.idx <- sample(1:nrow(df.complete), size=round(nrow(df.complete)/4), replace=F)
PM10.tr <- df.complete[-ts.idx, -c(2, 7)]
PM10.ts <- df.complete[ts.idx, -c(2, 6, 7)]
PM10.ts.lb <- df.complete[ts.idx, 6]

for(i in 1:2) {
  for(j in 1:4) {
    model <- svm(PM10 ~ ., data = PM10.tr, type = type.name[i], kernel = kernel.name[j])
    result <- predict(model, PM10.ts)
    result <- round(result, 1)
    cat(type.name[i], '\t-\t', kernel.name[j], '\t:\t', MAE(PM10.ts.lb, result), '\n')
  }
}

PM10.model <- svm(PM10~., data=PM10.tr, type='nu-regression', kernel='radial')
PM10.na <- new.df[is.na(new.df$PM10), -c(2, 6, 7)]
new.df[is.na(new.df$PM10), 6] <- round(predict(PM10.model, PM10.na), 1)
summary(new.df)

# svm으로 PM2.5 예측
df.complete <- new.df[complete.cases(new.df),]
ts.idx <- sample(1:nrow(df.complete), size=round(nrow(df.complete)/4), replace=F)
PM2.tr <- df.complete[-ts.idx, -2]
PM2.ts <- df.complete[ts.idx, -c(2, 7)]
PM2.ts.lb <- df.complete[ts.idx, 7]

for(i in 1:2) {
  for(j in 1:4) {
    model <- svm(PM2.5 ~ ., data = PM2.tr, type = type.name[i], kernel = kernel.name[j])
    result <- predict(model, PM2.ts)
    result <- round(result, 1)
    cat(type.name[i], '\t-\t', kernel.name[j], '\t:\t', MAE(PM2.ts.lb, result), '\n')
  }
}

PM2.model <- svm(PM2.5~., data=PM2.tr, type='eps-regression', kernel='linear')
PM2.na <- new.df[is.na(new.df$PM2.5), -c(2, 7)]
new.df[is.na(new.df$PM2.5), 7] <- round(predict(PM2.model, PM2.na), 1)
summary(new.df)

# svm으로 강수량 예측
df.complete <- new.df[complete.cases(new.df),]
ts.idx <- sample(1:nrow(df.complete), size=round(nrow(df.complete)/4), replace=F)
prec.tr <- df.complete[-ts.idx, ]
prec.ts <- df.complete[ts.idx, -2]
prec.ts.lb <- df.complete[ts.idx, 2]

for(i in 1:2) {
  for(j in 1:4) {
    model <- svm(precipitation ~ ., data = prec.tr, type = type.name[i], kernel = kernel.name[j])
    result <- predict(model, prec.ts)
    result <- round(result)
    cat(type.name[i], '\t-\t', kernel.name[j], '\t:\t', MAE(prec.ts.lb, result), '\n')
  }
}

prec.model <- svm(precipitation~., data=prec.tr, type='eps-regression', kernel='polynomial')
prec.na <- new.df[is.na(new.df$precipitation), -2]
new.df[is.na(new.df$precipitation), 2] <- round(predict(prec.model, prec.na), 1)

new.df[new.df$precipitation < 1,2] <- 0
summary(new.df)



# 모델 생성
train_idx <- sample(1:nrow(new.df), size=3*round(nrow(new.df)/4), replace=F)
train <- new.df[train_idx,]
test <- new.df[-train_idx, -13]
test.label <- new.df[-train_idx, 13]


# 내 데이터셋으로 svm 실험
type.name = c('eps-regression', 'nu-regression')
kernel.name = c('linear', 'polynomial','radial','sigmoid')

for(i in 1:2) {
  for(j in 1:4) {
    model <- svm(rental ~ ., data=train, type=type.name[i], kernel=kernel.name[j])
    result <- predict(model, test)
    result <- round(result)
    cat(type.name[i], '\t-\t', kernel.name[j], '\t:\t', NMAE(test.label, result), '\n')
  }
}

# 파일 저장
new.df$date <- df.raw$date
write.csv(new.df, file="real_data.csv", row.names=F)
new.df <- read.csv('real_data.csv')
new.df$date <- as.Date(new.df$date)
new.df[, 14:19] <- lapply(new.df[, 14:19], as.factor)
str(new.df)

# 실제 테스트
test.raw <- read.csv('test.csv')
summary(test.raw)
test.raw[is.na(test.raw$sunshine_sum), 9] <- mean(test.raw$sunshine_sum, na.rm=T)

test.raw[,1] <- as.Date(test.raw[,1])
test.raw$weekday <- weekdays(test.raw$date)
test.raw$weekday <- factor(test.raw$weekday %in% c('토요일','일요일'), labels=c('Weekday','Holiday'))

test.raw$season <- month(test.raw$date)
test.raw$season <- ifelse(test.raw$season %in% c(3, 4, 5), "Spring",
                    ifelse(test.raw$season %in% c(6, 7, 8), "Summer",
                           ifelse(test.raw$season %in% c(9, 10, 11), "Fall", "Winter")))
test.raw$season <- factor(test.raw$season, levels=c("Spring","Summer","Fall","Winter"))
tmp.date <- test.raw$date
dummy <- dummyVars("~.", data=test.raw)
test.raw <- data.frame(predict(dummy, newdata=test.raw))
test.raw[,13:18] <- lapply(test.raw[,13:18], as.factor)
test.raw$date <- tmp.date
summary(test.raw)

write.csv(test.raw, file='test_filled.csv', row.names=F)
test.raw <- read.csv('test_filled.csv')
test.raw$date <- as.Date(test.raw$date)
test.raw[,13:18] <- lapply(test.raw[,13:18], as.factor)

str(test.raw)

# test 데이터 강수량 채우기
### 이 부분부터 시작할 것
df.complete <- new.df
ts.idx <- sample(1:nrow(df.complete), size=round(nrow(df.complete)/4), replace=F)
prec.tr <- df.complete[-ts.idx, -13]
prec.ts <- df.complete[ts.idx, -c(2, 13)]
prec.ts.lb <- df.complete[ts.idx, 2]

type.name = c('eps-regression', 'nu-regression')
kernel.name = c('linear', 'polynomial','radial','sigmoid')

for(i in 1:2) {
  for(j in 1:4) {
    model <- svm(precipitation ~ ., data = prec.tr, type = type.name[i], kernel = kernel.name[j])
    result <- predict(model, prec.ts)
    result <- round(result)
    cat(type.name[i], '\t-\t', kernel.name[j], '\t:\t', MAE_FUN(prec.ts.lb, result), '\n')
  }
}

prec.tr <- new.df[, -13]
prec.model <- svm(precipitation~., data=prec.tr, type='eps-regression', kernel='polynomial')
test.raw.na <- test.raw[is.na(test.raw$precipitation), -2]
test.raw[is.na(test.raw$precipitation), 2] <- round(predict(prec.model, test.raw.na), 1)

test.raw[test.raw$precipitation < 1,2] <- 0
summary(test.raw)
str(test.raw)


# 마지막 결정
# 모델 생성
train <- new.df[,]
test <- test.raw

model <- svm(rental~., data=train, type='nu-regression', kernel='radial')
pred <- round(predict(model, test))
pred
write.csv(test.raw, 'test_filled.csv', row.names=F)


# 제출용 파일
submit <- read.csv('sample_submission.csv')
str(submit)
submit$rental <- pred
write.csv(submit, 'sample_submission.csv', row.names=F)
