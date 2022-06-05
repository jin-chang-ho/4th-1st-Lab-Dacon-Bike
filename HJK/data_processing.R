# data 불러오기
setwd("C:/Users/HJK/Desktop/lab/Dacon_Seoul_Bike/HJK/data")
train.raw <- read.csv("train.csv", stringsAsFactors=F)
test.raw <- read.csv("test.csv", stringsAsFactors=F)

# package 포함
library(ggplot2)
library(naniar)
library(psych)
library(rpart)
library(rpart.plot)
library(MASS)
library(lubridate)

# 함수 선언
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

replace <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# 데이터
train.normalize_no_na <- as.data.frame(lapply(train.no_na[,-1], normalize))
train.no_date <- train.raw[,-1]
train.no_na <- train.raw[complete.cases(train.raw),]
test.no_na <- test.raw[complete.cases(test.raw),]

# 탐색 및 전처리
str(train.raw)
summary(train.raw)

par(mfrow=c(3, 4))
for(i in 1:12) {
  boxplot(train.no_date[,i],
          main=colnames(train.no_date)[i])
}
par(mfrow=c(1,1))

par(mfrow=c(3, 4))
for(i in 1:12) {
  hist(train.no_date[,i],
          main=colnames(train.no_date)[i])
}
par(mfrow=c(1,1))

str(train.normalize)
summary(train.normalize)
summary(train.raw)

vis_miss(train.raw)


# 결측치 포함 행 제거 후 훈련
par(mfrow=c(3, 4))
for(i in 1:12) {
  hist(train.normalize_no_na[,i],
       main=colnames(train.normalize_no_na)[i])
}
par(mfrow=c(1,1))

cor(train.no_na[,-1])

pairs(train.no_na[,-1])
pairs.panels(train.no_na[,-1])

ins_model <- lm(rental~., data=train.no_na[,-1])
ins_model
summary(ins_model)

barplot(precipitation~date, data=train.no_na)

train.no_na2 <- train.no_na
train.no_na2$precipitation2 <- train.no_na2$precipitation^2

ins_model2 <- lm(rental~., data=train.no_na2[,-1])
summary(ins_model2)

train.no_na2$pred <- predict(ins_model2, train.no_na2[,-1])
cor(train.no_na2$pred, train.no_na2$rental)
plot(train.no_na2$pred, train.no_na2$rental)
abline(a=0, b=1, col="red", lwd=3, lty=2)


# 트리 회귀 적용
train <- train.no_na[1:280, ]
test <- train.no_na[281:386,]
m.rpart <- rpart(rental~., data=train[,-1])
m.rpart
summary(m.rpart)

rpart.plot(m.rpart, digits=3)
p.rpart <- predict(m.rpart, test[,-1])
summary(p.rpart)
summary(test$rental)
cor(p.rpart, test$rental)
MAE(p.rpart, test$rental)

mean(test$rental)
MAE(65142.05, test$rental)

# 실험
model <- lm(rental~., data=train.no_na[,-1])
model2 <- stepAIC(model)
model2
summary(model2)
summary(model)

# 정규화 후 회귀
train.clean_norm <- as.data.frame(lapply(train.no_na[,-1], normalize))
cor(train.clean_norm[,-1])

pairs(train.clean_norm[,-1])
pairs.panels(train.clean_norm[,-1])

ins_model <- lm(rental~., data=train.clean_norm[,-1])
ins_model
summary(ins_model)

train.clean_norm$precipitation2 <- train.clean_norm$precipitation^2

ins_model2 <- lm(rental~., data=train.clean_norm[,-1])
summary(ins_model2)

train.clean_norm$pred <- predict(ins_model2, train.clean_norm[,-1])
cor(train.clean_norm$pred, train.clean_norm$rental)
plot(train.clean_norm$pred, train.clean_norm$rental)
abline(a=0, b=1, col="red", lwd=3, lty=2)

model <- lm(rental~. data=train.clean_norm[,-1])
model2 <- stepAIC(model)
model2
summary(model)
summary(model2)

# 결측값 제거를 위한 실험
training <- function(df) {
  ins_model <- lm(rental~., data=df)
  print(ins_model)
  summary(ins_model)
  
  df$precipitation2 <- df$precipitation^2
  
  ins_model2 <- lm(rental~., data=df)
  summary(ins_model2)
  
  df$pred <- predict(ins_model2, df)
  print(cor(df$pred, df$rental))
  plot(df$pred, df$rental)
  abline(a=0, b=1, col="red", lwd=3, lty=2)
  
  model <- lm(rental~., data=df)
  model2 <- stepAIC(model)
  
  summary(model)
  summary(model2)
  
  
}

plot(df$date, df$rental, type='l')

df <- train.raw
df[,1] <- as.Date(df[,1])
df$weekday <- weekdays(df$date)
df$weekday <- factor(df$weekday %in% c('토요일','일요일'), labels=c('평일','주말'))

df$season <- month(df$date)
df$season <- ifelse(df$season %in% c(3, 4, 5), "봄",
                    ifelse(df$season %in% c(6, 7, 8), "여름",
                           ifelse(df$season %in% c(9, 10, 11), "가을", "겨울")))
df$season <- factor(df$season, levels=c("봄","여름","가을","겨울"))

agg <- aggregate(subset(df, select=-c(date, weekday, season)), by=list(season=df$season), mean, na.rm=T)
summary(df)
df[!complete.cases(df),"precipitation"] <- ifelse(df[!complete.cases(df),"season"]=="봄", agg$precipitation[1],
                                                  ifelse(df[!complete.cases(df),"season"]=="여름", agg$precipitation[2],
                                                         ifelse(df[!complete.cases(df),"season"]=="가을", agg$precipitation[3], agg$precipitation[4])))

df[!complete.cases(df),"PM10"] <- ifelse(df[!complete.cases(df),"season"]=="봄", agg$PM10[1],
                                                  ifelse(df[!complete.cases(df),"season"]=="여름", agg$PM10[2],
                                                         ifelse(df[!complete.cases(df),"season"]=="가을", agg$PM10[3], agg$PM10[4])))

df[!complete.cases(df),"PM2.5"] <- ifelse(df[!complete.cases(df),"season"]=="봄", agg$PM2.5[1],
                                                  ifelse(df[!complete.cases(df),"season"]=="여름", agg$PM2.5[2],
                                                         ifelse(df[!complete.cases(df),"season"]=="가을", agg$PM2.5[3], agg$PM2.5[4])))

df[!complete.cases(df),"sunshine_sum"] <- ifelse(df[!complete.cases(df),"season"]=="봄", agg$sunshine_sum[1],
                                                  ifelse(df[!complete.cases(df),"season"]=="여름", agg$sunshine_sum[2],
                                                         ifelse(df[!complete.cases(df),"season"]=="가을", agg$sunshine_sum[3], agg$sunshine_sum[4])))
training(df)

ins_model <- lm(rental~., data=df)
print(ins_model)
summary(ins_model)

df_tmp <- df
df_tmp$precipitation2 <- df$precipitation^2

model <- lm(rental~., data=df_tmp)
model2 <- stepAIC(model)

summary(model)
summary(model2)

model <- lm(rental~., data=df)
model2 <- stepAIC(model)

df_tmp$pred <- predict(model2, df_tmp)
cor(df_tmp$pred, df_tmp$rental)
plot(df_tmp$pred, df_tmp$rental)
abline(a=0, b=1, col="red", lwd=3, lty=2)

write.csv(df, file="new_data.csv", row.names=F)
write.csv(df_tmp, file="new_data2.csv", row.names=F)
