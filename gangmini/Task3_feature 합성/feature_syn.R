library(lubridate)

source("C:/Users/user/바탕 화면/따릉이_백업/custom_function.R")
data<-read.csv("C:/Users/user/바탕 화면/Dacon_Seoul_Bike/HJK/data/real_data.csv")
head(data)

# 날짜 데이터 모델별 분리
xg_data<-splitData(data)
svm_data<-data
svm_data$date <- as.Date(svm_data$date)

# 정규화
for(i in c(1:11)) {
  xg_data[,i]<-normalize(xg_data[,i])
}
for(i in c(2:12)) {
  svm_data[,i]<-normalize(svm_data[,i])
}


head(xg_data) # 데이터 확인
head(svm_data)

#년도별로 temp_mean 데이터의 사분위수 구하고, 평균 구함
acc<-c()
tmp<-subset(xg_data, year==2018)
acc[1]<-quantile(tmp$temp_mean, 0.5)
tmp<-subset(xg_data, year==2019)
acc[2]<-quantile(tmp$temp_mean, 0.5)
tmp<-subset(xg_data, year==2020)
acc[3]<-quantile(tmp$temp_mean, 0.5)
quan.m<-mean(acc)

acc<-c()
tmp<-subset(xg_data, year==2018)
acc[1]<-quantile(tmp$temp_mean, 0.25)
tmp<-subset(xg_data, year==2019)
acc[2]<-quantile(tmp$temp_mean, 0.25)
tmp<-subset(xg_data, year==2020)
acc[3]<-quantile(tmp$temp_mean, 0.25)
quan.l<-mean(acc)

acc<-c()
tmp<-subset(xg_data, year==2018)
acc[1]<-quantile(tmp$temp_mean, 0.75)
tmp<-subset(xg_data, year==2019)
acc[2]<-quantile(tmp$temp_mean, 0.75)
tmp<-subset(xg_data, year==2020)
acc[3]<-quantile(tmp$temp_mean, 0.75)
quan.r<-mean(acc)


#각 분위 구간을 다시 2등분해 편차 정해줌


#각 사분위~편차 안에 속하는 데이터에 따라 가중치를 다르게 줌(temp_mean에 가중치를 곱해서 새로운 변수 생성)
for(i in 1:nrow(xg_data)){
  if(xg_data[i,"temp_mean"] < ((quan.l)-2) || xg_data[i,"temp_mean"] > ((quan.r)+2)){
    xg_data[i,"weight"]<-2
  }else if((xg_data[i,"temp_mean"]>=((quan.l)-2) && xg_data[i,"temp_mean"]<=((quan.l)+4)) || (xg_data[i,"temp_mean"]>=((quan.r)-4) && xg_data[i,"temp_mean"]<=((quan.r)+2))){
    xg_data[i,"weight"]<-1
  }else{
    xg_data[i,"weight"]<-0.5
  }
}

#temp.mean과 가중치 곱한 합성 feature 만들기
for(j in 1:nrow(xg_data)){
  xg_data[j,"temp_weight"]<-xg_data[j,"temp_mean"]*xg_data[j,"weight"]
}

xg_data <- xg_data[,-c(2,3,4,22)]#가중치 칼럼제거, 원래 temp 칼럼들 제거


#정규화
for(i in c(1:8)) {
  xg_data[,i]<-normalize(xg_data[,i])
}
xg_data[,19]<-normalize(xg_data[,19])

#kfold로 예측
set.seed(123) #재현성
ds <- xg_data[,-9]
cl <- xg_data$rental

fold <- cvFolds(nrow(ds), K=5) #fold 생성
print(kfold('xgb',ds,cl,fold))
