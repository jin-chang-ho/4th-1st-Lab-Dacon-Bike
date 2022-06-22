library(caret)
library(lubridate)
library(e1071)

########데이터 load/전처리########
bike_before_origin<-read.csv("C:/Users/user/바탕 화면/Dacon_Seoul_Bike/HJK/data/train.csv")
bike_before_real<-read.csv("C:/Users/user/바탕 화면/Dacon_Seoul_Bike/HJK/data/real_data.csv")
head(bike_before_origin)
head(bike_before_real)

bike_before_real <- bike_before_real[,-c(14,15,16,17,18,19)]



#재경선배 weekly, season, 강수량(평균값 사용) 데이터 사용
bikeData_HJK<-read.csv("C:/Users/user/바탕 화면/Dacon_Seoul_Bike/HJK/data/new_data.csv")
head(bikeData_HJK)
bike_after<-cbind(bike_after, bike_before[,c(1,2,3,15)])


# 날짜, 강수량 정보, 결측값 제거
bike_after <- bike_before[,-c(14,15,16,17)]
#bike_after <- bike_after[,-14] #precipitation 제거용
bike_before_origin <- na.omit(bike_before_origin) #결측값 제거
head(bike_after)



# svm으로 예측용 데이터셋 만들기
bike_before_real <- bike_before_real[,-c(1)] # 강수량 칼럼 없애기

##bike_before_origin["precipitation"] <- bike_before_real["precipitation"]
bike_before_real <- bike_before_real[,-c(6)]#초먼열 엾애기
bike_before_real <- bike_before_real[,-c(5)]#미먼 열 없애기

#미먼/초먼 NA행 없애고 강수량 예측한 데이터셋 만들기
bike_before_origin <- na.omit(bike_before_origin) #모든 결측값 제거
prec.model <- svm(precipitation~., data=bike_before_origin, type='eps-regression', kernel='polynomial') #결측값 예측에 사용될 모델


bike_before_origin <- bike_before_origin[!((is.na(bike_before_origin$PM10) == TRUE || is.na(bike_before_origin$PM2.5) == TRUE)) ]
is.na(bike_before_origin)

#초먼미먼 결측값만 없애기
na.idx <- is.na(bike_before_origin$PM10) | is.na(bike_before_origin$PM2.5) | is.na(bike_before_origin$sunshine_sum)
bike_before_origin <- bike_before_origin[!na.idx, ]
temp <- bike_before_origin
na.idx <- is.na(temp$precipitation)
temp<-temp[na.idx,]
temp<-temp[,-c(1)]
pred <- round(predict(prec.model, temp),1)


j<-1
for(i in c(1:nrow(bike_before_origin))){
  if(is.na(bike_before_origin[i,"precipitation"])){
    bike_before_origin[i,"precipitation"] <- pred[j]
    j<-j+1
  }
}
bike_before_origin <-na.omit(bike_before_origin)

is.na(bike_before_origin[1,"precipitation"])

#범주형 데이터로 바꿈


#단순 labeling 방식
for(k in c(1:nrow(bike_after))){
  if(bike_after[k,"weekday"]=="Holiday"){
    bike_after[k,"weekday"]=1
  }else if(bike_after[k,"weekday"] == "Weekday"){ 
    bike_after[k,"weekday"]=0
  }
  
  if(bike_after[k,"season"]=="Winter"){
    bike_after[k,"season"]=0
    
  }else if(bike_after[k,"season"]=="Spring"){
    bike_after[k,"season"]=1
  }else if(bike_after[k,"season"]=="Summer"){
    bike_after[k,"season"]=2
  }else{
    bike_after[k,"season"]=3
  }
  
}



# normalization
normalize <- function(x) { # Min-Max Scaling
  return((x-min(x))/(max(x)-min(x)))
}

for(i in c(1:11)) {
  bike_before_origin[,i]<-normalize(bike_before_origin[,i])
}
head(bike_before_origin)

bike_before_origin[,1]<-normalize(bike_before_origin[,1])


#평가산식 함수
nmae<-function(answer_df, submission_df){
  
  #score<-0
  #for(i in c(1:length(answer_df))){
  # score<- (score + (abs(answer_df-submission_df)/answer_df))
  #}
  
  score <- sum(abs(answer_df-submission_df)/answer_df)/length(answer_df)
  
  return(score)
}



library(cvTools)
library(randomForest)
library(class)
library(tidyverse)
library(MASS)
library(xgboost)

# kfold 적용 모델 훈련 함수 
kfold <- function(classifier, ds, cl, fold,mtr,ntr) {
  acc <- c() #fold별acc 저\
  svm_pred<-c()
  for (i in 1:5) {
    ts.idx<-fold$subset[which(fold$which==i)]
    ds.train <- ds[-ts.idx, ]
    ds.test <-  ds[ts.idx, ]
    cl.train <- cl[-ts.idx]
    cl.test <- cl[ts.idx]
    
    if (classifier == 'svm') {
      model <- svm(ds.train, cl.train,type = 'eps-regression', kernel = 'radial')
      svm_pred[i] <- predict(model, ds.test)
      #acc[i] <- mean(pred==cl.test)
    }
    if (classifier == 'rf') {
      model <- randomForest(ds.train, cl.train, mtry = mtr,ntree = ntr,importance = TRUE) 
      pred <- round(predict(model, ds.test),0)
      acc[i] <- nmae(cl.test,pred)
    }

  }
  if(classifier == 'svm'){
    return(mean(svm_pred)) #예측값의 평균치를 반환
  }
  return(mean(acc))
    
}
 

set.seed(123) #재현성

# acc저장할 matrix 생성
result_mt<-matrix("",nrow=60,ncol=3) 
colnames(result_mt)<-c("svm","rf","xgb") #row,col 이름 지정
name_list<-c()
idx<-1
for(n in c(8:10)){
  for(m in c(1:20)){
    name_list[idx]<-paste(as.character(n),"_",as.character(m*50))
    idx<-idx+1
  }
}
rownames(result_mt)<-name_list


ds <- bike_before_origin[,-12]
cl <- bike_before_origin$rental

fold <- cvFolds(nrow(ds), K=5) #fold 생성
print(kfold('rf',ds,cl,fold,10,100))


#모델 훈련 및 최적 파라미터 탐색
idx<-1
for(i in c(8:10)){
  for(j in c(1:20)){
    
    result_mt[idx,2]<-kfold('rf',ds,cl,fold,i,j*50)
    idx <- idx+1
    
  }
}


#####test######
test_data<-read.csv("D:/단국대학교/Dacon_ddareung/dataset_origin/test.csv")
head(test_data)

#test data 전처리
normalize(test_data)

model_test <- randomForest(bike_after, bike_after$rental, mtry = mtr,ntree = ntr,importance = TRUE) 
pred <- round(predict(model, test_data),0)
write.csv(pred,"D:/단국대학교/Dacon_ddareung/submission.csv")


