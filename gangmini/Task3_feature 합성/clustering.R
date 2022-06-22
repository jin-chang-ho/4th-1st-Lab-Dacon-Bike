source("C:/Users/user/바탕 화면/따릉이_백업/custom_function.R")
data<-read.csv("C:/Users/user/바탕 화면/Dacon_Seoul_Bike/HJK/data/real_data.csv")
head(data)

# 날짜 데이터 변환
data<-splitData(data)

#정규화
for(i in c(1:11)) {
  data[,i]<-normalize(data[,i])
}




library("cluster")
set.seed(124)

data<-data[,-c(12)]#rental 제거

fit<-kmeans(x=data[,-c(12:21)],centers = 4) 
fit$centers #각 군집의 feature의 중심점
fit$cluster

#clusplot(new_data,fit$cluster,color = TRUE,labels=4,lines = 0)
out <- rep(1,nrow(data))
for(i in c(1:nrow(data))){
  if(data[i,"season.Winter"]==1){
    out[i]<-1
  }else if(data[i,"season.Spring"]==1){
    out[i]<-4
  }else if(data[i,"season.Summer"]==1){
    out[i]<-2
  }else if(data[i,"season.Fall"]==1){
    out[i]<-3
  }
}

color = c('black','red','blue','green')
clusplot(data,fit$cluster,color = TRUE,labels=4,lines = 0,col.p=color[out])

