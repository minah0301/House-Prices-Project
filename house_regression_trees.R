#tree

#library
library(ggplot2)
library(rpart)
library(rpart.plot)
library(RWeka)
install.packages("RWeka")

#훈련,테스트 데이터 75%와 25%로 나누기
house_train<-train[1:1095,]
house_test<-train[1096:1460,]
1460*0.75

#회귀 트리 모델
m.rpart<-rpart(SalePrice~.,data=train)
summary(m.rpart)

#시각화
rpart.plot(m.rpart,digits = 3)

#모델 성능 평가
p.rpart<-predict(m.rpart,house_test)
#1분위와 3분위는 비슷하게 맞췄다. 그러나 예측의 min은 118200인데 실제는 52500, 예측의 max는 532100인데 실제는 74500으로 양 극단은 잘 식별하지 못했다.
summary(p.rpart)
summary(house_test$SalePrice)

#예측된 가격과 실제 값 사이의 상관관계는 모델의 성능을 측정하는 간단한 방법이다. 상관관계 0.82는 매우 높다.
cor(p.rpart,house_test$SalePrice)

#평균 절대 오차로 성능 측정.평균적으로 모델의 예측과 실제 품질 점수간의 차가 28236.21이다.
MAE<-function(actual,predicted) {
  mean(abs(actual-predicted))
}
MAE(p.rpart,house_test$SalePrice)

#모델 성능 개선-모델 트리 구축해보기
#모델 트리는 잎 노드를 회귀 모델로 대체함으로써 회귀 트리를 개선한다.
m.m5p<-M5P(SalePrice~.,data=house_train)
summary(m.m5p)

#테스트 데이터에 얼마나 잘 실행하는지 보기- 회귀 트리 보다 나아졌다. 
p.m5p<-predict(m.m5p,house_test)
summary(p.m5p)
summary(house_test$SalePrice)
