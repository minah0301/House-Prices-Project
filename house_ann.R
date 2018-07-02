#ANN

#library
library(neuralnet)

#데이터 확인
dummy_all_Sale<-cbind(dummy,num_fea)
dummy_all_Sale_noid<-dummy_all_Sale[-1]
str(dummy_all_Sale_noid)
dim(dummy_all_Sale_noid)
tail(dummy_all_Sale)
head(dummy_all_Sale_noid)
#정규화나 표준화를 해야한다. 데이터가 종 모양의 곡선이면 scale() 함수를 이용해 표준화 하는 것이 맞다. 하지만 데이터가 균일 분포를 따르거나 엄격히 비정규적이라면 0-1범위로 정규화를 하는 것이 더 적절하다.
#정규화
house_norm<-as.data.frame(lapply(dummy_all_Sale_noid, normarlize))
str(house_norm)
dim(house_norm)
tail(house_norm)
#훈련 테스트를 75%,25%로 나누다.
house_train<-house_norm[1:1095,]
house_test<-house_norm[1096:1460,]
1460*0.75

#수치 예측을위한 신경망 훈련.은닉 노드가 하나뿐인 가장 단순한 다층 순방향 네트워크 훈련
house_model<-neuralnet(SalePrice~OverallQual+GrLivArea,data = house_train)
plot(house_model)

#모델 성능 평가-모델에 사용되지 않은 열들은다 없애줘야된다.
model_results<-compute(house_model,house_test[c(270,282)])

predicted_SalePrice<-model_results$net.result

#분류 문제가 아닌 수치 예측 문제이기 때문에 모델의 정학도를 검토할 때 혼동 행렬(confusion matriz)을 사용할 수 없다. 대신 가격의 가격과 실제 값의 상관관계를 측정해야 된다
#0.8이므로 매우 강한 관계
cor(predicted_SalePrice,house_test$SalePrice)

#모델 개선
house_model2<-neuralnet(SalePrice~OverallQual+GrLivArea,data = house_train,hidden = 5)
plot(house_model2)

#확인
#0.2이므로 더 낮아졌다.
model_results2<-compute(house_model2,house_test[c(270,282)])

predicted_SalePrice2<-model_results2$net.result
cor(predicted_SalePrice2,house_test$SalePrice)

