#SVM

#library
library(kernlab)

#data
str(dummy_all_Sale_noid)
dim(dummy_all_Sale_noid)
colnames(dummy_all_Sale_noid)

#훈련,테스트 80%,20%
1460*0.8
house_train<-house_norm[1:1168,]
house_test<-house_norm[1169:1460,]
dim(house_train);dim(house_test)
1168+292

#
house_classifier<-ksvm(SalePrice~.,data=house_train,kernel="vanilladot")

#모델 성능 평가-다 예측 전혀 못함
house_predictions<-predict(house_classifier,house_test)
head(house_predictions)
table(house_predictions,house_test$SalePrice)

agreement<-house_predictions==house_test$SalePrice
table(agreement)

#모델 개선
house_classifier_rbf<-ksvm(SalePrice~.,data=house_train,kernel="rbfdot")
house_predictions_rbf<-predict(house_classifier_rbf,house_test)
agreement_rbf<-house_predictions_rbf==house_test$SalePrice
table(agreement_rbf)
