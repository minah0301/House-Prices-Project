#lm으로 outlier 제거 후 모델 fit시 결과가 너무 큼..glm도 너무 큼
#test 데이터 불러오기
getwd()
test_f<-read.csv("test_NA_removed.csv")

#수정후 선형 회귀 
house_model_no_outlier<-lm(SalePrice~.,data=train_no_outlier)
summary(house_model)

#Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#factor Condition2 has new levels PosN
house_model_no_outlier$xlevels[["Condition2"]] <- union(house_model_no_outlier$xlevels[["Condition2"]], levels(test_f$Condition2))

house_model_no_outlier$xlevels[["PoolQC"]] <- union(house_model_no_outlier$xlevels[["PoolQC"]], levels(test_f$PoolQC))

summary(house_model_no_outlier)

house_no_outlier_predicted<-predict(house_model_no_outlier,test_f)


#glm-Gamma
glmGamma<-glm(formula=SalePrice~.,family=Gamma(link=log),data =train_no_outlier)
summary(glmGamma)

#
Gamma_predicted<-predict(glmGamma,test_f,type = "response")
summary(Gamma_predicted)

glmGamma$xlevels[["Condition2"]] <- union(glmGamma$xlevels[["Condition2"]], levels(test_f$Condition2))

glmGamma$xlevels[["PoolQC"]] <- union(glmGamma$xlevels[["PoolQC"]], levels(test_f$PoolQC))



#glm-poisson
glmPoisson<-glm(formula=SalePrice~.,family=poisson(link="log"),data =train_no_outlier)
summary(glmGamma)

glmPoisson$xlevels[["Condition2"]] <- union(glmPoisson$xlevels[["Condition2"]], levels(test_f$Condition2))

glmPoisson$xlevels[["PoolQC"]] <- union(glmPoisson$xlevels[["PoolQC"]], levels(test_f$PoolQC))


Poisson_predicted<-predict(glmPoisson,test_f,type = "response")
summary(Poisson_predicted)

#OverallQual

Cor_model<-glm(formula = SalePrice~OverallQual+GrLivArea+GarageCars+TotalBsmtSF+FullBath+YearBuilt+YearRemodAdd+BsmtQual+KitchenQual,data=train_no_outlier,family=Gamma(link = "log"))

Cor_predicted<-predict(Cor_model,test_f,type = "response")

summary(Cor_predicted)

#submision 만들기
Id<-c(1461:2919)
tail(Id)
SalePrice<-Cor_predicted

subm<-data.frame(Id,SalePrice)
head(subm)
tail(subm)
getwd()
setwd("/Users/may/Documents/House Prices")
write.csv(subm,"glm_cor.csv",row.names = FALSE)
summary(OverallQual_model)
