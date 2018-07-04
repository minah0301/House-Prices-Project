#randomForest

#library
library(randomForest)
library(caret)

#
set.seed(1234)

#랜덤 포레스트가 500개의 트리를 포함하고 각 분할 시점에 26개의 변수를 시도했다.
rf<-randomForest(SalePrice~.,data=train_levels_rtrain)
rf

#Error in predict.randomForest(rf, test) : 
#Type of predictors in new data do not match that of the training data.
#위 오류는 predict 시킬 때 나타나는 오류.
#train의 범주형 변수 levels과 test의 범주형 변수 levels가 달라서 생기는 문제이다. 
#for문으로 levels 맞춰주기
#levels안에 test$var로 해주면 안된다.
for (var in colnames(fac_fea)) {
  levels(test[[var]]) <- levels(train[[var]])
}

head(test[[1]])
class(test[[1]])
class(test[1])
train_levels<-cbind(fac_fea,num_fea)
train_levels
dim(train_levels)
1460*0.8
train_levels_rtrain<-train_levels[1:1168,]
train_levels_rtest<-train_levels[1169:1460,]
dim(train_levels_rtrain)
head(train_levels_rtrain)
dim(train_levels_rtest)

#predict
rf_predcited<-predict(rf,train_levels_rtest[1:80])


