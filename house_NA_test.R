### library
library(ggplot2)
library(reshape2)
library(data.table)
library(dplyr)

### data 불러오기
test<-read.csv("data/test.csv")

### NA가 있는 열 확인
colSums(is.na(test))
which(colSums(is.na(test))>0)

### MSZoning
head(test$MSZoning)
str(test$MSZoning)
summary(test$MSZoning)

ggplot(data=test,aes(x=Id,y=MSZoning))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="general zoning classification")

#NA가 4개 있으므로 최빈값 RL으로 바꿔줌
test[which(is.na(test[,"MSZoning"])),"MSZoning"]<-"RL"
summary(test$MSZoning)

### LotFrontage 탐색
head(test$LotFrontage)
str(test$LotFrontage)
summary(test$LotFrontage)

ggplot(data=test,aes(x=Id,y=LotFrontage))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Linear feet of street")

#NA 227개 중앙값 68로
hist(test$LotFrontage)
boxplot(test$LotFrontage)
test[which(is.na(test[,"LotFrontage"])),"LotFrontage"]<-67
summary(test$LotFrontage)

### Alley 탐색 
head(test$Alley)
str(test$Alley)
summary(test$Alley)

ggplot(data=test,aes(factor(Alley)))+ 
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Id",y="Type of alley")

# NA가 1352개 None으로 변경
levels(test$Alley)
levels(test$Alley)<-c("Grvl", "Pave","None") 
test[which(is.na(test[,"Alley"])),"Alley"]<-"None"
summary(test$Alley)
str(test$Alley)

#Utilities-1개 빼고 모두 AllPub이므로 변수 삭제해도 될꺼같음 
head(test$Utilities)
str(test$Utilities)
summary(test$Utilities)

ggplot(data=test,aes(factor(Utilities)))+ 
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Id",y="Type of utilities")

# NA가 2개이므로 'AllPub'으로 변경
levels(test$Utilities)
test[which(is.na(test[,"Utilities"])),"Utilities"]<-"AllPub"
summary(test$Utilities)
str(test$Utilities)

#Exterior1st
str(test$Exterior1st)
summary(test$Exterior1st)

ggplot(data=test,aes(factor(Exterior1st)))+ 
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Id",y="Exterior covering on house")

#NA가 1개 있으므로 최빈값 VinylSd으로 바꿔줌
test[which(is.na(test[,"Exterior1st"])),"Exterior1st"]<-"VinylSd"
summary(test$Exterior1st)

#Exterior2nd
str(test$Exterior2nd)
summary(test$Exterior2nd)

ggplot(data=test,aes(factor(Exterior2nd)))+ 
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Id",y="Exterior covering on house")

#NA가 1개 있으므로 최빈값 VinylSd으로 바꿔줌
test[which(is.na(test[,"Exterior2nd"])),"Exterior2nd"]<-"VinylSd"
 
### MasVnrType
head(test$MasVnrType)
str(test$MasVnrType)
summary(test$MasVnrType)

ggplot(data=test,aes(factor(MasVnrType)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Masonry veneer type")

#NA가 16개 있으므로 최빈값 None으로 바꿔줌

test[which(is.na(test[,"MasVnrType"])),"MasVnrType"]<-"None"
summary(train$MasVnrType)

### MasVnrArea
str(test$MasVnrArea)
summary(test$MasVnrArea)

ggplot(data=test,aes(x=Id,y=MasVnrArea))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Masonry veneer Area")

median(test$MasVnrArea,na.rm = TRUE) #중앙값이 0이다

#15개의 NA값을 중앙값 0으로 대체
is.na(test[,"MasVnrArea"])
which(is.na(test[,"MasVnrArea"]))
test[which(is.na(test[,"MasVnrArea"])),"MasVnrArea"]<-median(test$MasVnrArea,na.rm = TRUE)
summary(test$MasVnrArea)
str(test$MasVnrArea)

### BsmtQual
head(test$BsmtQual)
str(test$BsmtQual)
summary(test$BsmtQual)

ggplot(data=test,aes(factor(BsmtQual)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Height of the basement")

#NA 44개 None으로 변경
levels(test$BsmtQual)
levels(test$BsmtQual)<-c("Ex", "Fa", "Gd", "TA","None")
test[which(is.na(test[,"BsmtQual"])),"BsmtQual"]<-"None"
summary(test$BsmtQual)
str(test$BsmtQual)

### BsmtCond
head(test$BsmtCond)
str(test$BsmtCond)
summary(test$BsmtCond)

ggplot(data=test,aes(factor(BsmtCond)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="General condition of basement")

#NA 45개 None으로 변경
levels(test$BsmtCond)
levels(test$BsmtCond)<-c("Fa", "Gd", "Po", "TA","None") 
test[which(is.na(test[,"BsmtCond"])),"BsmtCond"]<-"None"
summary(test$BsmtCond)
str(test$BsmtCond)


### BsmtExposure
str(test$BsmtExposure)
summary(test$BsmtExposure)

ggplot(data=test,aes(factor(BsmtExposure)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="walkout of garden level walls")

#NA가 44개 있음. NO(노출 없음)는 951개 있다. NA를 None으로 변경
levels(test$BsmtExposure)
levels(test$BsmtExposure)<-c("Av", "Gd", "Mn", "No","None")
test[which(is.na(test[,"BsmtExposure"])),"BsmtExposure"]<-"None"
summary(test$BsmtExposure)
str(test$BsmtExposure)

### BsmtFinType1
str(test$BsmtFinType1)
summary(test$BsmtFinType1)

ggplot(data=test,aes(factor(BsmtFinType1)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Rating of basement finished area")

#NA 42개 None으로 변경
levels(test$BsmtFinType1)
levels(test$BsmtFinType1)<-c("ALQ", "BLQ", "GLQ", "LwQ", "Rec", "Unf","None")
test[which(is.na(test[,"BsmtFinType1"])),"BsmtFinType1"]<-"None"
summary(test$BsmtFinType1)
str(test$BsmtFinType1)

###BsmtFinSF1
str(test$BsmtFinSF1)
summary(test$BsmtFinSF1)

ggplot(data=test,aes(x=Id,y=BsmtFinSF1))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Type 1 finished square feet")
hist(test$BsmtFinSF1)

#1개의 NA값을 중앙값으로 대체
is.na(test[,"BsmtFinSF1"])
which(is.na(test[,"BsmtFinSF1"]))
test[which(is.na(test[,"BsmtFinSF1"])),"BsmtFinSF1"]<-median(test$BsmtFinSF1,na.rm = TRUE)
summary(test$MasVnrArea)
str(test$MasVnrArea)

### BsmtFinType2
str(test$BsmtFinType2)
summary(test$BsmtFinType2)

ggplot(data=test,aes(factor(BsmtFinType2)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Rating of basement finished area")

#NA 42개 None으로 변경
levels(test$BsmtFinType2)
levels(test$BsmtFinType2)<-c("ALQ", "BLQ", "GLQ", "LwQ", "Rec", "Unf","None")
test[which(is.na(test[,"BsmtFinType2"])),"BsmtFinType2"]<-"None"
summary(train$BsmtFinType2)
str(train$BsmtFinType2)


###BsmtFinSF2
str(test$BsmtFinSF2)
summary(test$BsmtFinSF2)

ggplot(data=test,aes(x=Id,y=BsmtFinSF2))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Type 2 finished square feet")
hist(test$BsmtFinSF2)

#1개의 NA값을 중앙값으로 대체
is.na(test[,"BsmtFinSF2"])
which(is.na(test[,"BsmtFinSF2"]))
test[which(is.na(test[,"BsmtFinSF2"])),"BsmtFinSF2"]<-median(test$BsmtFinSF2,na.rm = TRUE)
summary(test$BsmtFinSF2)
str(test$BsmtFinSF2)

#BsmtUnfSF
str(test$BsmtUnfSF)
summary(test$BsmtUnfSF)

ggplot(data=test,aes(x=Id,y=BsmtUnfSF))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Unfinished square feet")
hist(test$BsmtUnfSF)

#1개의 NA값을 중앙값으로 대체
is.na(test[,"BsmtUnfSF"])
which(is.na(test[,"BsmtUnfSF"]))
test[which(is.na(test[,"BsmtUnfSF"])),"BsmtUnfSF"]<-median(test$BsmtUnfSF,na.rm = TRUE)
summary(test$BsmtUnfSF)
str(test$BsmtUnfSF)

#TotalBsmtSF
str(test$TotalBsmtSF)
summary(test$TotalBsmtSF)

ggplot(data=test,aes(x=Id,y=TotalBsmtSF))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Total square feet")
hist(test$TotalBsmtSF)

#1개의 NA값을 중앙값으로 대체
is.na(test[,"TotalBsmtSF"])
which(is.na(test[,"TotalBsmtSF"]))
test[which(is.na(test[,"TotalBsmtSF"])),"TotalBsmtSF"]<-median(test$TotalBsmtSF,na.rm = TRUE)
summary(test$TotalBsmtSF)
str(test$TotalBsmtSF)

#BsmtFullBath
str(test$BsmtFullBath)
summary(test$BsmtFullBath)
table(test$BsmtFullBath)

ggplot(data=test,aes(x=Id,y=BsmtFullBath))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Basement full bathrooms")

#NA가 2개 있으므로 최빈값 0으로 바꿔줌
test[which(is.na(test[,"BsmtFullBath"])),"BsmtFullBath"]<-0
summary(test$BsmtFullBath)

#BsmtHalfBath
str(test$BsmtHalfBath)
summary(test$BsmtHalfBath)
table(test$BsmtHalfBath)

ggplot(data=test,aes(x=Id,y=BsmtHalfBath))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Basement half bathrooms")

#NA가 2개 있으므로 최빈값 0으로 바꿔줌
test[which(is.na(test[,"BsmtHalfBath"])),"BsmtHalfBath"]<-0
summary(test$BsmtHalfBath)

#KitchenQual
str(test$KitchenQual)
summary(test$KitchenQual)
table(test$KitchenQual)

ggplot(data=test,aes(x=Id,y=KitchenQual))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Kitchen quality")

#NA가 1개 있으므로 최빈값 TA으로 바꿔줌
test[which(is.na(test[,"KitchenQual"])),"KitchenQual"]<-"TA"
summary(test$KitchenQual)

#Functional
str(test$Functional)
summary(test$Functional)
table(test$Functional)

ggplot(data=test,aes(x=Id,y=Functional))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Home functionality")

#NA가 2개 있으므로 최빈값 Typ으로 바꿔줌
test[which(is.na(test[,"Functional"])),"Functional"]<-"Typ"
summary(test$Functional)


### FireplaceQu
str(test$FireplaceQu)
summary(test$FireplaceQu)

ggplot(data=test,aes(factor(FireplaceQu)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Fireplace Quality")

#NA 730개 None으로 변경
levels(test$FireplaceQu)
levels(test$FireplaceQu)<-c("Ex", "Fa", "Gd", "Po", "TA","None") 
test[which(is.na(test[,"FireplaceQu"])),"FireplaceQu"]<-"None"
summary(test$FireplaceQu)
str(test$FireplaceQu)

### GarageType
str(test$GarageType)
summary(test$GarageType)

ggplot(data=test,aes(factor(GarageType)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Garage location")

#NA 76개 None으로 변경
levels(test$GarageType)
levels(test$GarageType)<-c("2Types",  "Attchd",  "Basment", "BuiltIn", "CarPort", "Detchd","None") 
test[which(is.na(test[,"GarageType"])),"GarageType"]<-"None"
summary(test$GarageType)
str(test$GarageType)

### GarageYrBlt-22--이상치있음!!!
str(test$GarageYrBlt)
summary(test$GarageYrBlt) 

ggplot(data=test,aes(x=GarageYrBlt,y=Id))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Year Garage was Built")
# NA 78개 0으로 변경
test[which(is.na(test[,"GarageYrBlt"])),"GarageYrBlt"]<-0

### GarageFinish
str(test$GarageFinish)
summary(test$GarageFinish)

ggplot(data=test,aes(factor(GarageFinish)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Interior finish of Garage")

#NA 78개 None으로 변경
levels(test$GarageFinish)
levels(test$GarageFinish)<-c("Fin", "RFn", "Unf","None")
test[which(is.na(test[,"GarageFinish"])),"GarageFinish"]<-"None"
summary(test$GarageFinish)
str(test$GarageFinish)

###GarageCars
str(test$GarageCars)
summary(test$GarageCars)
table(test$GarageCars)

ggplot(data=test,aes(factor(GarageCars)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Size of garage in car capacity")

#NA가 1개 있으므로 최빈값 2으로 바꿔줌
test[which(is.na(test[,"GarageCars"])),"GarageCars"]<-2
summary(test$GarageCars)

#GarageArea
str(test$GarageArea)
summary(test$GarageArea)
table(test$GarageArea)

ggplot(data=test,aes(x=Id,y=GarageArea))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Size of garage in square feet")
#NA가 1개 있으므로 중앙값으로 바꿔줌
hist(test$GarageArea)
boxplot(test$GarageArea)
test[which(is.na(test[,"GarageArea"])),"GarageArea"]<-480
summary(test$GarageArea)


#### GarageQual
str(test$GarageQual)
summary(test$GarageQual)

ggplot(data=test,aes(factor(GarageQual)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Garage Quality")

#NA가 78개 인데 TA가 1293로 압도적으로 높음.None으로 바꿈
levels(test$GarageQual)
levels(test$GarageQual)<-c("Fa", "Gd", "Po", "TA","None") 
test[which(is.na(test[,"GarageQual"])),"GarageQual"]<-"None"
summary(test$GarageQual)
str(test$GarageQual)

### GarageCond
str(test$GarageCond)
summary(test$GarageCond)

ggplot(data=test,aes(factor(GarageCond)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Garage Condition")

# NA가 78개 None으로 변경. TA가 1328으로 압도적으로 높음.
levels(test$GarageCond)
levels(test$GarageCond)<-c("Ex", "Fa", "Gd", "Po", "TA","None")
test[which(is.na(test[,"GarageCond"])),"GarageCond"]<-"None"
summary(test$GarageCond)
str(test$GarageCond)

### PoolQC
str(test$PoolQC)
summary(test$PoolQC)

ggplot(data=test,aes(factor(PoolQC)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Pool Quality")

#NA가 1456으로 거의 대부분이고 수영장이없는집-None으로 바꿈
levels(test$PoolQC)
levels(test$PoolQC)<-c("Ex", "Gd","None") 
test[which(is.na(test[,"PoolQC"])),"PoolQC"]<-"None"
summary(test$PoolQC)
str(test$PoolQC)

### Fence
str(test$Fence)
summary(test$Fence)

ggplot(data=test,aes(factor(Fence)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Fence Quality")

#NA가 1169로 압도적으로 많으며 담장이 없는집-None으로 변경
levels(test$Fence)
levels(test$Fence)<-c("GdPrv","GdWo","MnPrv","MnWw","None")
test[which(is.na(test[,"Fence"])),"Fence"]<-"None"
summary(test$Fence)
str(test$Fence)

### MiscFeature
str(test$MiscFeature)
summary(test$MiscFeature)

ggplot(data=test,aes(factor(MiscFeature)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Miscellanious Feature")

#NA 1408 None으로 변경
levels(test$MiscFeature)
levels(test$MiscFeature)<-c("Gar2","Othr","Shed","None")
test[which(is.na(test[,"MiscFeature"])),"MiscFeature"]<-"None"
summary(test$MiscFeature)
str(test$MiscFeature)

#SaleType 
str(test$SaleType)
summary(test$SaleType)

ggplot(data=test,aes(factor(SaleType)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Miscellanious Feature")

#NA 1 최빈값 WD로 변경
levels(test$MiscFeature)
levels(test$MiscFeature)<-c("Gar2","Othr","Shed","None")
test[which(is.na(test[,"MiscFeature"])),"MiscFeature"]<-"None"
summary(test$MiscFeature)
str(test$MiscFeature)

test[which(is.na(test[,"SaleType"])),"SaleType"]<-"WD"
summary(test$SaleType)
