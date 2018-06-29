### library
library(ggplot2)
library(reshape2)
library(data.table)
library(dplyr)

### data 불러오기
train<-read.csv("data/train.csv")

### NA가 있는 열 확인
colSums(is.na(train))
which(colSums(is.na(train))>0)

### LotFrontage 탐색
head(train$LotFrontage)
str(train$LotFrontage)
summary(train$LotFrontage)

ggplot(data=train,aes(x=Id,y=LotFrontage))+ #50피트 쯤에 여러개 있음.클러스터링?
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Linear feet of street")

# 이상치 2개를 뺀 평균
sort(train$LotFrontage,decreasing = TRUE)
which(train$LotFrontage==313)
train[,"LotFrontage"]

x<-train$LotFrontage
head(x)
length(x)
summary(x)
max(x)
xNoOutlier<-x[-c(935,1299)]
length(xNoOutlier)
max(xNoOutlier) #NA가이 있으므로 NA가 출력됨
summary(xNoOutlier)
mean(xNoOutlier,na.rm = TRUE)

# 259개의 NA값을 이상치 제거한 평균 69.6447으로 대체
train[,"LotFrontage"]
is.na(train[,"LotFrontage"])
which(is.na(train[,"LotFrontage"]))
train[which(is.na(train[,"LotFrontage"])),"LotFrontage"]<-mean(xNoOutlier,na.rm = TRUE)
summary(train$LotFrontage)

### Alley 탐색 
head(train$Alley)
str(train$Alley)
summary(train$Alley)

ggplot(data=train,aes(factor(Alley)))+ 
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Id",y="Type of alley")

# NA가 1369개 None으로 변경
levels(train$Alley)
levels(train$Alley)<-c("Grvl", "Pave","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"Alley"])),"Alley"]<-"None"
summary(train$Alley)
str(train$Alley)

### MasVnrType
head(train$MasVnrType)
str(train$MasVnrType)
summary(train$MasVnrType)

ggplot(data=train,aes(factor(MasVnrType)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Masonry veneer type")

#NA가 8개 있으므로 최빈값 None으로 바꿔줌

train[which(is.na(train[,"MasVnrType"])),"MasVnrType"]<-"None"
summary(train$MasVnrType)

### MasVnrArea
str(train$MasVnrArea)
summary(train$MasVnrArea)

ggplot(data=train,aes(x=Id,y=MasVnrArea))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Id",y="Masonry veneer Area")

median(train$MasVnrArea,na.rm = TRUE) #중앙값이 0이다

#8개의 NA값을 중앙값 0으로 대체
is.na(train[,"MasVnrArea"])
which(is.na(train[,"MasVnrArea"]))
train[which(is.na(train[,"MasVnrArea"])),"MasVnrArea"]<-median(train$MasVnrArea,na.rm = TRUE)
summary(train$LotFrontage)

### BsmtQual
head(train$BsmtQual)
str(train$BsmtQual)
summary(train$BsmtQual)

ggplot(data=train,aes(factor(BsmtQual)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Height of the basement")

#NA 37개 None으로 변경

levels(train$BsmtQual)
levels(train$BsmtQual)<-c("Ex", "Fa", "Gd", "TA","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"BsmtQual"])),"BsmtQual"]<-"None"
summary(train$BsmtQual)
str(train$BsmtQual)

### BsmtCond
head(train$BsmtCond)
str(train$BsmtCond)
summary(train$BsmtCond)

ggplot(data=train,aes(factor(BsmtCond)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="General condition of basement")

#NA 37개 None으로 변경
levels(train$BsmtCond)
levels(train$BsmtCond)<-c("Fa", "Gd", "Po", "TA","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"BsmtCond"])),"BsmtCond"]<-"None"
summary(train$BsmtCond)
str(train$BsmtCond)


### BsmtExposure
str(train$BsmtExposure)
summary(train$BsmtExposure)

ggplot(data=train,aes(factor(BsmtExposure)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="walkout of garden level walls")

#NA가 38개 있음. NO(노출 없음)는 953개 있다. NA를 None으로 변경
levels(train$BsmtExposure)
levels(train$BsmtExposure)<-c("Av", "Gd", "Mn", "No","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"BsmtExposure"])),"BsmtExposure"]<-"None"
summary(train$BsmtExposure)
str(train$BsmtExposure)

### BsmtFinType1
str(train$BsmtFinType1)
summary(train$BsmtFinType1)

ggplot(data=train,aes(factor(BsmtFinType1)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Rating of basement finished area")

#NA 37개 None으로 변경
levels(train$BsmtFinType1)
levels(train$BsmtFinType1)<-c("ALQ", "BLQ", "GLQ", "LwQ", "Rec", "Unf","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"BsmtFinType1"])),"BsmtFinType1"]<-"None"
summary(train$BsmtFinType1)
str(train$BsmtFinType1)

### BsmtFinType2
str(train$BsmtFinType2)
summary(train$BsmtFinType2)

ggplot(data=train,aes(factor(BsmtFinType2)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Rating of basement finished area")

#NA 38개 None으로 변경
levels(train$BsmtFinType2)
levels(train$BsmtFinType2)<-c("ALQ", "BLQ", "GLQ", "LwQ", "Rec", "Unf","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"BsmtFinType2"])),"BsmtFinType2"]<-"None"
summary(train$BsmtFinType2)
str(train$BsmtFinType2)

### Electrical
str(train$Electrical)
summary(train$Electrical)

ggplot(data=train,aes(factor(Electrical)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Electrical System")

#NA값 1개를 최빈값 SBrkr로 바꿔준다.
train[which(is.na(train[,"Electrical"])),"Electrical"]<-"SBrkr"
summary(train$Electrical)

### FireplaceQu
str(train$FireplaceQu)
summary(train$FireplaceQu)

ggplot(data=train,aes(factor(FireplaceQu)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Fireplace Quality")

#NA 690개 None으로 변경
levels(train$FireplaceQu)
levels(train$FireplaceQu)<-c("Ex", "Fa", "Gd", "Po", "TA","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"FireplaceQu"])),"FireplaceQu"]<-"None"
summary(train$FireplaceQu)
str(train$FireplaceQu)

### GarageType
str(train$GarageType)
summary(train$GarageType)

ggplot(data=train,aes(factor(GarageType)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Garage location")

#NA 81개 None으로 변경
levels(train$GarageType)
levels(train$GarageType)<-c("2Types",  "Attchd",  "Basment", "BuiltIn", "CarPort", "Detchd","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"GarageType"])),"GarageType"]<-"None"
summary(train$GarageType)
str(train$GarageType)

### GarageYrBlt
str(train$GarageYrBlt)
summary(train$GarageYrBlt) #NA가 81개...몇년도로 해야될까..?

ggplot(data=train,aes(x=GarageYrBlt,y=Id))+ 
  geom_point()+
  theme_minimal()+
  labs(x="Year Garage was Built")

### GarageFinish
str(train$GarageFinish)
summary(train$GarageFinish)

ggplot(data=train,aes(factor(GarageFinish)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Interior finish of Garage")

#NA 81개 None으로 변경
levels(train$GarageFinish)
levels(train$GarageFinish)<-c("Fin", "RFn", "Unf","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"GarageFinish"])),"GarageFinish"]<-"None"
summary(train$GarageFinish)
str(train$GarageFinish)

#### GarageQual
str(train$GarageQual)
summary(train$GarageQual)

ggplot(data=train,aes(factor(GarageQual)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Garage Quality")

#NA가 81개 인데 TA가 1311로 압도적으로 높으니깐...이걸로 바꿀까?? None으로 바꿈
levels(train$GarageQual)
levels(train$GarageQual)<-c("Ex", "Fa", "Gd", "Po", "TA","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"GarageQual"])),"GarageQual"]<-"None"
summary(train$GarageQual)
str(train$GarageQual)

### GarageCond
str(train$GarageCond)
summary(train$GarageCond)

ggplot(data=train,aes(factor(GarageCond)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Garage Condition")

# NA가 81개 None으로 변경. TA가 1326으로 압도적으로 높음.
levels(train$GarageCond)
levels(train$GarageCond)<-c("Ex", "Fa", "Gd", "Po", "TA","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"GarageCond"])),"GarageCond"]<-"None"
summary(train$GarageCond)
str(train$GarageCond)

### PoolQC
str(train$PoolQC)
summary(train$PoolQC)

ggplot(data=train,aes(factor(PoolQC)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Pool Quality")

#NA가 1453으로 거의 대부분이고 수영장이없는집-None으로 바꿈
levels(train$PoolQC)
levels(train$PoolQC)<-c("Ex", "Fa", "Gd","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"PoolQC"])),"PoolQC"]<-"None"
summary(train$PoolQC)
str(train$PoolQC)

### Fence
str(train$Fence)
summary(train$Fence)

ggplot(data=train,aes(factor(Fence)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Fence Quality")

#NA가 1179로 압도적으로 많으며 담장이 없는집-None으로 변경
levels(train$Fence)
levels(train$Fence)<-c("GdPrv","GdWo","MnPrv","MnWw","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"Fence"])),"Fence"]<-"None"
summary(train$Fence)
str(train$Fence)

### MiscFeature
str(train$MiscFeature)
summary(train$MiscFeature)

ggplot(data=train,aes(factor(MiscFeature)))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(x="Miscellanious Feature")

#NA 1406 None으로 변경
levels(train$MiscFeature)<-c("Gar2","Othr","Shed","TenC","None") #level지정을 안하면 새로운 레벨의 변수를 입력하지 못한다.(아래 코드가 실행되지 않는다.)
train[which(is.na(train[,"MiscFeature"])),"MiscFeature"]<-"None"
summary(train$MiscFeature)
str(train$MiscFeature)
