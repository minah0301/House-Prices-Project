# library 불러오기
library(dummies)

# data 가져오기
getwd()
train<-read.csv("data/train.csv")

# factor 변수 골라내기
fac_name<-sapply(train,is.factor)
fac_name

fac_fea<-train[,which(fac_name==TRUE)]
fac_fea

# numeric 변수 골라내기
num_name<-sapply(train,is.numeric)
num_name

num_fea<-train[,which(num_name==TRUE)]
num_fea

# one hot encoding -dummy성공
dummy<-dummy.data.frame(fac_fea, names = c(colnames(fac_fea)), sep="_")
head(dummy)
str(dummy)
dim(dummy)

# dummy와 numeric 합치기
dim(num_fea)
dummy_all<-cbind(num_fea,dummy)
head(dummy_all)
dim(dummy_all)
