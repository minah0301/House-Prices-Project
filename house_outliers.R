#이상치 처리

#library
library(car)
library(carData)

#
str(train)
dim(train)
colSums(is.na(train))


#선형 회귀
house_model<-lm(SalePrice~.,data=train)

#cook distance로 예측 결과의 각 열에 영향을을 주는 것 찾기. i번 관측이 있을 때와 없을 때 fitted Y의 변화를 측정한다.
cooksd<-cooks.distance(house_model)
length(cooksd)
table(is.na(cooksd))

#cook's distance plot을 그려준다.
plot(cooksd,pch="*",cex=2,main="Influential Obs by Cooks distance")

# cutoff line을 그려준다
abline(h=4*mean(cooksd,na.rm=T),col="red")

#label을 붙여준다.
text(x=1:length(cooksd)+1,y=cooksd,labels = ifelse(cooksd>4*mean(cooksd,na.rm=T),names(cooksd),""),col="red")

#영향있는 row 탐색-198,524,692,826,1171,1183,1424
influential<-as.numeric(names(cooksd)[(cooksd>4*mean(cooksd,na.rm=T))])
length(influential)

head(train[influential,])

#
train[c(198,524,692,826,1171,1183,1424),]

train[198,] #GrLivArea(3112)
train[524,] #LotFrontage(130),LotArea(40094),MasVnrArea(762),BsmtFinSF1(2260),TotalBsmtSF(3138),X1stFlrSF(3138),X2ndFlrSF(1538),FullBath(3),GrLivArea(4676)
train[692,] #GrLivArea(4316)
train[826,] #GrLivArea(2084),X1stFlrSF(2084),,
train[1171,] #PoolArea(576)
train[1183,] #PoolArea(555)
train[1424,] #PoolArea(738)



#이삭치 삭제-R^2이 0.94로 그전보다 성능이 향상됨
7/1460*100
train_no_outlier<-train[-c(198,524,692,826,1171,1183,1424),]
dim(train_no_outlier)

#수정후 선형 회귀 
house_model<-lm(SalePrice~.,data=train_no_outlier)
summary(house_model)
