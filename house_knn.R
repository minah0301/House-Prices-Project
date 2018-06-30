# knn

# library
library(class)
library(gmodels)

# 1.data 불러오기
str(dummy_all)
names(dummy_all)
head(dummy_all)

# 2. data 탐색과 준비
# Id 열은 단순히 해당 집의 아이디 이기때문에 중요한 정보가 아니므로 삭제한다.
dummy_all_noid<-dummy_all[-1]
head(dummy_all_noid)
length(dummy_all_noid)
str(dummy_all_noid)
names(dummy_all_noid)

# SalePrice를 중앙값보다 크면 2로 작거나 같으면 1로 바꾼다.
median(dummy_all_noid$SalePrice) #163000이다.

# 163000대신 median(dummy_all_noid$SalePrice)라고 입력하면 for문 돌릴때마다 median 값이 달라져 1과2의 비율이 732:728로 안나옴. High에 엄청 편중되게 나온다.
for (x in 1:length(dummy_all_noid$SalePrice)) {
  if (dummy_all_noid$SalePrice[x]>163000) { 
    dummy_all_noid$SalePrice[x]<-2
  } else {
    dummy_all_noid$SalePrice[x]<-1
  }
}

str(dummy_all_noid$SalePrice)
summary(dummy_all_noid$SalePrice)

# SalePrice를 중앙값 기준으로 나눴을 때 1,2의 갯수
table(dummy_all_noid$SalePrice)
str(dummy_all_noid$SalePrice)

#SalePrice를 factor로 바꾸고 labels 파라미터를 이용해 2와 1에 유용한 정보를 주는 레이블을 제공함
#levels의 순서를 잘 맞춰야함
dummy_all_noid$SalePrice<-factor(dummy_all_noid$SalePrice,levels = c(1,2),labels = c("Low","High"))
table(dummy_all_noid$SalePrice)

#prop.table()
round(prop.table(table(dummy_all_noid$SalePrice))*100,digits = 1)

#정규화 함수
normarlize<-function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

#SalePrice(37) 빼고 데이터 정규화
dummy_all_noid_n<-as.data.frame(lapply(dummy_all_noid[-37], normarlize))
summary(dummy_all_noid_n)
length(dummy_all_noid_n)

#훈련데이터 80 테스트데이터 20으로 나눈다.
dummy_all_noid_n_train<-dummy_all_noid_n[1:1168,]
dummy_all_noid_n_test<-dummy_all_noid_n[1169:1460,]
1460*0.8
dim(dummy_all_noid_n_train);dim(dummy_all_noid_n_test)

#정규화된 훈련 데이터셋과 테스트 데이터셋을 구성할 때 목표변수 SalePrice를 제외했었다. k-NN모델 훈련을 위해 이 클래스 레이블을 팩터 벡터에 저장하고 훈련 데이터 셋과 테스트 데이터 셋으로 분리한다.
dummy_all_noid_n_train_labels<-dummy_all_noid[1:1168,37]
dummy_all_noid_n_test_labels<-dummy_all_noid[1169:1460,37]

# 3.데이터로 모델 훈련. 훈련 데이터가 1168개 이므로 대략 1168의 제곱근 33으로 k를 정함.2범주 결과이므로 홀수를 사용해 동점 표로 끝날 가능성 제거
knn_pred<-knn(train=dummy_all_noid_n_train,test = dummy_all_noid_n_test,cl=dummy_all_noid_n_train_labels,k=33)

# 4.모델 성능 평가
CrossTable(x=dummy_all_noid_n_test_labels,y=knn_pred,prop.chisq=FALSE) # knn_pred벡터에 있는 예측된 클래스가 dummy_all_noid_n_test_labels 벡터에 있는 알려진 값과 얼마나 잘 일치하는 가를 평가하는 것임.
#SalePrice의 low과 high의 비율을 안 맞춰주고 high가 많게 나눠주면 dummy_all_noid_n_test_labels 의 High만 있고 밑에 low가 없어짐
#결과를 보면 좌측 상단(참부정,true negative) 즉 (Low,Low)는 134개,즉 45%가 집값이 낮고 k-NN알고리즘이 정확히 낮음으로 식별한 경우이다. 우측 하단(참긍정, true positive) 즉 (High,High)는 122개, 즉 41%고 분류기와 데이터처리로 판단된 레이블의 집값이 High라는 것에 동의한다.
#다른 대각선에 있는 셀들을 k-NN 방법이 실제 레이블과 일치하지 않는 예시의 개수를 포함한다.

# 5.모델 성능 개선(1.수치 특징 재조정하기 위한 z-점수 표준화, 2.k값 변화)
#1.z-점수 표준화(z-점수 표준화 값은 사전에 정의된 최솟값, 최댓값이 없기 때문에 극값이 중심 방향으로 축소되지 않는다. 매우 극단적인 이상치가 나타날수도 있으므로 거리계산에서 이상치에 큰 가중치는 두는 것이 좋다.)
dummy_all_noid_z<-as.data.frame(scale(dummy_all_noid[-37]))
#z-점수로 표준화된 변수의 평균은 항상 0이어야 하고 범위는 상당히 작아야 한다.
summary(dummy_all_noid_z$MSSubClass)

#이후 n한것과 똑같은 방법 실행
dummy_all_noid_z_train<-dummy_all_noid_z[1:1168,]
dummy_all_noid_z_test<-dummy_all_noid_z[1169:1460,]
#
dummy_all_noid_z_train_labels<-dummy_all_noid[1:1168,37]
dummy_all_noid_z_test_labels<-dummy_all_noid[1169:1460,37]
#
knn_pred_z<-knn(train=dummy_all_noid_z_train,test=dummy_all_noid_z_test,cl=dummy_all_noid_z_train_labels,k=33)
#
CrossTable(x=dummy_all_noid_z_test_labels,y=knn_pred_z,prop.chisq=FALSE)
#n과 z를 비교하면 n=45+41 즉 n일때 86%정확히 분류 됐고 z=44+43 즉 87% 분류됨으로 소폭 상승하였다.
n=45+41
n
z=44+43
z
#2.k값 대체
#k=1일때 85%정확도
knn_pred_z<-knn(train=dummy_all_noid_z_train,test=dummy_all_noid_z_test,cl=dummy_all_noid_z_train_labels,k=1)
#
CrossTable(x=dummy_all_noid_z_test_labels,y=knn_pred_z,prop.chisq=FALSE)

#k=50일때 87% 정확도
knn_pred_z<-knn(train=dummy_all_noid_z_train,test=dummy_all_noid_z_test,cl=dummy_all_noid_z_train_labels,k=50)
#
CrossTable(x=dummy_all_noid_z_test_labels,y=knn_pred_z,prop.chisq=FALSE)





