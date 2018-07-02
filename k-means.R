#k-means

#
str(dummy_all_Sale_noid)
colnames(dummy_all_Sale_noid)
length(dummy_all_Sale_noid)
dim(dummy_all_Sale_noid)

#
house<-dummy_all_Sale_noid[1:302]
colnames(house)
dim(house)
head(house)
tail(house)

#분석을 할 때 거리 계산을 이용할 경우 일반적으로 분석 전에 각 특징이 동일한 범위를 이용하게 특징을 정규화하거나 z-점수 표준화를 한다.
house_z<-as.data.frame(lapply(house,scale))
str(house_z)
summary(house_z)
dim(house_z)

#set.seed()는 R의 난수 발생기를 초기화해 특정 수열을 만든다.
set.seed(1234)
house_cluster<-kmeans(house_z,25)

#각 그룹에 속하는 예시의 수를 검토해 본다. 그룹이 너무 크거나 너무 작으면 매우 유용하지 않을 가능성이 있다.
# 13  49  10   1  12  95  76 150 143 174  17  28  61  37  63   6   8  17   7  12 121   1 232  52  75 로 유용하지 않은 것 같다
house_cluster$size
#$center 컴포넌트를 사용해서 클러스터 중심점의 좌표를 검토해볼 수 있다.
house_cluster$centers

#k를 3으로 해본다
house_cluster<-kmeans(house_z,3)
house_cluster$size
house_cluster$centers
