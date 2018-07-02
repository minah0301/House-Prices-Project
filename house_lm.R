###회귀분석

#데이터 기본 탐색
summary(train$SalePrice)
str(train)
colSums(is.na(train))

#평균값이(180900) 중앙값(163000)보다 크기 때문에 가격 분포는 오른쪽으로 꼬리가 긴(right-skewed) 분포다.
hist(train$SalePrice)
pairs(train)

#lm
house_model<-lm(SalePrice~.,data=train)
#Adjusted R-squared:  0.9193로 이 모델이 SalePrice를 약 91% 설명할 수 있다. 
#p-value: < 2.2e-16로 통계적으로 유의하다고 간주된다.
#변수를 포함시킬지 말지는 p-값을 보면 된다. 
summary(house_model)
