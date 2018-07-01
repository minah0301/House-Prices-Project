#Naive Bayes

#library
library(tm)
library(slam)
library(NLP)
library(devtools)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(gmodels)
library(SnowballC)

#데이터 확인
dim(fac_fea);dim(num_fea)
str(fac_fea)

#slam을 devtools이용해서 설치하기
slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)

#Sale Price 를 중앙값기준보다 높으면 2, 작거나 같으면 1로 나누기
fac_fea_SalePrice<-cbind(fac_fea,num_fea$SalePrice)
names(fac_fea_SalePrice)
str(fac_fea_SalePrice)
head(fac_fea_SalePrice)
length(fac_fea_SalePrice)
colnames(fac_fea_SalePrice)[44]<-"SalePrice"

for (x in 1:length(fac_fea_SalePrice$SalePrice)) {
  if (fac_fea_SalePrice$SalePrice[x]>163000) { 
    fac_fea_SalePrice$SalePrice[x]<-2
  } else {
    fac_fea_SalePrice$SalePrice[x]<-1
  }
}

str(fac_fea_SalePrice$SalePrice)
summary(fac_fea_SalePrice$SalePrice)

# SalePrice를 중앙값 기준으로 나눴을 때 1,2의 갯수
table(fac_fea_SalePrice$SalePrice)
str(fac_fea_SalePrice$SalePrice)

#SalePrice를 factor로 바꾸고 labels 파라미터를 이용해 2와 1에 유용한 정보를 주는 레이블을 제공함
#levels의 순서를 잘 맞춰야함
fac_fea_SalePrice$SalePrice<-factor(fac_fea_SalePrice$SalePrice,levels = c(1,2),labels = c("Low","High"))
table(fac_fea_SalePrice$SalePrice)
str(fac_fea_SalePrice)

table(fac_fea_SalePrice$BsmtQual)

#텍스트 문서 모음인 코퍼스(corpus)를 생성하기.fac_fea_SalePrice$BsmtQual벡터에서 VectorSouce() 리더(reader)함수를 이용해 소스 객체를 생성한후 VCorpus()에 제공한다.
house_corpus<-VCorpus(VectorSource(fac_fea_SalePrice$BsmtQual))
print(house_corpus)
#
inspect(house_corpus[1:2])
#
as.character(house_corpus[[1]])
#소문자로 변환
house_corpus_clean<-tm_map(house_corpus,content_transformer(tolower))
as.character(house_corpus_clean[[1]])
#숫자제거
house_corpus_clean<-tm_map(house_corpus_clean,removeNumbers)
#불용어(stop words) 제거
house_corpus_clean<-tm_map(house_corpus_clean,removeWords,stopwords())
#구두점 제거
house_corpus_clean<-tm_map(house_corpus_clean,removePunctuation)
#어근 형태의 동일한 용어 벡터를 반환한다.
corpus_clean<-tm_map(house_corpus_clean,stemDocument)
#추가 여백 제거
house_corpus_clean<-tm_map(house_corpus_clean,stripWhitespace)

#토큰화하기. DocumentTermMatrix()함수는 코퍼스를 가져와서 문서-단어 행렬(DTM,Document Term Matrix)이라고 하는 데이터 구조를 만든다.
#tm코퍼스가 주어지면 DTM희소 행렬의 생성은 다일 명령으로 가능하다.
house_dtm<-DocumentTermMatrix(house_corpus_clean)
#아래는 디폴트 설정을 재지정하는 control 파라미터를 설정한것
house_dtm2<-DocumentTermMatrix(house_corpus,control = list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=TRUE,
  removePunctuation=TRUE,
  stemming=TRUE
))
#훈련용 테스트 데이서셋 생성
house_dtm_train<-house_dtm[1:1095,]
house_dtm_test<-house_dtm[1095:1460,]
1460*0.75
#훈련 및 테스트 데이터 프레임의 행별 레이블을 갖는 벡터를 각각 저장해두는것이 좋다.
house_train_labels<-fac_fea_SalePrice[1:1095,]$SalePrice
house_test_labels<-fac_fea_SalePrice[1095:1460,]$SalePrice
#비율
prop.table(table(house_train_labels))
prop.table(table(house_test_labels))

#단어 구름
wordcloud(house_corpus_clean,min.freq = 3,random.order = FALSE)

#High와 Low 구분
High<-subset(fac_fea_SalePrice,SalePrice=="High")
Low<-subset(fac_fea_SalePrice,SalePrice=="Low")
str(High)

#High와 Low wordcloud보기
wordcloud(High$BsmtQual,max.words = 1,scale=c(3,0.5))
wordcloud(Low$BsmtQual,max.words = 1,scale=c(3,0.5))

#자주 나타나는 단어
house_freq_words<-findFreqTerms(house_dtm_train,1)
str(house_freq_words)

#
house_dtm_freq_train<-house_dtm_train[,house_freq_words]
house_dtm_freq_test<-house_dtm_test[,house_freq_words]

#
conver_count<-function(x) {
  x<-ifelse(x>0,"Yes","No")
}

#conver_count함수 적용하기
house_train<-apply(house_dtm_freq_train,MARGIN = 2,
                    conver_count)
house_test<-apply(house_dtm_freq_test,MARGIN = 2,
                   conver_count)

#모델 훈련
house_classifier<-naiveBayes(house_train,house_train_labels)

#모델 성능평가
#표를보면 336개의 단어중 179개가 부정확하게 분류되어 있다. Low 185개중 179개가 High로 잘못 분류 됨.
house_test_pred<-predict(house_classifier,house_test)
CrossTable(house_test_pred,house_test_labels,
           prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn = c('predicted','actual'))
#모델 성능 개선-라플라스 추정량을 설정한다. 
house_classifier2<-naiveBayes(house_train,house_train_labels,laplace = 1)
house_test_pred2<-predict(house_classifier2,house_test)

CrossTable(house_test_pred2,house_test_labels,
           prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn = c('predicted','actual'))
