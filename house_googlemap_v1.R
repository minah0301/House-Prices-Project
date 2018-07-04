#library
library(ggplot2)
library(RgoogleMaps)
library(ggmap)
citation('ggmap') # ggmap을 사용하려면 입력해야함

#데이터
train<-read.csv("train.csv")
summary(train$Neighborhood)
str(train$Neighborhood)
table(is.na(train$Neighborhood))

#Neighborhood 변수에 있는 장소를 구글 지도에 나타내보기
N_names<-levels(train$Neighborhood)
length(levels(train$Neighborhood))
#N_addr<-c('Bloomington Rd', 'Bluestem', 'Briardale', 'Brookside', 'Clear Creek', 'College Creek', 'Crawford', 'Edwards', 'Gilbert', 'Iowa DOT and Rail Road', 'Meadow Village', 'Mitchell', 'North Ames', 'Northridge', 'Northpark Villa', 'Northridge Heights', 'Northwest Ames', 'Old Town', 'South & West of Iowa State University', 'Sawyer', 'Sawyer West', 'Somerset Apartments', 'Stone Brook', 'Timberland', 'Veenker Dr')
N_addr<-c('Westwind Drive Ames, IA','Colorado Ave Ames, IA','Briardale Dr
Iowa Falls, IA','1325 6th St, Ames, IA','Clear Creek, Ames, IA','College Creek, Ames, IA','Crawford Avenue, Ames, IA','Edwards Elementary School, Miller Avenue, Ames, IA','Gilbert, IA','800 Lincoln Way, Ames, IA','Meadow Lane Court, Ames, IA','Mitchell Elementary School, Jewel Drive, Ames, IA','N, Ames, IA','Northridge Parkway, Ames, IA','320 West Kimberly Rd, Sp 0001, Davenport, IA','Northridge Lane, Ames, IA','NW Ames Ave
Kansas City, MO','307 8th St, Ames, IA','South & West of Iowa State University, Ames, IA','Sawyer, IA','1223 S Gear Ave # 202, West Burlington, IA','Somerset, Ames, IA','Stone Brooke Road, Ames, IA','Timberland Road, Ames, IA','Veenker Drive, Ames, IA')
length(N_addr)

#위도 경도 확인
gc<-geocode(enc2utf8(N_addr))

#구글에 쿼리 몇 번 요청할 수 있는지 확인하기
geocodeQueryCheck()

#
df<-data.frame(name=N_names,lon=gc$lon,lat=gc$lat)

#중앙지점 정하기
cen<-c(mean(df$lon,na.rm = TRUE),mean(df$lat,na.rm=TRUE))

#
map<-get_googlemap(center=cen,
                   maptype="roadmap",
                   zoom=7,
                   maker=gc)
ggmap(map,extent = "normal",fullpage=TRUE) # 점안나옴

#########점 나오게 찍는 방법
ggmap(map,fullpage=TRUE)+
  geom_point(data = df,aes(x=lon,y=lat),color="red",alpha=0.8)+
  geom_text(data=df,aes(x=lon,y=lat+0.01,label=name),size=2)




