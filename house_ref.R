#library
library(ggplot2)
library(ggrepel)
library(plyr)
library(dplyr)
library(knitr)
library(gridExtra)

#data
train<-read.csv("data/train.csv",stringsAsFactors = FALSE)
test<-read.csv("data/test.csv",stringsAsFactors = FALSE)

test_labels<-test$Id
test$Id<-NULL
train$Id<-NULL

test$SalePrice<-NA

all<-rbind(train,test)

#EDA
ggplot(data = all[!is.na(all$SalePrice),], aes(x=GrLivArea,y=SalePrice))+
  geom_point(col="steelblue")+
  geom_smooth(method = "lm",se=FALSE, color="black")+
  scale_y_continuous(breaks = seq(0,800000,by=100000),labels = scales::comma)+
  geom_text_repel(aes(label=ifelse(GrLivArea[!is.na(SalePrice)]>4500,rownames(all),'')))

#524,1299 집값은 낮은데 가격은 저렴하고 만족도 높음
all[c(524,1299),c('SalePrice','GrLivArea','OverallQual')]

###########NA
#Pool
NAcol<-which(colSums(is.na(all))>0)
sort(colSums(sapply(all[NAcol], is.na)),decreasing = TRUE)

all$PoolQC[is.na(all$PoolQC)]<-'None'

cor(all$PoolArea,all$PoolQC)
plot(all$PoolArea,all$PoolQC)

ggplot(data=all,aes(x=PoolArea,y=PoolQC,color=OverallQual))+
  geom_point()+
  geom_smooth(method = "lm",color="black")+
  scale_color_gradient(low = "steelblue",high = "pink")

Qualities<-c('None'=0,'Po'=1,'Fa'=2,'TA'=3,'Gd'=4,'Ex'=5)

all$PoolQC<-as.integer(revalue(all$PoolQC,Qualities))
table(all$PoolQC)

all[all$PoolArea>0 & all$PoolQC==0,c('PoolArea','PoolQC','OverallQual')]

all[all$PoolArea>0 ,c('PoolArea','PoolQC','OverallQual')]

###by eye, imputed numbers
all$PoolQC[2421]<-1
all$PoolQC[2504]<-4
all$PoolQC[2600]<-1

#Miscellaneous
table(all$MiscFeature)
all$MiscFeature[is.na(all$MiscFeature)]<-'None'
all$MiscFeature<-as.factor(all$MiscFeature)

#Alley
all$Alley[is.na(all$Alley)]<-'None'
all$Alley<-as.factor(all$Alley)

#Fence
all$Fence[is.na(all$Fence)]<-'None'
all[!is.na(all$SalePrice),]%>%group_by(Fence)%>%summarise(median=median(SalePrice),counts=n())

all$Fence<-as.factor(all$Fence)

all[,c(FireplaceQu,Fireplaces)]
table(is.na(all$FireplaceQu))
table(all$Fireplaces)

#Fireplace variables
all$FireplaceQu[is.na(all$FireplaceQu)]<-'None'
all$FireplaceQu<-as.integer(revalue(all$FireplaceQu,Qualities))
table(all$FireplaceQu)

#Lot variables
ggplot(all[!is.na(all$LotFrontage),],aes(x=as.factor(Neighborhood),y=LotFrontage))+
  geom_bar(stat = 'summary',fun.y='median',fill='steelblue')+
  theme(axis.text.x = element_text(angle=45,hjust = 1))

ggplot(all[!is.na(all$LotFrontage),],aes(x=as.factor(MSSubClass),y=LotFrontage))+
  geom_bar(stat = 'summary',fun.y='median',fill='steelblue')+
  theme(axis.text.x = element_text(angle=45,hjust = 1))

numericVars<-which(sapply(all,is.numeric))
all_numVar<-all[,numericVars]
cor_numVar<-cor(all_numVar,use="pairwise.complete.obs")
cor_sorted<-as.matrix(sort(cor_numVar[,'LotFrontage'],decreasing = TRUE))

cor_numVar<-cor(dummy_all,use="pairwise.complete.obs")
cor_sorted<-tail(as.matrix(sort(cor_numVar[,'LotFrontage'],decreasing = TRUE)))

table(is.na(all$LotFrontage))

lot_lm<-lm(LotFrontage~LotArea+MSSubClass,data=all)
lot_predicted<-predict(lot_lm,all[is.na(all$LotFrontage),])


list_lot_predicted<-as.list(lot_predicted)

str(lot_predicted)
lot_predicted[["8"]]

dim(lot_predicted)
is(lot_predicted)
str(lot_predicted)
class(lot_predicted)

is.na(lot_predicted[['8']])
all[8,"LotFrontage"]

is.na(all$LotFrontage[8])



###########완성##################
for (i in 1:nrow(all)) {
  if(is.na(all[i,3])) {
    all[i,"LotFrontage"]<-sowhat[which(sowhat$numbers==i),2]
  }
}


numbers<-names(lot_predicted)
numbers_result<-unname(lot_predicted)
str(numbers_result)

sowhat<-data.frame(numbers,numbers_result)
dim(sowhat)
head(sowhat)

ggplot(all[!is.na(all$LotShape),],aes(x=as.factor(LotShape),y=SalePrice))+
  geom_bar(stat='summary',fun.y='min',fill='steelblue')


######have to see again
table(is.na(all$LotShape))
all$LotShape<-as.factor(all$LotShape)

all$LotConfig <- as.factor(all$LotConfig)

#Year- changed to 0 but can change to values with the values in YearBuilt..############
all$GarageYrBlt[is.na(all$GarageYrBlt)]<-0

table(is.na(all$GarageYrBlt))

length(which(is.na(all$GarageType)&is.na(all$GarageFinish)&is.na(all$GarageCond)&is.na(all$GarageQual)))

kable(all[!is.na(all$GarageType)&is.na(all$GarageFinish),c('GarageCars','GarageArea','GarageType','GarageCond','GarageQual','GarageFinish')])

all$GarageCond[2127]<-names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127]<-names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127]<-names(sort(-table(all$GarageFinish)))[1]

kable(all[2127,c('GarageCars','GarageArea','GarageType','GarageCond','GarageQual','GarageFinish')])


all$GarageCars[2577]<-0
all$GarageArea[2577]<-0
all$GarageType[2577]<-NA
length(which(is.na(all$GarageType)&is.na(all$GarageFinish)&is.na(all$GarageCond)&is.na(all$GarageQual)))

all$GarageType[is.na(all$GarageType)]<-'No Garage'
all$GarageType<-as.factor(all$GarageType)
table(is.na(all$GarageType))

ggplot(all,aes(x=as.factor(GarageFinish),y=SalePrice))+
  geom_bar(stat = 'summary',fun.y='min')

all$GarageFinish[is.na(all$GarageFinish)]<-'None'
Finish<-c('None'=0,'Unf'=1,'RFn'=2,'Fin'=3)

all$GarageFinish<-as.integer(revalue(all$GarageFinish,Finish))
table(all$GarageFinish)

all$GarageQual[is.na(all$GarageQual)]<-'None'
all$GarageQual<-as.integer(revalue(all$GarageQual,Qualities))


all$GarageCond[is.na(all$GarageCond)]<-'None'
all$GarageCond<-as.integer(revalue(all$GarageCond,Qualities))
table(is.na(all$GarageCond))

####Basement

length(which(is.na(all$BsmtQual)&is.na(all$BsmtCond)&is.na(all$BsmtExposure)&is.na(all$BsmtFinType1)&is.na(all$BsmtFinType2)))

length(which(is.na(all$BsmtQual)))
length(which(is.na(all$BsmtCond)))
length(which(is.na(all$BsmtExposure)))
length(which(is.na(all$BsmtFinType2)))
length(which(is.na(all$BsmtFinType1)))


all[!is.na(all$BsmtFinType1)&(is.na(all$BsmtCond)|is.na(all$BsmtQual)|is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2')]


all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]

all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtQual<-as.integer(revalue(all$BsmtQual, Qualities))

table(all$BsmtQual)

all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond<-as.integer(revalue(all$BsmtCond, Qualities))

all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)

all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, Exposure))

all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, FinType))
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, FinType))

all[(is.na(all$BsmtFullBath)|is.na(all$BsmtHalfBath)|is.na(all$BsmtFinSF1)|is.na(all$BsmtFinSF2)|is.na(all$BsmtUnfSF)|is.na(all$TotalBsmtSF)), c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]

all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0


########mansory..will have to check again later
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2]

table(all$MasVnrType)
table(is.na(all$MasVnrType))

all$MasVnrType[is.na(all$MasVnrType)] <- 'None'

all[!is.na(all$SalePrice),] %>% group_by(MasVnrType) %>% dplyr::summarise(median=median(SalePrice),counts=n()) %>% arrange(median)

all$MasVnrType<-as.factor(all$MasVnrType)
all$MasVnrArea[is.na(all$MasVnrArea)] <-0                                                                                                              
###MS Zoning
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)

#Kitchen

table(all$KitchenQual)
table(is.na(all$KitchenQual))
all$KitchenQual[is.na(all$KitchenQual)] <- 'TA'
all$KitchenQual<-as.integer(revalue(all$KitchenQual, Qualities))
sum(table(all$KitchenAbvGr))

#Utilities
kable(all[is.na(all$Utilities) | all$Utilities=='NoSeWa',1:9])
all$Utilities <- NULL

#Functional
table(is.na(all$Functional))
table(all$Functional)
ggplot(all,aes(x=as.factor(Functional),y=SalePrice))+
  geom_bar(stat='summary',fun.y='min')

all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]

all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(all$Functional)

##Exterior
table(all$Exterior1st)
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]

all$Exterior1st <- as.factor(all$Exterior1st)

all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]

all$Exterior2nd <- as.factor(all$Exterior2nd)

all$ExterQual<-as.integer(revalue(all$ExterQual, Qualities))
all$ExterCond<-as.integer(revalue(all$ExterCond, Qualities))

#Electric
all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]

all$Electrical <- as.factor(all$Electrical)

#Saletype and condition
all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]

all$SaleType <- as.factor(all$SaleType)
all$SaleCondition <- as.factor(all$SaleCondition)


#####year and month

ys<-ggplot(all[!is.na(all$SalePrice),],aes(x=as.factor(YrSold),y=SalePrice))+
  geom_bar(stat='summary',fun.y='median',fill='steelblue')+
  scale_y_continuous(breaks = seq(0,800000,by=25000),labels = scales::comma)+
  coord_cartesian(ylim = c(0,200000))+
  geom_hline(yintercept = 163000,linetype='dashed',color='red')

ms<-ggplot(all[!is.na(all$SalePrice),],aes(x=MoSold,y=SalePrice))+
  geom_bar(stat='summary',fun.y='median',fill='steelblue')+
  scale_y_continuous(breaks = seq(0,800000,by=25000),labels = scales::comma)+
  geom_label(stat = 'count',aes(label=..count..,y=..count..))+
  coord_cartesian(ylim = c(0,200000))+
  geom_hline(yintercept = 163000,linetype='dashed',color='red')

grid.arrange(ys,ms,widths=c(1,2))
