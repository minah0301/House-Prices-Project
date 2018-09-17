select Neighborhood, avg(SalePrice) as AvgSales, count(Neighborhood) as countNeighbor
from house_prices
group by Neighborhood
order by avg(SalePrice) DESC;

select MSSubClass, avg(SalePrice) as AvgSales
from house_prices
group by MSSubClass
order by avg(SalePrice) DESC;

select MSZoning, avg(SalePrice) as AvgSales
from house_prices
group by MSZoning
order by avg(SalePrice) DESC;

select OverallQual, avg(SalePrice) as AvgSales
from house_prices
group by OverallQual
order by avg(SalePrice) DESC;

select GrLivArea, avg(SalePrice) as AvgSales
from house_prices
group by GrLivArea
order by avg(SalePrice) DESC;

select distinct(GarageCars)
from house_prices;

select GarageCars, avg(SalePrice) as AvgSales
from house_prices
group by GarageCars
order by avg(SalePrice) DESC;

select ExterQual, avg(SalePrice) as AvgSales
from house_prices
group by ExterQual
order by avg(SalePrice) DESC;

select GrLivArea
from house_prices
where `id` IN (198,524,692,826,1171,1183,1424);

select avg(GrLivArea)
from house_prices;

select PoolArea
from house_prices
where `id` IN (198,1171,1183,1424);

select Id, PoolArea
from house_prices
where PoolArea>0;

select Id, PoolArea, SalePrice
from house_prices
where PoolArea>0;

select avg(SalePrice)
from house_prices
where PoolArea>0;

select avg(SalePrice)
from house_prices;

select SalePrice
from house_prices
group by SalePrice
Having SUM(SIGN)


select Id, GrLivArea
from house_prices
order by GrLivArea DESC
limit 30;

select avg(SalePrice) as AvgOutlier
from house_prices
where `Id` IN (198,524,692,826,1171,1183,1424);

select count(distinct LotFrontage)
from house_prices;

select distinct LotFrontage
from house_prices
order by LotFrontage DESC;

##########LotFrontage가 70인 집이 329개 있다. 총 110개의 LotFrontage가 있다.
select distinct LotFrontage, count(LotFrontage)
from house_prices
group by LotFrontage
order by count(LotFrontage) DESC;

select distinct LotFrontage, count(LotFrontage), avg(SalePrice)
from house_prices
group by LotFrontage
order by avg(SalePrice) DESC;

select Neighborhood
from house_prices
where LotFrontage=70
group by Neighborhood;

select distinct LotFrontage, Neighborhood, count(Neighborhood), avg(SalePrice)
from house_prices
where LotFrontage=70
group by LotFrontage,Neighborhood
order by avg(SalePrice) DESC;

select avg(LotFrontage)
from house_prices;







