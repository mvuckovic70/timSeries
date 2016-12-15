# 1. LOADING DATASETS

# executing import functions
# partner data not needed for this type of analysis

rm(list=ls())

source('D:/Projects/mhs/code/final/0_initialization.r')

library(dplyr)

category <- import_category()
# partner <- import_partner()
product <- import_product()
productCategory <- import_product_category()
sales_purchases <- import_sales_purchases()
warehouse <- import_warehouse()

# exclude irrelevant shops

target=c(6,7,8,9,14,15,20)
warehouse <- filter(warehouse, warehouseid %in% target)

# 2. DATA TRANFORMATION

# standardizing features properties

names(sales_purchases)[1] <- 'productid'
names(sales_purchases)[9] <- 'warehouseid'
names(sales_purchases)[10] <- 'partnerid'
names(sales_purchases)[12] <- 'storeid'

sales_purchases$date <- as.Date(sales_purchases$date, format='%d.%m.%Y')

manufacturer <- as.data.frame(unique(product$manufname))
manufacturer$manufacturerid <- row.names(manufacturer)
names(manufacturer)[1] <- 'manufname'

# merging manufacturer and product data frames

product <- merge(manufacturer, product, by = 'manufname', all.x = TRUE)
productCategory <- merge(productCategory, category, by = 'categoryprid', all.y = TRUE)

# data frame sales_purchases contains both sales and procurements
# splitting in two main parts: sales and purchase

purchase <- subset(sales_purchases, sales_purchases$type =="N")
sales <- subset(sales_purchases, sales_purchases$type =="P")

# adding stores, products and categories to sales for exploratory analysis

sales <- merge(sales, warehouse, by='warehouseid', all.x = TRUE)
sales <- merge(sales, product, by='productid', all.x = TRUE)
sales <- merge(sales, productCategory, by = 'productid')

names(sales)[13] <- 'warehouseName'
names(sales)[16] <- 'productName'
names(sales)[18] <- 'categoryName'

# adding different time components

library(lubridate)
library(zoo)

sales$week <- week(sales$date)
sales$year <- year(sales$date)
sales$weekYear <- strftime(sales$date,format="%Y-%W")
sales$monthYear <- strftime(sales$date,format="%Y-%m")
sales$weekYear <-  as.factor(sales$weekYear)
sales$monthYear <-  as.factor(sales$monthYear)

# rounding numbers

sales$profit <- round(sales$profit, 2)
sales$amount <- round(sales$amount, 2)
sales$profitAmount <- round(sales$profitAmount, 2)
sales$profitRate <- with(sales, profitAmount/amount)

# 3. OUTLIER DETECTION AND REMOVAL

# search for outliers

library(dplyr)

outlier_values <- boxplot.stats(sales$amount)$out  # outlier values.
boxplot(sales$amount, main="EUR", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

# detect largest outliers

max(sales$amount)
tail(sort(sales$amount),5)
which(sales$amount==max(sales$amount))

# 4. AGGREGATIONS

# sales for period 2006 - 2008, filtering dates
# removing wholesale department containing outliers
# and set amount to be > 0 to exclude returns

salesBetween <- subset(sales, sales$date >= '2006-01-01' & sales$date <= '2008-12-31' & storeid != 'M6' & amount > 0)
salesAfter <- subset(sales, sales$date >= '2009-01-01' & sales$date <= '2010-12-31' & storeid != 'M6' & amount > 0)

# dealing with missing values

sum(is.na(salesBetween$warehouseName))
salesBetween <- na.omit(salesBetween)
salesAfter <- na.omit(salesAfter)

# total sales by store

salesByWarehouse <- with(salesBetween, aggregate(amount, by=list(warehouseName = warehouseName, monthYear = monthYear), FUN = sum))
names(salesByWarehouse)[3] = 'amount'
salesByWarehouse$warehouseName <- as.factor(salesByWarehouse$warehouseName)
salesByWarehouse$monthYear <- as.factor(salesByWarehouse$monthYear)

# total sales by category

salesByCategory <- with(salesBetween, aggregate(amount, by=list(categoryName = categoryName, monthYear = monthYear), FUN = sum))
names(salesByCategory)[3] = 'amount'
salesByCategory$categoryName <- as.factor(salesByCategory$categoryName)
salesByCategory$monthYear <- as.factor(salesByCategory$monthYear)

# total sales by manufacturer

salesByManufacturer <- with(salesBetween, aggregate(amount, by=list(manufname = manufname, monthYear = monthYear), FUN = sum))
names(salesByManufacturer)[3] = 'amount'
salesByManufacturer$manufname <- as.factor(salesByManufacturer$manufname)
salesByManufacturer$monthYear <- as.factor(salesByManufacturer$monthYear)

# total sales by store in weeks

salesTotalWeek <- with(salesBetween, aggregate(amount, by=list(warehouseName = warehouseName, weekYear = weekYear), FUN = sum))
names(salesTotalWeek)[3] = 'amount'
names(salesTotalWeek)[2] = 'week'
salesTotalWeek$warehouseName <- as.factor(salesTotalWeek$warehouseName)
salesTotalWeek$weekYear <- as.factor(salesTotalWeek$week)

salesTotalWeekAfter <- with(salesAfter, aggregate(amount, by=list(warehouseName = warehouseName, weekYear = weekYear), FUN = sum))
names(salesTotalWeekAfter)[3] = 'amount'
names(salesTotalWeekAfter)[2] = 'week'
salesTotalWeekAfter$warehouseName <- as.factor(salesTotalWeekAfter$warehouseName)
salesTotalWeekAfter$weekYear <- as.factor(salesTotalWeekAfter$week)

# total sales by store in months

salesTotalMonth <- with(salesBetween, aggregate(amount, by=list(warehouseName = warehouseName, monthYear = monthYear), FUN = sum))
names(salesTotalMonth)[3] = 'amount'
salesTotalMonth$warehouseName <- as.factor(salesTotalMonth$warehouseName)
salesTotalMonth$monthYear <- as.factor(salesTotalMonth$monthYear)

# total sales by store profitability in months

salesTotalProfit <- with(salesBetween, aggregate(profitAmount, by=list(warehouseName = warehouseName, monthYear = monthYear), FUN = sum))
names(salesTotalProfit)[3] = 'profit'
salesTotalProfit$warehouseName <- as.factor(salesTotalProfit$warehouseName)
salesTotalProfit$monthYear <- as.factor(salesTotalProfit$monthYear)

# total sales by store revenue and profitability in months

salesTotalRevenue <- aggregate(cbind(profitAmount, amount)~warehouseName + monthYear, data=salesBetween, sum)
salesTotalRevenue$profitRate <- with(salesTotalRevenue, profitAmount/amount)
names(salesTotalProfit)[3] = 'profit'
salesTotalProfit$warehouseName <- as.factor(salesTotalProfit$warehouseName)
salesTotalProfit$monthYear <- as.factor(salesTotalProfit$monthYear)

# total sales scaled

salesTotalScaled <- as.data.frame(scale(salesBetween$amount))
salesTotalScaled <- cbind(salesTotalScaled, monthYear = salesBetween$monthYear, warehouseName = salesBetween$warehouseName)
names(salesTotalScaled)[1] <- 'amount'

# find starting dates for all stores for relevance

library(sqldf)

df <- sqldf('select warehouseName, min(date) as start, max(date) as end from salesBetween group by warehouseName')
df$start <- as.Date(df$start)
df$end <- as.Date(df$end)
df$duration <- with(df, difftime(end, start))
df

# remove primary tables and clear the screen

rm('warehouse', 'sales_purchases','sales', 'purchase', 'category','manufacturer',
                'product','productCategory', 'outlier_values', 'paths', 'target')

cat("\014")

  
