str(attr_data)
summary(attr_data)

library(readxl)

sales_data <- read_excel("C:/Users/admin/Desktop/DS with R/Projects/Retail sales rpt analysis/Dress Sales.xlsx", head())
head(sales_data)

as.Date(names(sales_data)[c(-1,-2,-3)], origin = '1899-12-30') 
dim(sales_data)

names(sales_data)

names(sales_data)[4]<-as.Date(c(41314), origin = '1899-12-30')
names(sales_data)[4] <- "2013-02-09"

