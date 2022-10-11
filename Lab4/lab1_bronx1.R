library(gdata) 
#faster xls reader but requires perl!
#bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="<SOMEWHERE>/perl/bin/perl.exe") 
#bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
library("readxl")
?read_excel
bronx1<-read_excel(file.choose(), skip=4,col_names =TRUE)
colnames(bronx1) = c(
  "BOROUGH",
  "NEIGHBORHOOD",
  "BUILDING.CLASS.CATEGORY",
  "TAX.CLASS.AT.PRESENT",
  "BLOCK",
  "LOT",
  "EASEMENT",
  "BUILDING.CLASS.AT.PRESENT",
  "ADDRESS",
  "APARTMENT.NUMBER",
  "ZIP.CODE",
  "RESIDENTIAL.UNITS",
  "COMMERCIAL.UNITS",
  "TOTAL.UNITS",
  "LAND.SQUARE.FEET",
  "GROSS.SQUARE.FEET",
  "YEAR.BUILT",
  "TAX.CLASS.AT.TIME.OF.SALE",
  "BUILDING.CLASS.AT.TIME.OF.SALE",
  "SALE.PRICE",
  "SALE.DATE"
)
View(bronx1)

attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE))
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
GROSS.SQUARE.FEET[GROSS.SQUARE.FEET == 0] = 1
LAND.SQUARE.FEET[LAND.SQUARE.FEET == 0] = 1
SALE.PRICE[SALE.PRICE == 0] = 1
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

m2<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#
