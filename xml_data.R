
#

library(XML)
library(xml2)
html <- "http://biz.finance.sina.com.cn/stock/flash_hq/kline_data.php?symbol=sh603569&begin_date=19990101&end_date=20161115"

data<-xmlParse(html)
#a<-xmlRoot(a)
a<-xmlToList(xmlRoot(data))
a<-data.table(t(sapply(a, c)))
a[, d:= ymd(d)]
a[, bl:= NULL]

a[, retCH:=log(c/h)]
a[, retCL:= log(c/l)]
a[, retCHY:= shift(retCH)]
a[, retCLY:= shift(retCL)]
a[is.na(retCHY),retCHY:=0]
a[is.na(retCLY),retCLY:=0]
a[,retCHYCat:=cut(retCHY,quantile(retCHY),include.lowest = T)]
a[,retCLYCat:=cut(retCLY,quantile(retCLY),include.lowest = T)]
a[,retCHCLYSum := retCHY+retCLY]
a[,retCHCLYSumCat:= cut(retCHCLYSum,quantile(retCHCLYSum),include.lowest = T)]
a[,retCHYOverRetCLY:=abs(retCHY/retCLY)]
a[is.na(retCHYOverRetCLY),retCHYOverRetCLY:=0]
a[,retCHYOverRetCLYCat:=cut(retCHYOverRetCLY,quantile(retCHYOverRetCLY),include.lowest = T)]
a[,retCO:= log(c/o)]
a[,retCC:=log(c/shift(c,1))]
a[is.na(retCC),retCC:=0]
a[,percentile:=(c-l)/(h-l)]
a[, percentileY:=shift(percentile,1)]
a[is.na(percentileY),percentileY:=0]
a[,percentileYCat:=cut(percentileY,quantile(percentileY),include.lowest = T)]
a[,weekday:=wday(d)-1]
a[,weekday:=factor(weekday)]
a[,cy:= shift(c,1)]
a[,retOPC:=log(o/cy)]
a[is.na(cy),cy:=c]
a[is.na(retOPC),retOPC:=0]

