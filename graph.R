#graph

d<-latest[, list(Open,High,Low,Close)]
d <- xts(d, order.by = latest$Date)
candleChart(d,theme="white",type="candles")
candleChart(d['2016-11-28/2017-1-1'],theme="white",type="candles")

#2016
d2016 <- fread("f:/Data/SH000001_20160104_20161202.csv")
d2016[, newT:= str_c(str_sub(V2,1,str_length(V2)-2),":",str_sub(V2,str_length(V2)-2+1,str_length(V2)))]
d2016[, newD:= str_c(V1," ", newT)]
d2016[, parseD := ymd_hm(newD)]

d2016[, names(d2016)[3] <- "Open"]
d2016[, names(d2016)[4] <- "High"]
d2016[, names(d2016)[5] <- "Low"]
d2016[, names(d2016)[6] <- "Close"]

e<- xts(d2016[, list(Open,High,Low,Close)], order.by= d2016$parseD)
candleChart(e['2016-06-13/2016-06-18'], theme="white",type="candles")

 #2015
d2015 <- fread("f:/Data/SH000001_2015.csv")
d2015[, newT:= str_c(str_sub(Time,1,str_length(Time)-2),":",str_sub(Time,str_length(Time)-2+1,str_length(Time)))]
d2015[, newD:= str_c(Date," ", newT)]
d2015[, parseD := ymd_hm(newD)]

d2015[, names(d2016)[3] <- "Open"]
d2015[, names(d2016)[4] <- "High"]
d2015[, names(d2016)[5] <- "Low"]
d2015[, names(d2016)[6] <- "Close"]

e2015<- xts(d2015[, list(Open,High,Low,Close)], order.by= d2015$parseD)
candleChart(e2015['2015-09-01/2015-09-07'], theme="white",type="candles")

# key methods
latest[, weekMinN12345:= c("fri","thur","wed","tue","mon")[which.min(list(dayMin,dayMinY,dayMinY2,dayMinY3,dayMinY4))],keyby=list(Date)]
latest[, weekMaxN12345:= c("fri","thur","wed","tue","mon")[which.max(list(dayMax,dayMaxY,dayMaxY2,dayMaxY3,dayMaxY4))],keyby=list(Date)]
latest[, weekMinN12345N:= factor(weekMinN12345, levels  = c("mon","tue","wed","thur","fri"))]
latest[, weekMaxN12345N:= factor(weekMaxN12345, levels = c("mon","tue","wed","thur","fri"))]

latest[, weekMinN1234:= c("thur","wed","tue","mon")[which.min(list(dayMinY,dayMinY2,dayMinY3,dayMinY4))],keyby=list(Date)]
latest[, weekMaxN1234:= c("thur","wed","tue","mon")[which.max(list(dayMaxY,dayMaxY2,dayMaxY3,dayMaxY4))],keyby=list(Date)]
latest[, weekMinN1234N:= factor(weekMinN1234, levels  = c("mon","tue","wed","thur"))]
latest[, weekMaxN1234N:= factor(weekMaxN1234, levels = c("mon","tue","wed","thur"))]


#candle charting
candleChart(e2015['2015-07-01/2015-07-03'], theme="white",type="candles")
