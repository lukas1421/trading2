#wtd reversal
require(quantmod)

calcWtd <- function(symb,dateStr) {
  #symb<-"sh000016"
  d<- getDataPure(symb)
  d<-d[D>dateStr]
  d[, weekday:=factor(weekdays(D),levels=c("星期一","星期二","星期三","星期四","星期五"),labels=c("1","2","3","4","5"))]
  d[, ret:= ifelse(shift(C,1)!=0.0,C/shift(C,1)-1,0)]
  d[!is.na(ret), calcSharp(ret), keyby=list(weekday)]
  d[, mondayOfWeek:=getMondayOfWeek(D), keyby=list(D)]
  d[, weekMax:= cummax(H), .(mondayOfWeek)]
  d[, weekMin:= cummin(L), .(mondayOfWeek)]
  d[,.(D,weekday,mondayOfWeek),.(mondayOfWeek)]
  d[,.(D,weekday,mondayOfWeek,.I,rleid(.I), .N, rleid(.I)==1, rleid(.I)==.N),.(mondayOfWeek)]
  d[, weekOpen:= O[rleid(.I)==1] , .(mondayOfWeek)]
  d[, weekClose:= C[rleid(.I)==.N], .(mondayOfWeek)]
  d[, countInWeek:= rleid(.I), .(mondayOfWeek)]
  d[, dayTotalInWeek:= .N, .(mondayOfWeek)]
  d[, wtdPercentile:= (C - weekMin)/( weekMax - weekMin)]
  d[, wtdPercentileY:= shift(wtdPercentile,1)]
  d[, wtdPercentileYCat:=cut(wtdPercentileY, breaks = quantile(wtdPercentileY,na.rm = T),include.lowest = T)]
  d[, CC:=C/shift(C,1)-1]
  print(d[, calcSharp(CC), keyby=list(weekday,wtdPercentileYCat)])
  

  ### week graph
  dWeek<-d[countInWeek==dayTotalInWeek, ]
  g<-dWeek[,list(Open=weekOpen,High=weekMax,Low=weekMin,Close=weekClose),]
  g<-xts(g,order.by = dWeek$D)
  candleChart(g['20130501/20190101'], theme="white",type="candles")
  ###
}



