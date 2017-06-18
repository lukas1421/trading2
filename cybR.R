
#All r update
require(data.table)
require(ggplot2)
require(lubridate)
require(stringr)
require(reshape2)
require(PerformanceAnalytics)
require(quantmod)
require(httr)

cybDir <- "H:\\Data\\cybR\\"
dayDataFolder <-  "G:\\export\\"

cybDay <- fread( paste0(dayDataFolder,"SZ#399006.txt"),header = TRUE,skip = 1,fill = T,
                 showProgress = TRUE,col.names = c("D","O","H","L","C","V","A"))

cybDay <- cybDay[!.N,]
cybDay[, D:=ymd(D)]

fillData399006<- function() {
  dest <- paste0(cybDir,"SZ399006_2017.csv")
  #source <- "J:\\TDX\\T0002\\export_1m\\SZ#399006.txt"
  source <- paste0(minuteDataFolder,"SZ#399006.txt")
  #get last date
  destDT <- fread(dest,fill=T, header = T )
  print(destDT)
  lastDateinDest <- ymd(destDT[.N, D])
  print(lastDateinDest)
  sourceDT <- fread(source,fill = T,skip = 1,header = T)
  names(sourceDT) <-c("D","T","O","H","L","C","V","A")
  sourceDT <- sourceDT[-.N]
  print(sourceDT)
  sourceDT[, D:=ymd(D)]
  #sourceDT[]
  res <- sourceDT[D>lastDateinDest][, 1:6]
  write.table(res, dest, sep=",",col.names = F,append = T,row.names = F)
  return(res)
}



resCyb <- data.table()
tmp <- data.table()

for(i in 2010:2017) {
  
  #assign(paste0("f",i),fread(paste0(mainDir,"SH000001_", i,".csv")))
  #assign(tmp,fread(paste0(mainDir,"SH000001_", i,".csv")))
  tmp <- fread(paste0(cybDir,"SZ399006_", i,".csv"))
  #tmp <- get(paste0("f",i))
  if(length(names(tmp)) > 6) {
    tmp[, names(tmp)[7:length(names(tmp))]:=NULL]
  }
  names(tmp) <- c("D","T","O","H","L","C")
  print(is.data.table(tmp))
  tmp[,D:=ymd(D)]
  tmp[ ,T:= str_replace(T,":",""),]
  tmp[, T:=ifelse(str_sub(T,1,1)=="0", str_sub(T,2),T),]
  tmp[,T:=as.numeric(T)]
  assign(paste0("f",i),tmp)
  resCyb<-rbindlist(list(resCyb,tmp),use.names = TRUE,fill = TRUE)
}

print(resCyb)


resCyb1<-melt.data.table(resCyb, id.vars = c("D","T"))
resCyb2 <- dcast.data.table(resCyb1, D ~ variable+T, sep = "")


for (v in c("O","H","L","C")) {
  resCyb2[is.na(get(paste0(v,1130))), eval(paste0(v,1130)):=get(paste0(v,1129)),]
  resCyb2[is.na(get(paste0(v,1129))), eval(paste0(v,1129)):=get(paste0(v,1128)),]
  resCyb2[is.na(get(paste0(v,1301))), eval(paste0(v,1301)):=get(paste0(v,1302)),]
  resCyb2[is.na(get(paste0(v,1300))), eval(paste0(v,1300)):=get(paste0(v,1301)),]
  resCyb2[is.na(get(paste0(v,1500))), eval(paste0(v,1500)):=get(paste0(v,1459)),]
}

#max min
resCyb2[, dayMax:=max(unlist(mget(paste0("H",tradeTime)))), keyby=list(D)]
resCyb2[, dayMaxT1:=as.numeric(tradeTime[which.max(unlist(mget(paste0("H",tradeTime))))]), keyby=list(D)]
resCyb2[, dayMin:=min(unlist(mget(paste0("L",tradeTime)))), keyby=list(D)]
resCyb2[, dayMinT1:=as.numeric(tradeTime[which.min(unlist(mget(paste0("L",tradeTime))))]), keyby=list(D)]

resCyb2[, amMax:=max(unlist(mget(paste0("H",amTime)))), keyby=list(D)]
resCyb2[, amMaxT1:= as.numeric(amTime[which.max(unlist(mget(paste0("H",amTime))))]), keyby=list(D)]
resCyb2[, amMin:=min(unlist(mget(paste0("L",amTime)))), keyby=list(D)]
resCyb2[, amMinT1:=as.numeric(amTime[which.min(unlist(mget(paste0("L",amTime))))]), keyby=list(D)]

resCyb2[, pmMax:=max(unlist(mget(paste0("H",pmTime)))), keyby=list(D)]
resCyb2[, pmMaxT1:=as.numeric(pmTime[which.max(unlist(mget(paste0("H",pmTime))))]), keyby=list(D)]
resCyb2[, pmMin:=min(unlist(mget(paste0("L",pmTime)))), keyby=list(D)]
resCyb2[, pmMinT1 := as.numeric(pmTime[which.min(unlist(mget(paste0("L",pmTime))))]), keyby=list(D)]


cybMerged <- merge(cybDay,resCyb2,by = "D" )

cybMerged[, weekday:= factor(weekdays(D),levels = c("星期一","星期二","星期三","星期四","星期五"),
                             labels =c("1","2","3","4","5") )]

cybMerged[, range:= log(dayMax/dayMin)]
cybMerged[, first10:= log(C940/O931)]
cybMerged[, first1:=log(C931/O931)]
#cybMerged <- merge(cybDay,resCyb2,by = c("D"))
cybMerged[, retCL:= log(C/L)]
cybMerged[, retLO:= log(L/O)]
cybMerged[, retHO:= log(H/O)]
cybMerged[, retHOY:= shift(retHO,1)]
cybMerged[, retCH:= log(C/H)]
cybMerged[, retCHY:= shift(retCH,1)]

cybMerged[, rangeDay:= log(H/L)]

cybMerged[, openPercentile:=(O - dayMin)/(dayMax - dayMin)]
cybMerged[, openPercentileY:= shift(openPercentile,1)]
cybMerged[, openPercentileYCat:=cut(openPercentileY, quantile(openPercentileY,na.rm = T),include.lowest = T)]
cybMerged[, retAM:= log(C1130/O)]
cybMerged[, retPM:= log(C/O1300)]
cybMerged[, retCO:= log(C/O)]
cybMerged[, retOPC:= log(O/shift(C,1))]
cybMerged[is.na(retOPC), retOPC:=0]
cybMerged[, retAMHO := log(amMax/O)]
cybMerged[, retPMCH := log(C/pmMax)]

cybMerged[, percentile:= (C-L)/(H-L)]
cybMerged[, percentileY:=shift(percentile,1)]
cybMerged[, percentileYCat:=cut(percentileY,quantile(percentileY,na.rm = T),include.lowest = T)]
cybMerged[, dayMinY:= shift(dayMin,1)]
cybMerged[, retPMClose1315:= log(C/O1315)]

cybMerged[, retAMCO:= log(C1130/O)]
cybMerged[, retAMCOY:= shift(retAMCO,1)]
cybMerged[, retPMCO:=log(C/O1300) ]
cybMerged[, retPMCOY:= shift(retPMCO,1)]
cybMerged[, retCOY:= shift(retCO,1)]

cybMerged[is.na(retPMCOY), retPMCOY:=0]
cybMerged[, retPMCOYCat:= cut(retPMCOY, quantile(retPMCOY,na.rm = T),include.lowest = T)]
cybMerged[, retPMCL:= log(C/pmMin)]
cybMerged[, retPMHO:= log(pmMax/O1300)]
cybMerged[, pmRange:= log(pmMax/pmMin)]
cybMerged[, pmclOverPmRange:= retPMCL/pmRange]
cybMerged[, pmclOverPmRangeY:=shift(pmclOverPmRange,1)]
cybMerged[, pmclOverPmRangeCat:= cut(pmclOverPmRange,quantile(pmclOverPmRange,na.rm = T),include.lowest = T)]
cybMerged[, pmclOverPmRangeYCat:= cut(pmclOverPmRangeY,quantile(pmclOverPmRangeY,na.rm = T),include.lowest = T)]
cybMerged[, retPMCH:= log(C/pmMax)]
cybMerged[, pmHOCHRatio:= (retPMHO-retPMCH)/pmRange]
cybMerged[, pmHOCHRatioY:= shift(pmHOCHRatio,1)]
cybMerged[, pmHOCHRatioYCat:= cut(pmHOCHRatioY, quantile(pmHOCHRatioY,na.rm = T),include.lowest = T) ]
cybMerged[, retPMLO:= log(pmMin/O1300)]
cybMerged[, pmLOCLRatio:= (retPMLO-retPMCL)/pmRange]
cybMerged[, pmLOCLRatioY:= shift(pmLOCLRatio,1)]
cybMerged[, pmLOCLRatioYCat:=cut(pmLOCLRatioY,breaks = quantile(pmLOCLRatioY,na.rm = T),include.lowest = T)]
cybMerged[, amCP:= (C1130 - amMin)/(amMax - amMin )]
cybMerged[, amCPY:= shift(amCP,1)]
cybMerged[,amCPYCat:=cut(amCPY,breaks = quantile(amCPY,na.rm = T),include.lowest = T)]
cybMerged[, mean(retPMCO), keyby=list(amCPYCat)]
cybMerged[, HOCHRatio:= (retHO - retCH)/rangeDay ]
cybMerged[, AMPMRatio:= (retAM-retPM)/rangeDay]
cybMerged[, HOCHRatioY:=  shift(HOCHRatio,1)]
cybMerged[, AMPMRatioY:= shift(AMPMRatio,1)]
cybMerged[, HOCHRatioYCat:= cut(HOCHRatioY,breaks = quantile(HOCHRatioY,na.rm = T),include.lowest = T)]
cybMerged[, AMPMRatioYCat:= cut(AMPMRatioY,quantile(AMPMRatioY,na.rm = T),include.lowest = T)]




# daily graph bars
cybGraph<-cybDay[,list(Open=O,High=H,Low=L,Close=C),]
cybGraph <- xts(cybGraph,order.by = cybDay$D)
candleChart(cybGraph['20170101/20190101'], theme="white",type="candles")

#Detailed Graph CYB
resCyb[, DT:=ymd_hm(paste(D,paste0(str_sub(T,1,str_length(T)-2),":",str_sub(T,str_length(T)-1))))]
cybGraphD<-resCyb[,list(Open=O,High=H,Low=L,Close=C),]
cybGraphD <- xts(cybGraphD,order.by = resCyb$DT)
candleChart(cybGraphD['20170612/20170613'], theme="white",type="candles")



res <- data.table()
tmp <- data.table()
