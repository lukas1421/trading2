
#All r update
require(data.table)
require(ggplot2)
require(lubridate)
require(stringr)
require(reshape2)
require(PerformanceAnalytics)
require(quantmod)

if(Sys.getenv("USERNAME")=="LUke") {
  mainDir <- "J:\\Data\\mainBoardR\\"
  dayDataFolder <- "J:\\TDX\\T0002\\export\\"
} else if(Sys.getenv("USERNAME")=="Luke Shi") {
  mainDir <- "H:\\Data\\mainBoardR\\"
  dayDataFolder <-  "G:\\export\\"
}



indexDay <- fread(paste0(dayDataFolder,"SH#000001.txt"),header = TRUE,skip = 1,fill = T,
                  showProgress = TRUE,col.names = c("D","O","H","L","C","V","A"))

indexDay<- indexDay[!.N,]
indexDay[, D:=ymd(D)]


fillData000001 <- function() {
  dest <- paste0(mainDir,"SH000001_2017.csv")
  source <- paste0(minuteDataFolder,"SH#000001.txt")
  #get last date
  destDT <- fread(dest,fill=T, header = F )
  print(destDT)
  lastDateinDest <- ymd(destDT[.N, V1])
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

res <- data.table()
tmp <- data.table()

for(i in 1999:2017) {
  
  #assign(paste0("f",i),fread(paste0(mainDir,"SH000001_", i,".csv")))
  #assign(tmp,fread(paste0(mainDir,"SH000001_", i,".csv")))
  tmp <- fread(paste0(mainDir,"SH000001_", i,".csv"))
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
  res<-rbindlist(list(res,tmp),use.names = TRUE,fill = TRUE)
}
print(res)


res1<-melt.data.table(res, id.vars = c("D","T"))
res2 <- dcast.data.table(res1, D ~ variable+T, sep = "")

for (v in c("O","H","L","C")) {
  res2[is.na(get(paste0(v,1130))), eval(paste0(v,1130)):=get(paste0(v,1129)),]
  res2[is.na(get(paste0(v,1129))), eval(paste0(v,1129)):=get(paste0(v,1128)),]
  res2[is.na(get(paste0(v,1301))), eval(paste0(v,1301)):=get(paste0(v,1302)),]
  res2[is.na(get(paste0(v,1300))), eval(paste0(v,1300)):=get(paste0(v,1301)),]
  res2[is.na(get(paste0(v,1500))), eval(paste0(v,1500)):=get(paste0(v,1459)),]
}



#max min
res2[, dayMax:=max(unlist(mget(paste0("H",tradeTime)))), keyby=list(D)]
res2[, dayMaxT1:=as.numeric(tradeTime[which.max(unlist(mget(paste0("H",tradeTime))))]), keyby=list(D)]
res2[, dayMin:=min(unlist(mget(paste0("L",tradeTime)))), keyby=list(D)]
res2[, dayMinT1:=as.numeric(tradeTime[which.min(unlist(mget(paste0("L",tradeTime))))]), keyby=list(D)]

res2[, amMax:=max(unlist(mget(paste0("H",amTime)))), keyby=list(D)]
res2[, amMaxT1:= as.numeric(amTime[which.max(unlist(mget(paste0("H",amTime))))]), keyby=list(D)]
res2[, amMin:=min(unlist(mget(paste0("L",amTime)))), keyby=list(D)]
res2[, amMinT1:=as.numeric(amTime[which.min(unlist(mget(paste0("L",amTime))))]), keyby=list(D)]

res2[, pmMax:=max(unlist(mget(paste0("H",pmTime)))), keyby=list(D)]
res2[, pmMaxT1:=as.numeric(pmTime[which.max(unlist(mget(paste0("H",pmTime))))]), keyby=list(D)]
res2[, pmMin:=min(unlist(mget(paste0("L",pmTime)))), keyby=list(D)]
res2[, pmMinT1 := as.numeric(pmTime[which.min(unlist(mget(paste0("L",pmTime))))]), keyby=list(D)]


# MERGE ########################################################################################
resMerged <- merge(indexDay,res2,by = "D" )

#resMerged[, weekday:= factor(weekdays(D),levels = c("","???ڶ?","??????","??????","??????"),
#                             labels =c("1","2","3","4","5") )]

resMerged[, range:= log(dayMax/dayMin)]
resMerged[, first10:= log(C940/O931)]
resMerged[, first1:=log(C931/O931)]
resMerged <- merge(indexDay,res2,by = c("D"))
resMerged[, retCL:= log(C/L)]
resMerged[, retLO:= log(L/O)]
resMerged[, retHO:= log(H/O)]
resMerged[, retCH:= log(C/H)]
resMerged[, rangeDay:= log(H/L)]
resMerged[, openPercentile:=(O - dayMin)/(dayMax - dayMin)]
resMerged[, openPercentileY:= shift(openPercentile,1)]
resMerged[, openPercentileYCat:=cut(openPercentileY, quantile(openPercentileY,na.rm = T),include.lowest = T)]
resMerged[, retAM:= shift(C1130/O)]
resMerged[, retPM:= shift(C/O1300)]
resMerged[, retOPC:= log(O/shift(C,1))]
resMerged[is.na(retOPC), retOPC:=0]
resMerged[, retAMHO := log(amMax/O)]
resMerged[, retPMCH := log(C/pmMax)]
resMerged[, percentile:= (C-L)/(H-L)]
resMerged[, percentileY:=shift(percentile,1)]
resMerged[, dayMinY:= shift(dayMin,1)]
resMerged[, retPMClose1315:= log(C/O1315)]
resMerged[, retAMCO:= log(C1130/O)]
resMerged[, retAMCOY:= shift(retAMCO,1)]
resMerged[, retPMCO:=log(C/O1300) ]
resMerged[, retPMCOY:= shift(retPMCO,1)]
resMerged[is.na(retPMCOY), retPMCOY:=0]
resMerged[, retPMCOYCat:= cut(retPMCOY, quantile(retPMCOY,na.rm = T),include.lowest = T)]
resMerged[, retPMCL:= log(C/pmMin)]
resMerged[, retPMHO:= log(pmMax/O1300)]
resMerged[, pmRange:= log(pmMax/pmMin)]
resMerged[, pmclOverPmRange:= retPMCL/pmRange]
resMerged[, pmclOverPmRangeY:=shift(pmclOverPmRange,1)]
resMerged[, pmclOverPmRangeCat:= cut(pmclOverPmRange,quantile(pmclOverPmRange,na.rm = T),include.lowest = T)]
resMerged[, pmclOverPmRangeYCat:= cut(pmclOverPmRangeY,quantile(pmclOverPmRangeY,na.rm = T),include.lowest = T)]
resMerged[, retPMCH:= log(C/pmMax)]
resMerged[, pmHOCHRatio:= (retPMHO-retPMCH)/pmRange]
resMerged[, pmHOCHRatioY:= shift(pmHOCHRatio,1)]
resMerged[, pmHOCHRatioYCat:= cut(pmHOCHRatioY, quantile(pmHOCHRatioY,na.rm = T),include.lowest = T) ]
resMerged[, retPMLO:= log(pmMin/O1300)]
resMerged[, pmLOCLRatio:= (retPMLO-retPMCL)/pmRange]
resMerged[, pmLOCLRatioY:= shift(pmLOCLRatio,1)]
resMerged[, pmLOCLRatioYCat:=cut(pmLOCLRatioY,breaks = quantile(pmLOCLRatioY,na.rm = T),include.lowest = T)]
resMerged[, amCP:= (C1130 - amMin)/(amMax - amMin )]
resMerged[, amCPY:= shift(amCP,1)]
resMerged[,amCPYCat:=cut(amCPY,breaks = quantile(amCPY,na.rm = T),include.lowest = T)]
resMerged[, mean(retPMCO), keyby=list(amCPYCat)]
resMerged[, HOCHRatio:= (retHO - retCH)/rangeDay ]
resMerged[, AMPMRatio:= (retAM-retPM)/rangeDay]
resMerged[, HOCHRatioY:=  shift(HOCHRatio,1)]
resMerged[, AMPMRatioY:= shift(AMPMRatio,1)]
resMerged[, HOCHRatioYCat:= cut(HOCHRatioY,breaks = quantile(HOCHRatioY,na.rm = T),include.lowest = T)]
resMerged[, AMPMRatioYCat:= cut(AMPMRatioY,quantile(AMPMRatioY,na.rm = T),include.lowest = T)]


resMerged[is.na(percentileY), percentileY:= 0.5]
resMerged[, percentileCat:=cut(percentile, breaks = quantile(percentile),include.lowest = T)]
resMerged[, percentileYCat:=cut(percentileY, breaks = quantile(percentileY,na.rm = T),include.lowest = T)]
#resMerged[, weekday:= factor(weekdays(D),levels = c("Monday","Tuesday","Wednesday","Thursday","Friday"),
#                        labels =c("1","2","3","4","5") )]



isAm <- function(x) {
  #print(str_sub(x,2))
  if(is.numeric(as.numeric(str_sub(x,2)))) {
    return(as.numeric(str_sub(x,2))<1200)
  }
  return(FALSE)
}

isPm <- function(x) {
  if(is.numeric(as.numeric(str_sub(x,2)))) {
    return(as.numeric(str_sub(x,2))>1200)
  }
  return(FALSE)
}

assign(paste0("f",1999),fread(paste0(mainDir,"SH000001_", 1999,".csv")))
assign(paste0("f",2000),fread(paste0(mainDir,"SH000001_", 2000,".csv")))
assign(paste0("f",2001),fread(paste0(mainDir,"SH000001_", 2001,".csv")))


data.table::rbindlist(list(f1999,f2000),use.names = T,fill = T)

#Graph
g<-resMerged[,list(Open=O,High=H,Low=L,Close=C),]
g<-xts(g,order.by = resMerged$D)
candleChart(g['20170101/20170801'], theme="white",type="candles")

res[, DT:=ymd_hm(paste(D,paste0(str_sub(T,1,str_length(T)-2),":",str_sub(T,str_length(T)-1))))]
g1<-res[,list(Open=O,High=H,Low=L,Close=C),]
g1 <- xts(g1,order.by = res$DT)
candleChart(g1['20170717/20170719'], theme="white",type="candles")
