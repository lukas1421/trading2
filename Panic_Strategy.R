# panic strategy

require(xts)
require(zoo)
require(data.table)
require(lubridate)
require(quantmod)
require(ggplot2)
require(stringr)

minuteDataFolder <- "J:\\TDX\\T0002\\export_1m\\"
#minuteDataFolder <- "G:\\export_1m\\"

getDataYtdToday <- function(symb) {
  print(paste0(" getting ",symb))
  ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
  d<- fread(paste0(minuteDataFolder,ticker,".txt"),skip = 1,fill = T,showProgress = TRUE,col.names = c("D","T","O","H","L","C","V","A"))
  d <- d[!.N,]
  d[, D:=ymd(D)]
  return(d[,list(D,T,O,H,L,C)])
}

#Graph

d[, DT:=ymd_hm(paste(D,paste0(str_sub(T,1,str_length(T)-2),":",str_sub(T,str_length(T)-1))))]
g1<-d[,list(Open=O,High=H,Low=L,Close=C),]
g1 <- xts(g1,order.by = d$DT)
candleChart(g1['20170508/20170518'], theme="white",type="candles")

graphThisWeek <- function(symb) {
  
  d <- getDataYtdToday(symb)
  d[, DT:=ymd_hm(paste(D,paste0(str_sub(T,1,str_length(T)-2),":",str_sub(T,str_length(T)-1))))]
  g1<-d[,list(Open=O,High=H,Low=L,Close=C),]
  g1 <- xts(g1,order.by = d$DT)
  candleChart(g1['20170508/20170512'], theme="white",type="candles")
  return(g1)
}


getPanicReturn <- function(symb) {
  d<-getDataYtdToday(symb)
  d[, runningMax:= cummax(H),by= list(D)]
  d[, runningMin:= cummin(L),by= list(D)]
  d[, openDay:=  O[T==931],by=list(D)]
  d[, closeDay:=  C[T==1500],by=list(D)]
  #d[, runningPerc:= ( C-runningMin)/(runningMax-runningMin), by=list(D)]
  d[, runningPerc:= ( C-runningMin)/(runningMax-runningMin), ]
  d[, ho:= log(runningMax/openDay) , by=list(D)]
  d[, amho:=ifelse(T<1200, log(runningMax/openDay), log(runningMax[T==1129]/openDay)), by=list(D)]
  d[, returnToClose:= log(C[T==1500]/C), by=list(D)]
}


checkPanicAvailable <- function() {
  
  
}

getMondayOfWeek <- function(dat) {
  require(lubridate)
  weekdayl <- list("星期一"=1,"星期二"=2,"星期三"=3,"星期四"=4,"星期五"=5,"星期六"=6,"星期日"=7 )
  if(is.Date(dat)) {
    return(dat-weekdayl[[weekdays(dat)]]+1)
  }
  return(dat)
}

#
getFridaySharpe <- function(symb) {
  #symb <- "sz002032"
  d<- getData(symb)
  d <- d[L>0,]
  d[, weekday:=factor(weekdays(D),levels=c("星期一","星期二","星期三","星期四","星期五"),labels=c("1","2","3","4","5"))]
  d[, mon:=getMondayOfWeek(D), keyby=list(D)]
  d[, maxWeek:=cummax(H), keyby=list(mon)]
  d[, minWeek:=cummin(L), keyby=list(mon)]
  d[, percWeek:= (C - minWeek)/( maxWeek-minWeek)]
  d[, percWeekY:=shift(percWeek,1)]
  d[weekday=="5", calcSharp(log(C/O)), keyby=list(cut(percWeekY,quantile(percWeekY,na.rm = T),include.lowest = T))]
}

tickerList<- fread(paste0(tradingFolder,"test.txt"),header = FALSE)

getLastCl <- function(symb) {
  print(symb)
  #folder <-  "J:\\TDX\\T0002\\export\\"
  ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
  d<- fread(paste0(dayDataFolder,ticker,".txt"),skip = 1,fill = T,showProgress = TRUE,col.names = c("D","O","H","L","C","V","A"))
  
  return(d[.N-1, log(C/L)])
}

getLastClGen <- function(symb) {
  print(symb)
  #folder <-  "J:\\TDX\\T0002\\export\\"
  ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
  d<- fread(paste0(dayDataFolder,ticker,".txt"),skip = 1,fill = T,showProgress = TRUE,col.names = c("D","O","H","L","C","V","A"))
  
  closeP <- d[.N-2, list(closeP=(C-L)/(H-L))]
  
  l1 <- d[.N-1, list(cl=log(C/L),lo=log(L/O), range = log(H/L), CLLORange= (log(C/L)-log(L/O))/(log(H/L)))]
  
  l2 <- as.list(c(closeP,l1))
  print(length(l2))
  
  return(as.list(c(closeP,l1)))
}



getLastAll <- function() {
  
  ticker<- fread(paste0(tradingFolder,"test.txt"),header = FALSE)
  t<-ticker[, getLastClGen(V1), keyb=list(V1)]
  return(t)
}




