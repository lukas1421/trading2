# 000016 kiyodo analysis


#Constituents and weights
library(data.table)
d<-fread(paste0(tradingFolder,"bigCap.txt"),header = T)
d[, sum(Weight), keyby=list(Industry)][order(-V1)]
d[, c("y1","y2","y3","y4") :=getReturn(Ticker), by=list(Ticker)]
d[, y1All:=sum(Weight*y1/100,na.rm = T)]
d[, y2All:=sum(Weight*y2/100,na.rm = T)]
d[, y3All:=sum(Weight*y3/100,na.rm = T)]
d[, y4All:=sum(Weight*y4/100,na.rm = T)]

d[, y1Contrib:=Weight*y1/y1All]
d[, y2Contrib:=Weight*y2/y2All]
d[, y3Contrib:=Weight*y3/y3All]
d[, y4Contrib:=Weight*y4/y4All]

d[order(-Weight)]

d[, sum(y1Contrib), by=list(Industry)][order(-V1)]
d[, sum(y2Contrib), by=list(Industry)][order(-V1)]
d[, sum(y3Contrib), by=list(Industry)][order(-V1)]
d[, sum(y4Contrib), by=list(Industry)][order(-V1)]

d[, totalContrib:=(y1Contrib+y2Contrib+y3Contrib+y4Contrib)]
d[, sum(totalContrib), by=list(Industry)][order(-V1)]
d[order(-totalContrib)][,.(Ticker,Name,Industry,totalContrib)]









getReturn <- function(symb) {
  data <- getDataPure(paste0("sh",symb))
  data[, cc:=round((C/shift(C,1)-1)*10000)/100]
  print(data)
  print(Sys.Date()-1)
  #data
  return(list(y1=data[D==Sys.Date()-1, cc],y2=data[D==Sys.Date()-2, cc]
              ,y3=data[D==Sys.Date()-3, cc],y4=data[D==Sys.Date()-4, cc]  ))
}


