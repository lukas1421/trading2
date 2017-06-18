#ma 20 
library(stringr)
library(data.table)
library(lubridate)
library(zoo)

getMA <- function(symb,numDays) {
  label <- paste0("ma", deparse(substitute(numDays)))
  #print(label)
  dt<-getDataPure(symb)
  #z <- as.zoo(dt)
  dt[, eval(label):=rollmean(C,numDays,fill = NA, align = "right")]
  print(dt[.N,get(eval(label))])
  return(dt[.N, get(eval(label))])
}

getMAAll <- function(numDays) {
  res<- fread(paste0(tradingFolder,"test.txt"),header = FALSE)
  res<- res[,(getMA(V1,numDays)),keyby=list(V1)]
  write.table(res, paste0(tradingFolder,"ma20.txt"),quote = FALSE,sep = "\t", row.names = FALSE,col.names = FALSE)
  res
}

