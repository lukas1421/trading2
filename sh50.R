

# sh50 analysis

#get last week return and sharpe
res<- fread("C:\\Users\\LUke\\Desktop\\Trading\\sh50.txt",header = FALSE)

getLastWeekSharpe <- function(symb)  {
  d <- getDataPure(symb)
  d[, cc:= C/shift(C,1)-1]
  #d <- d[D>=ymd("20170717")]
  m <- d[D>=ymd("20170717"), mean(cc)]
  s <- d[D>=ymd("20170717"), sd(cc)]
  return(list(sr=m/s))
  
}

computeLastWeekSharpeAll <- function() {
  d<-fread("C:\\Users\\LUke\\Desktop\\Trading\\sh50.txt",header = FALSE)
  d<- d[,c(V2,getLastWeekSharpe(V1)),keyby=list(V1)]
  return(d)
}