require(rvest)
require(data.table)
require(lubridate)
require(stringr)
require(lubridate)
require(plyr)


divText<- fread(paste0(tradingFolder,"divRaw1.txt"),header = TRUE)


divText<- fread(paste0(tradingFolder,"divCSV.csv"),header = TRUE)
names(divText) <- c("ticker","chineseName","divs")
divText[, ticker:=str_pad(ticker,width = 6,side = "left",pad = "0") ]
divText[, ticker:=ifelse(str_sub(ticker,1,1)=="6", paste0("sh",ticker),paste0("sz",ticker) )]

res<-divText[, c(chineseName,extractDiv1(ticker,divs)), by=.(ticker)]

res[, adjFactor:= ((lastPrice - cashDiv/10)/(1+as.numeric(stockDiv)/10))/lastPrice]

res[, adjPrice := ((lastPrice - cashDiv/10)/(1+as.numeric(stockDiv)/10))]

res<-res[!(!is.na(stockDivDate) & cashDivDate!=Sys.Date())]
res<-res[!is.infinite(adjFactor) & !is.nan(adjFactor)]

res[, inList:=isInStockList(ticker), .(ticker) ]

res<-res[inList==TRUE, ]

write.table(res, paste0(tradingFolder,"div.txt"),quote = FALSE,sep = "\t")

extractDiv1 <- function(tickerFull,x) {
  date <- Sys.Date()
  cashDiv = ifelse(is.na(as.numeric(str_match(x,"派(.*)(?:\\s+)?元")[2])),0,as.numeric(str_match(x,"派(.*)(?:\\s+)?元")[2]))
  #cashDivDate = (str_match(x, "派息日(\\d{4}-\\d{2}-\\d{2})")[2])
  stockDiv1 = ifelse(is.na(as.numeric(str_match(x,"[送](.*?)(?:\\s+)?股" )[2])),0,as.numeric(str_match(x,"[送](.*?)(?:\\s+)?股" )[2]))
  stockDiv2 = ifelse(is.na(as.numeric(str_match(x,"[转](.*?)(?:\\s+)?股" )[2])),0,as.numeric(str_match(x,"[转](.*?)(?:\\s+)?股" )[2]))
  dateOff <- ifelse(weekdays(date)=="星期一", 3,1)
  lastPrice <- as.numeric(getLastCloseV3(tickerFull,date-dateOff))
  return(list(cashDiv=cashDiv, cashDivDate=date,  stockDiv=stockDiv1+stockDiv2, stockDivDate = date, lastPrice=lastPrice ))
}

getLastCloseV3 <- function(symb,dat) {
  ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
  stock <- data.table()
  tryCatch( 
    {
      stock <- fread(paste0(dataFolder,ticker, ".txt"),header = TRUE,skip = 1,fill = T,
                     showProgress = TRUE,col.names = c("D","O","H","L","C","V","A"))
      stock <- stock [!.N,]
      stock [, D:=ymd(D)]
      return(stock[D<=dat, ][.N,C])
    }, error = function(err) {
      print(err)
      stock <- 0.0
      return(0.0)
    })
}
