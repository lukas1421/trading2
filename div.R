
require(rvest)
require(data.table)
require(lubridate)
require(stringr)
require(lubridate)
require(plyr)

print(html_attr(l,"href"))
l3<-print(html_attrs(l))
l2<-html_text(l)%>% (function(x) iconv(x,"utf-8","gb2312"))
l1[grep("查看往日资讯", l1)]

################################# METHODS ####################################
extractDiv <- function(x) {
  if(!is.na(str_match(x, "（([:digit:]{6})）")[[2]])) {
    print("################################################################")
    #print(str_match(x, "(.*)（[:digit:]{6}）")[2])
    #print(str_match(x, "（([:digit:]{6})）")[2])
    #print(str_match(x,"派(.*)(?:\\s+)?元")[2])
    #print(str_match(x, "派息日(\\d{4}-\\d{2}-\\d{2})")[2])  
    #print(str_match(x,"转(.*?)(?:\\s+)?股" )[2])
    #print(str_match(x, "新增股份上市日(?:\\s+)?(\\d{4}-\\d{2}-\\d{2})")[2])
    #print(str_match(x, "（([:digit:]{6})）")[[2]])
    
    chineseName = str_match(x, "(.*).[:digit:]{6}")[2]
    print(chineseName)
    ticker =str_match(x, "（([:digit:]{6})）")[2]
    cashDiv = as.numeric(str_match(x,"派(.*)(?:\\s+)?元")[2])
    cashDivDate = (str_match(x, "派息日(\\d{4}-\\d{2}-\\d{2})")[2])
    stockDiv1 = ifelse(is.na(as.numeric(str_match(x,"[送](.*?)(?:\\s+)?股" )[2])),0,as.numeric(str_match(x,"[送](.*?)(?:\\s+)?股" )[2]))
    stockDiv2 = ifelse(is.na(as.numeric(str_match(x,"[转](.*?)(?:\\s+)?股" )[2])),0,as.numeric(str_match(x,"[转](.*?)(?:\\s+)?股" )[2]))
    stockDivDate = (str_match(x, "新增股份上市日(?:\\s+)?(\\d{4}-\\d{2}-\\d{2})")[2])
    tickerFull = ifelse(str_sub(ticker,1,1)!="6",paste0("sz",ticker), paste0("sh",ticker))
    
    print(tickerFull)
    datePrice = ymd(ifelse(!is.na(stockDivDate), ifelse(!is.na(cashDivDate),ifelse(cashDivDate<stockDivDate,cashDivDate,stockDivDate)
                                                        ,stockDivDate), cashDivDate))
    dateOff <- ifelse(weekdays(datePrice)=="星期一", 3,1)
    lastPrice <- as.numeric(getLastCloseV2(tickerFull,datePrice-dateOff))
    #lastPrice = as.numeric(getLastClose(tickerFull, Sys.Date()-4))
    
    print(lastPrice)
    
    sl <- list(ticker=tickerFull, chineseName=chineseName, cashDiv=cashDiv, cashDivDate=cashDivDate,
               stockDiv=stockDiv1+stockDiv2,stockDivDate=stockDivDate,lastPrice=lastPrice)
    
    #print(sl)
    #print("res")
    #print(res)
    res<<-data.table::rbindlist(list(res,sl),use.names = T,fill = TRUE)
    
    #print(list(ticker=ticker, chineseName=chineseName, cashDiv=cashDiv, cashDivDate=cashDivDate,stockDiv=stockDiv,stockDivDate=stockDivDate))
    #return(list(ticker, chineseName, cashDiv, cashDivDate,stockDiv,stockDivDate))
    return()
  }
}


#start here
url <- getDivURL()
#url <- "http://stock.10jqka.com.cn/20170510/c598501361.shtml"
a<-read_html(url)
l<-html_nodes(a,"p")
l<-html_text(l)%>% (function(x) iconv(x,"utf-8","gb2312"))
l<-l[grep(paste0(Sys.Date()), l)]
l<-str_trim(l)
res <- data.table(ticker="",chineseName="", cashDiv=0, cashDivDate = "", stockDiv=0, stockDivDate="", lastPrice=0)
l_ply(l, extractDiv)

res[, cashDivDate:=ymd(cashDivDate)]
res[, stockDivDate:=ymd(stockDivDate)]
res[is.na(stockDiv), stockDiv:=0]
res[is.na(cashDiv), cashDiv:=0]
res[, adjFactor:= ((lastPrice - cashDiv/10)/(1+as.numeric(stockDiv)/10))/lastPrice]
res[, adjPrice := ((lastPrice - cashDiv/10)/(1+as.numeric(stockDiv)/10))]
res<-res
res<-res[!(!is.na(stockDivDate) & cashDivDate!=Sys.Date())]
res<-res[!is.infinite(adjFactor) & !is.nan(adjFactor)]
res[, inList:=isInStockList(ticker), .(ticker) ]
res<-res[inList==TRUE, ]
write.table(res, paste0(tradingFolder,"div.txt"),quote = FALSE,sep = "\t")


##############################################################
tickerList<- fread(paste0(tradingFolder,"tickerListDiv",".txt"), header=F)
isInStockList <- function(symb) {
  sum(tickerList==symb)>0
}


########get close V2 

getLastCloseV2 <- function(symb,dat) {
  
  ticker <- paste0(toupper(str_sub(symb,1,2)),"#",str_sub(symb,3))
  stock <- data.table()
  tryCatch( 
    {
      stock <- fread(paste0(dayDataFolder,ticker, ".txt"),header = TRUE,skip = 1,fill = T,
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
    


#####################################  GET LAST CLOSE ########################
getLastClose<- function(symb, dat) {
  
  require(rvest)
  require(data.table)
  require(lubridate)
  
  print(paste0(" dat in get close is ",dat))
  
  begin <- format(dat,"%Y%m%d")
  end <- format(Sys.Date(),"%Y%m%d")
  
  url <- paste0("http://biz.finance.sina.com.cn/stock/flash_hq/kline_data.php?symbol=",symb,"&begin_date=",begin,"&end_date=",end)
  a<-read_html(url)
  l<-html_nodes(a,"content")
  #print(l)
  if(length(l)!=0) {
    dt <- data.table(d=ymd(xml_attr(html_nodes(a,"content"),"d")),o=as.numeric(xml_attr(html_nodes(a,"content"),"o")),    
            h=as.numeric(xml_attr(html_nodes(a,"content"),"h")),l=as.numeric(xml_attr(html_nodes(a,"content"),"l")),
                c=as.numeric(xml_attr(html_nodes(a,"content"),"c")),v=as.numeric(xml_attr(html_nodes(a,"content"),"v")))
    print(dt)
    
    if(nrow(dt[d==dat]) >0) {
      return(dt[d==dat, c])
    }
  }
  return(0)
}

################################################ PARSE DIV URL ######################################################
getDivURL <-function() {
  url<-"http://stock.10jqka.com.cn/fhspxx_list/"
  a<-read_html(url,encoding = "gbk")
  l<-html_nodes(a,"a")
  l1<-xml2::xml_attrs(l)
  return((l1[which(lapply(l1,function(x) grep("沪（深）市分红配送信息提示",x))>0)[1]])[[1]] %>% 
           (function(x) x[which(names(x)=="href")])) 
}

d<- fread(paste0(tradingFolder,"tickerListDiv",".txt"), showProgress = TRUE)
d1<-unlist(d)
          