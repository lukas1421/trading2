

userName <- Sys.getenv("USERNAME")
#tradingFolder <- "c:"

getFTSEData <- function() {
  
  download.file("https://www.ftse.com/analytics/factsheets/Home/DownloadConstituentsWeights/?indexdetails=XINA50"
                , destfile = paste0(tradingFolder,"res.pdf"), mode="wb")
  
  toc <- pdf_text(paste0(tradingFolder,"res.pdf"))
  
  #print(toc)
  t1<-str_split(toc[1],"\n")  

  res <- data.table(rep(0,50),rep(0,50))
  names(res) <- c("stock","weight")

  j<-1

  for(i in seq(7,24,1)) {
    lapply(str_split(str_trim(str_split(t1[[1]][i],"CHINA")[[1]],"right"), "\\s\\s+"), 
         
         function(x) {
           
           if(str_trim(x, side="left")[[1]] !="") {
              if(length(str_trim(x, side="left")) == 2) {
                
                #print(str_trim(x, side="left")[[1]])
                #print(str_replace(str_trim(x, side="left")[[1]],"\\(.*\\)",""))
                #print(str_trim(x, side="left")[[1]][1])
                #print(str_trim(x, side="left")[[2]][1])
                #print(i)
                res$stock[j] <<- str_trim(toupper(str_replace(str_trim(x, side="left")[[1]],"\\(.*\\)","")))
                res$weight[j] <<- as.numeric(str_trim(x, side="left")[[2]])
                j <<- j+1
              } else if (length(str_trim(x, side="left")) == 3) {
                #print(str_trim(x, side="left"))
                res$stock[j] <<- str_trim(toupper(str_replace(str_trim(x, side="left")[2],"\\(.*\\)","")))
                res$weight[j] <<- as.numeric(str_trim(x, side="left")[3])
                j <<- j+1
              }
              #res$stock[i-6] <- str_trim(x, side="left")[1]
              #res$weight[i-6] <- str_trim(x, side="left")[2]
            }
         })
    }
    #print(res)
    #return(res)
  
    wb <- loadWorkbook(paste0(tradingFolder,"new.xlsx"),create = TRUE)
    createSheet(wb,"Sheet1")
    writeWorksheet(wb,res,"Sheet1",startRow = 1,startCol = 1, header = T)
    saveWorkbook(wb)
}

updateFTSEWeights <- function() {
  res <- getFTSEData()
  wb <- loadWorkbook(paste0("C:\\Users\\",Sys.getenv("USERNAME"),"\\Desktop\\Trading\\new.xlsx"),create = TRUE)
  createSheet(wb,"Sheet1")
  writeWorksheet(wb,res,"Sheet1",startRow = 1,startCol = 1, header = T)
  saveWorkbook(wb)
}

#################################################################################################################################################################################

# getting NAVs

getNAV <- function() {

  stocks <- c("2823:HK","2822:HK", "3147:HK", "3188:HK", "FXI:US","CNXT:US","ASHR:US","ASHS:US")
  require(rvest)
  require(stringr)
  
  for(i in stocks) {
    print(i)
    #a <- read_html(httr::GET(paste("https://www.bloomberg.com/quote/",i, sep=""),use_proxy("127.0.0.1",1080)))
    a <- read_html(httr::GET(paste("https://www.bloomberg.com/quote/",i, sep="")))
    b<-html_nodes(a,"div") %>% html_text() %>% (function(x) {x[str_sub(str_trim(x),1,3) == "NAV"]})
    b<-(stringr::str_split(b[[1]],"\\s\\s+"))
    c<-as.numeric(str_match(html_nodes(a,"meta")[str_detect(html_nodes(a,"meta"),"price")][1],"[[:digit:].]+"))
    d<-as.numeric(b[[1]][3])
    print(paste('price',c,'NAV',d,'prem/disc',sprintf("%.2f%%", 100*round(10000*(c/d-1))/10000)))   
  }
}
########################################################################################################################################################################

#getting ftse A50 index
getFTSE50Index <- function (){
  a <- read_html("https://hk.investing.com/indices/ftse-china-a50")
  a<- (html_nodes(a,"td") %>% (function(x) {x[str_detect(x,".*28930.*")]}) %>% html_text())
  a <- as.numeric(str_replace(a,"[,|%]",""))
  #return(c(a[1],a[2],a[1]-a[2], log(a[1]/(a[1]-a[2])),a[3]))
  
  return(c(a[1],a[2],a[1]-a[2],sprintf("%.2f%%", 100*log(a[1]/(a[1]-a[2]))))) 
}

print(getFTSE50Index())

########################################################################################################################################################################

#XIN0U
getXIN0UIndex <- function() {
a <- read_html("http://finance.yahoo.com/quote/XIN0UN.FGI?ltr=1")
price <-  html_nodes(a,"span") %>% (function(x) {x[str_detect(x,"36px.*4px")]}) %>% html_text()
chg <- html_nodes(a,"span") %>% (function(x) {x[str_detect(x,"10px.*24px")]}) %>% html_text()
chg <- str_match(chg, "^[+-][:digit:]+\\.[:digit:]+")
#print(chg)
price <- as.numeric(str_replace(price,"[,|%]",""))
yest <- as.numeric(price)-ifelse(str_sub(chg,1,1)=="+", 1,-1)*abs(as.numeric(chg))
return(c(price, chg[1],yest,sprintf("%.2f%%",100*log(price/yest))))
}
print(getXIN0UIndex())

########################################################################################################################################################################

#get Index 

getIndicies <- function() {
  indices <- c("sh000001","sz399006","sh000300","sh000905",'sh000016')
  for (i in indices) {
    #print(guess_encoding())
    a <- read_html(x = paste0("http://hq.sinajs.cn/list=",i))
    a <- a %>% html_text() 
    a <- str_split(a, ",")
    print(paste(i, ": ",a[[1]][4], " Previous " , a[[1]][3], " ch ", sprintf("%.2f%%",100*log(as.numeric(a[[1]][4])/as.numeric(a[[1]][3])))))
      #read_html()
  }
  #return("")
}
getIndicies()
##########################################################################################################################################

#daily shcomp

getSHCOMP <- function() {
  
require(XLConnect)
#minuteData <- fread("C:\\Users\\Administrator\\Desktop\\Trading\\minuteData.csv")

AMOPENT = 931
AMCLOSET = 1129
PMOPENT = 1300
CLOSET = 1500

#minuteData <- fread("C:\\Users\\LUke\\Desktop\\Trading\\shcompDate.txt")

minuteData <- fread("J:\\TDX\\T0002\\export_1m\\SH#000001.txt", skip = 1, fill=T, showProgress = T, col.names =c("D","T", "O","H","L","C","V","A") )
minuteData <- minuteData[!.N,]
minuteData[, D:=ymd(D)]
minuteData<-minuteData[D==ymd("2017-5-12"),list(D,T,O,H,L,C)]

res <- minuteData[, list("AmOpen"=O[T==AMOPENT], "931"=C[T==AMOPENT], "935"=C[T==935], "940"=C[T==940], "AmClose"=C[T==AMCLOSET], "AmMax" = max(H[T<1200]), "AmMin"=min(L[T<1200]),
             "AmMaxT" = T[T<1200][which.max(H[T<1200])], "AmMinT" = T[T<1200][which.min(L[T<1200])], "PmOpen"=O[T==PMOPENT], "Pm1310"=C[T==1310],"PmClose"=C[T==1459], 
             "PmMax" = max(H[T>1259]), "PmMin"=min(L[T>1259]), "PmMaxT" = T[T>1259][which.max(H[T>1259])], "PmMinT" = T[T>1259][which.min(L[T>1259])]
             ), ]

#minuteData[, dayHigh:= max(H), keyby=list(Date)]
#minuteData[, dayLow:= min(L), keyby=list(Date)]
#minuteData[, dayMaxT:=T[which.max(H)], keyby=list(Date)]
#minuteData[, dayMinT:=T[which.min(L)], keyby=list(Date)]
#minuteData[, dayRange:= log(dayHigh/dayLow)]
#minuteData[, retCH:= log(C[T== CLOSET]/dayHigh)]
#minuteData[, retCL:= log(C[T==1459]/dayLow), keyby=list(Date)]
#minuteData[, retHO:= log(dayHigh/O[T==930]), keyby=list(Date)]
#minuteData[, retLO:=log(dayLow/O[T==930])]
#minuteData[, retCO:= log(C[T==1500]/O[T==930]), keyby=list(Date)]
#minuteData[, retOPC:=log(O/shift(C,1))]
#minuteData[Date==shift(Date,1), retOPC:=0]
#minuteData[, retCC:= retCO+retOPC]
#minuteData[, amFirst1:= log(C[T==930]/O[T==930]), keyby=list(Date)]
#minuteData[, amFirst5:= log(C[T==935]/O[T==930]), keyby=list(Date)]
#minuteData[, amFirst10:= log(C[T==940]/O[T==930]), keyby=list(Date)]
#minuteData[, amco:= log(C[T==1130]/O[T==930]), keyby=list(Date)]
#minuteData[, pmco:= log(C[T==1459]/O[T==1300]), keyby=list(Date)]
#minuteData[, pmOpen:= O[T==1300], keyby=list(Date)]
#minuteData[, pmFirst1:= log(C[T==1300]/O[T==1300])]
#minuteData[, pmClose:= C[T==1459], keyby=list(Date)]
#minuteData[ ,pmChg:=log(C[T==1459]/O[T==1300]), keyby=list(Date)]

#minuteData[Date==shift(Date,1), retOPC:=0]
#res2 <- minuteData[T==930, list(Date,T,retOPC,dayHigh, dayLow, dayMaxT,dayMinT,dayRange,retCH,retCL,retHO,retLO,retCO,retCC), keyby=list(Date)]
#res3<- minuteData[T==931, list(Date,T,amFirst1,amFirst5,amFirst10,amco,pmco, pmOpen, pmClose, pmChg, pmFirst, pmChg-pmFirst1), keyby=list(Date) ]


print(res)
write.table(res,"C:\\Users\\LUke\\Desktop\\Trading\\shcomp.txt",quote = FALSE,sep = "\t",row.names = FALSE)
#print(res3)
#wb <- loadWorkbook("C:\\Users\\LUke\\Desktop\\Trading\\new.xlsx",create = TRUE)
#createSheet(wb,"Sheet2")
#writeWorksheet(wb,res,"Sheet2",startRow = 1,startCol = 1, header = T)
#saveWorkbook(wb)
}
require(lubridate)
getSHCOMP()


###########################################################################################################################################

getBOCRmbRate<- function(){
  a <- read_html("http://www.boc.cn/sourcedb/whpj")
  a <- html_nodes(a, "tr")
  a <- html_text(a)
  a <-iconv(a,"utf-8","gb2312")
  
  a<-str_split(str_trim(a[str_detect(a,"美元")]),"\\s+")
  a[[1]] <- NULL
  a <- data.table(matrix(unlist(a),nrow = length(a),byrow = T))
  #names(a) <- c("货币名称","现汇买入价"," 现钞买入价","现汇卖出价","现钞卖出价","中行折算价","发布日期","发布时间")
  names(a) <- c("currency","Buy wire"," Buy cash","Sell wire","Sell Cash","BOC","Date","Time")
  #a[, BOC:=as.numeric(BOC)/100]
  print(as.numeric(a$BOC)/100)
  print(a)
}

getBOCRmbRate()


###########################################################################################################################################
#Packages
require(data.table)
require(stringr)
require(pdftools)
require(XLConnect)
require(rvest)
require(httr)
library(plyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(Rcpp)
library(lubridate)
library(PerformanceAnalytics)
library(quantmod)
library(xts)
require(TTR)
require("XLConnect")

#Methods only:
getFTSEData()
getFTSE50Index()
getNAV()
getXIN0UIndex()  
getIndicies()
getBOCRmbRate()

#get wtd max and min everyday
getWtdMaxMinAll()
getMAAll(20)
