
calcBeta() {
  
  url <- paste0("http://biz.finance.sina.com.cn/stock/flash_hq/kline_data.php?symbol=",symb,"&begin_date=20160315&end_date=20170420")
  
}


getPrice <- function(symb) {
  
  require(rvest)
  require(data.table)
  require(lubridate)
  
  url <- paste0("http://biz.finance.sina.com.cn/stock/flash_hq/kline_data.php?symbol=",symb,"&begin_date=20140101&end_date=20170424")
  a<-read_html(url)
  
  l<-html_nodes(a,"content")
  
  dt=data.table(d=ymd(xml_attr(html_nodes(a,"content"),"d")),o=as.numeric(xml_attr(html_nodes(a,"content"),"o")),
                h=as.numeric(xml_attr(html_nodes(a,"content"),"h")),l=as.numeric(xml_attr(html_nodes(a,"content"),"l")),
                c=as.numeric(xml_attr(html_nodes(a,"content"),"c")),v=as.numeric(xml_attr(html_nodes(a,"content"),"v")))
  
  return(dt)
    
}


getReturnDT<- function(symb) {
  require(rvest)
  require(data.table)
  require(lubridate)
  
  url <- paste0("http://biz.finance.sina.com.cn/stock/flash_hq/kline_data.php?symbol=",symb,"&begin_date=20140101&end_date=20170424")
  a<-read_html(url)
  
  l<-html_nodes(a,"content")
  
  dt=data.table(d=ymd(xml_attr(html_nodes(a,"content"),"d")),o=as.numeric(xml_attr(html_nodes(a,"content"),"o")),
                h=as.numeric(xml_attr(html_nodes(a,"content"),"h")),l=as.numeric(xml_attr(html_nodes(a,"content"),"l")),
                c=as.numeric(xml_attr(html_nodes(a,"content"),"c")),v=as.numeric(xml_attr(html_nodes(a,"content"),"v")))
  
  
  
  dt[, co:=c/o-1]
  dt[, cy:= shift(c,1)]
  dt[, cc := log(c/cy)]
  dt[is.na(cc), cc:=0]
  dt1 = dt[, return(list(d,cc)), ]
    
    #names(dt1) <- c("D", deparse(substitute(symb)))
  names(dt1) <- c("D", symb)
  
    return(dt1)
}

computeBeta <- function(symb1,symb2){
dd<-getReturnDT(symb1)
dd2 <-getReturnDT(symb2)
dd3<<-merge(dd,dd2,by = "D")
print(dd3[, cov(eval(parse(text=symb1)),eval(parse(text=symb2)))/var(eval(parse(text=symb2))),])
}

require(PerformanceAnalytics)

graphN <- function(n) {
  require(ggplot2)
  print(names(dd3))
  print(names(dd3)[n])
  symb <- names(dd3)[n]
  symb1 <-names(dd3)[2]
  symb2 <-names(dd3)[3]
  print(symb)
  dd3[, qplot(D, cumprod(1+eval(parse(text = symb))),geom="line")]
  dd3[, cor(eval(parse(text=symb1)),eval(parse(text=symb2)))]
  print("beta ")
  dd3[, ]
  print(dd3[, cov(eval(parse(text=symb1)),eval(parse(text=symb2)))/var(eval(parse(text=symb2))),])
}

getSD <- function(symb) {
  tmp <- getReturnDT(symb)
  #print(tmp)
  
  #tmp[, sd5 <- sd.annualized()]
  return(tmp[, list(D, eval(parse(text=symb)), sd5=apply.rolling(xts(eval(parse(text=symb)),order.by=D),5,FUN = sd.annualized),
             sd10=apply.rolling(xts(eval(parse(text=symb)),order.by=D),10,FUN = sd.annualized),
             sd20=apply.rolling(xts(eval(parse(text=symb)),order.by=D),20,FUN = sd.annualized)
             ), ])
}


predictCLFrom52wp <- function(symb){

    tmp <- getPrice(symb)
    #print(tmp)
    tmp<-tmp[, list(d,h,l,c, cl=log(c/l))]
    tmp<-tmp[, list(d,c,cl, max=apply.rolling(xts(h,order.by = d),250, FUN = max ), 
                    min=apply.rolling(xts(l,order.by = d),250, FUN = min )
                    )]
    tmp<- tmp[, list(d,c,max,min, perc=(c-min)/(max-min), cl,
                     cl5 = apply.rolling(xts(cl,order.by = d),5,FUN = mean),
                     cl10 = apply.rolling(xts(cl,order.by = d),10,FUN = mean),
                     cl20 = apply.rolling(xts(cl,order.by = d),20,FUN = mean)
                     )]
    print(tmp)
    return(tmp)
}

dd[, cl5Next5:= shift(cl5,5,type = "lead")]
dd[, cl10Next10:= shift(cl10,10,type = "lead")]
dd[, cl20Next20:= shift(cl20,20,type = "lead")]

