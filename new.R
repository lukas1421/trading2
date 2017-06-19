
function(var) {

d2 <<- ddply(apr17Data,.(weekday,retOPCCat,amFirst5>0),summarise,med=sprintf("%.4f",median(substitute(var))), mean=sprintf("%.4f",mean(var)))


d2$x <- c(-0.05,0.05)

ggplot(apr17Data,aes(var,fill=amFirst5>0))+geom_density(alpha=0.4)+facet_grid(weekday~retOPCCat)+geom_text(data=d2,aes(x=d2$x, y=70, label=med, fill=d2$'amFirst5 > 0'), color="blue",show.legend = F)+geom_text(data=d2,aes(x=d2$x, y=50, label=mean, fill=d2$'amFirst5 > 0'
))+geom_text(data=d2,aes(x=0, y=70, label="median", fill=d2$'amFirst5 > 0'),color="blue")+geom_text(data=d2,aes(x=0, y=50, label="mean", fill=d2$'amFirst5 > 0'))

}

f <- function(var1) {
  a1 <- as.name(var1)
  #print(substitute(a1))
  #print(class((var1)))
  print(eval(substitute((ddply(apr17Data,.(weekday,retOPCCat,amFirst5>0),summarise,med=sprintf("%.4f",median(a)))), list(a=((substitute(a1)))))))
}

f <- function(var1) {
  a1 <- as.name(var1)
  #print(substitute(a1))
  #print(class((var1)))
  print(eval(substitute((ddply(apr17Data,.(weekday,percentileYCat,amFirst5>0),summarise,med=sprintf("%.4f",median(a)))), list(a=((substitute(a1)))))))
  dd2 <<- (eval(substitute((ddply(apr17Data,.(weekday,percentileYCat,amFirst5>0),summarise,med=sprintf("%.4f",median(a)))), list(a=((substitute(a1)))))))
}

f <- function(var1) {
  a1 <- as.name(var1)
  #print(substitute(a1))
  #print(class((var1)))
  print(eval(substitute((ddply(apr17Data,.(weekday,amClosePercentileCat,amFirst5>0),summarise,med=sprintf("%.4f",median(a)))), list(a=((substitute(a1)))))))
  dd2 <<- (eval(substitute((ddply(apr17Data,.(weekday,amClosePercentileCat,amFirst5>0),summarise,med=sprintf("%.4f",median(a)))), list(a=((substitute(a1)))))))
}

candleChart(as.xts(may8Data[Date>ymd("2016-1-1"),c("Open","High","Low","Close")], may8Data$Date[Date>ymd("2016-1-1")]), type="candlesticks", theme=chartTheme("white"))







