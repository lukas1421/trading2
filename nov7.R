
current<- fread("F:/nov9.csv")


current[,TRCat := cut(trueRangeP,quantile(trueRangeP),include.lowest = T)]
current[,amFirst10Cat := cut(First10,quantile(First10,include.lowest = T))]
current[,amMaxT:= (floor(amMaxT/100))+(amMaxT-floor(amMaxT/100)*100)/60 ]
current[,amMinT:= (floor(amMinT/100))+(amMinT-floor(amMinT/100)*100)/60 ]
current[,amMaxTCat := cut(amMaxT,quantile(amMaxT),include.lowest = T)]
current[,amMinTCat := cut(amMinT,unique(quantile(amMinT)),include.lowest = T)]
current[,openCat := cut(OpenP,quantile(OpenP),include.lowest = T)]
current[,percentileY:=shift(percentile,1)]
current[is.na(percentileY),percentileY:=50]
current[,percentileYCat := cut(percentileY,quantile(percentileY),include.lowest = T)]
current[,rangeCat := cut(Range,quantile(Range),include.lowest = T)]
current[,retOPCCat:=cut(retOPC,quantile(retOPC),include.lowest = T)]
current[,retPMCOY:=shift(retPMCO)]
current[is.na(retPMCOY),retPMCOY:=0]
current[,retPMCOYCat:=cut(retPMCOY,quantile(retPMCOY),include.lowest = T)]
current[,amCPCat:=cut(amCP,unique(quantile(amCP)),include.lowest = T)]
current[,retPMCOCat:=cut(retPMCO,quantile(retPMCO))]


nov7[,mean(percentile),keyby=list(TRCat)]


current[,mean(percentile),keyby=list(amFirst10Cat)]
current[,mean(percentile),keyby=list(amMaxTCat)]
nov7[,mean(percentile),keyby=list(openCat)]
nov7[,mean(percentile),keyby=list(amFirst10Cat)]
nov7[,mean(percentile),keyby=list(openCat)]


nov7[,mean(percentile),keyby=list(percentileYCat)]
nov7[,mean(percentile),keyby=list(amFirst10Cat)]

current[,mean(percentile),keyby=list(rangeCat)]
nov7[,mean(percentile),keyby=list(amMaxTCat)]
nov7[,amMinTCat := cut(MinT,quantile(MinT),include.lowest = T)]
nov7[,mean(percentile),keyby=list(amMinTCat)]

nov7[,mean(Return),keyby=list(amMinTCat)]


nov7[,mean(Return),keyby=list(amMaxTCat)]

nov7[,mean(retCO-First10),keyby=list(amFirst10Cat)]
nov7[,sizeCat := cut(Size,quantile(Size),include.lowest = T)]
nov7[,mean(Return),keyby=list(sizeCat)]
nov7[,mean(Return),keyby=list(amMaxTCat)]
nov7[,mean(Return),keyby=list(amMinT1Cat)]
nov7[,mean(Return),keyby=list(rangeCat)]
nov7[,mean(Return),keyby=list(sizeCat)]
nov7[,mean(Return),keyby=list(amFirst10Cat)]
nov7[,mean(Return-First10),keyby=list(amFirst10Cat)]
nov7[amFirst10< -0.4, list(Ticker)]
nov7[First10< -0.4  , list(Ticker,Name, retCC,retCO,First10,retCO-First10,percentile)][order(-V6)]


nov7[,amP:=]
nov7[,calcSharp(retPMCO), keyby=list(amFirst10Cat)]



nov7[First10< -0.4  , list(Ticker,Name, Return,Return-First10, percentile)]

nov7[,amMaxT:=floor(amMaxT/100)+(amMaxT-floor(amMaxT/100)*100)/60, ]
nov7[,amMinT:=floor(amMinT/100)+(amMinT-floor(amMinT/100)*100)/60, ]

nov7[,amMaxTCat:=cut(amMaxT,quantile(amMaxT),include.lowest = T)]
nov7[,amMinTCat:=cut(amMinT,quantile(amMinT),include.lowest = T)]

