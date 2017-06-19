


yest<- fread("F:/ChinaYesterday files/Jan122017.csv")

yest[, amcpcat:=cut(amcp,quantile(amcp),include.lowest = T)]
yest[, cyampcat:=cut(cyamp,quantile(cyamp),include.lowest = T)]
yest[, openP := (open-ammin)/(ammax-ammin),]
yest[, openCat := cut(openP,quantile(openP,na.rm=T),include.lowest = T)]
yest[,amFirst10Cat := cut(f10,quantile(f10,include.lowest = T))]
yest[,ammaxt:= (floor(ammaxt/100))+(ammaxt-floor(ammaxt/100)*100)/60 ]
yest[,ammint:= (floor(ammint/100))+(ammint-floor(ammint/100)*100)/60 ]
yest[,pmmaxt:= (floor(pmmaxt/100))+(pmmaxt-floor(pmmaxt/100)*100)/60 ]
yest[,pmmint:= (floor(pmmint/100))+(pmmint-floor(pmmint/100)*100)/60 ]
yest[,ammaxtCat := cut(ammaxt,unique(quantile(ammaxt)),include.lowest = T)]
yest[,ammintCat := cut(ammint,unique(quantile(ammint)),include.lowest = T)]



#commands 
yest[, mean(pmco), keyby=list(amcpcat)]


# from current



yest[,TRCat := cut(trueRangeP,quantile(trueRangeP),include.lowest = T)]

current[,amFirst10Cat := cut(First10,quantile(First10,include.lowest = T))]
current[,amMaxT:= (floor(amMaxT/100))+(amMaxT-floor(amMaxT/100)*100)/60 ]
current[,amMinT:= (floor(amMinT/100))+(amMinT-floor(amMinT/100)*100)/60 ]
current[,pmMaxT:= (floor(pmMaxT/100))+(pmMaxT-floor(pmMaxT/100)*100)/60 ]
current[,pmMinT:= (floor(pmMinT/100))+(pmMinT-floor(pmMinT/100)*100)/60 ]
current[,amMaxTCat := cut(amMaxT,unique(quantile(amMaxT)),include.lowest = T)]
current[,amMinTCat := cut(amMinT,unique(quantile(amMinT)),include.lowest = T)]
current[,openCat := cut(OpenP,quantile(OpenP),include.lowest = T)]
current[, openYPCat:=cut(openYP, quantile(openYP),include.lowest = T)]
current[,percentileYCat := cut(percentileY,quantile(percentileY),include.lowest = T)]
current[,rangeCat := cut(Range,quantile(Range),include.lowest = T)]
current[,retOPCCat:=cut(retOPC,quantile(retOPC),include.lowest = T)]
current[,retCO:=retCC-retOPC]
#current[,retPMCOY:=shift(retPMCO)]
#current[is.na(retPMCOY),retPMCOY:=0]
#current[,retPMCOYCat:=cut(retPMCOY,quantile(retPMCOY),include.lowest = T)]
current[,amCPCat:=cut(amCP,unique(quantile(amCP)),include.lowest = T)]
#current[,retPMCOCat:=cut(retPMCO,quantile(retPMCO))]
current[,retCHYCat:=cut(retCHY, quantile(retCHY),include.lowest = T)]
current[,retCLYCat:=cut(retCLY,quantile(retCLY),include.lowest = T)]
current[,retCHCLYSum:=retCHY+retCLY]
current[,retCHCLYSumCat:=cut(retCHCLYSum,quantile(retCHCLYSum),include.lowest = T)]
current[,sizeCat:=cut(size, quantile(size),include.lowest = T)]

current[,minTY:=(floor(minTY/100))+(minTY-floor(minTY/100)*100)/60]
current[,maxTY:=(floor(maxTY/100))+(maxTY-floor(maxTY/100)*100)/60]

current[,minTYCat:=cut(minTY,quantile(minTY),include.lowest = T)]
current[,maxTYCat:=cut(maxTY,quantile(maxTY),include.lowest = T)]

current[,sizeSizeYCat:=cut(sizeSizeY, quantile(sizeSizeY,na.rm = T),include.lowest = T)]
current[,retCOCat:=cut(retCO,quantile(retCO),include.lowest = T)]
current[, pmFirst10Cat:=cut(pmFirst10,quantile(pmFirst10),include.lowest = T)]



## Loss Aversion
#1
current[retCO < -2, list(T, name, amMinT,amMinTCat,amMaxT,amMaxTCat,amCPCat )][,table(amMaxTCat)] #exclude yili/wanke
current[retCO < -2, list(T, name, amMinT,amMinTCat,amMaxT,amMaxTCat,amCPCat )][,table(amMinTCat)]

#check CO based on amMax
current[, mean(retCO), keyby=list(amMaxTCat)]
current[, mean(retCO), keyby=list(amMinTCat,amMaxTCat)][order(-V1)]


