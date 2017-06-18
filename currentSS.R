
current<- fread("F:/dec16.csv")


current[,TRCat := cut(trueRangeP,quantile(trueRangeP),include.lowest = T)]
current[,amFirst10Cat := cut(First10,quantile(First10,include.lowest = T))]
current[,amFirst1Cat := cut(first1,quantile(first1,include.lowest = T))]
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
current[ ,VR930925:=(VR930/VR925)]
current[, VR925Cat:=cut(VR925, unique(quantile(VR925,na.rm = T)),include.lowest = T)]
current[, VR930925Cat:=cut(VR930925, unique(quantile(VR930925,na.rm = T)),include.lowest = T)]

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

current[,sizeSizeYCat:=cut(sizeSizeY, unique(quantile(sizeSizeY,na.rm = T)),include.lowest = T)]
current[,retCOCat:=cut(retCO,quantile(retCO),include.lowest = T)]
current[, pmFirst10Cat:=cut(pmFirst10,unique(quantile(pmFirst10)),include.lowest = T)]
current[, cyampCat:= cut(cyamp, quantile(cyamp),include.lowest = T)]

current[, amMinPYCat:= cut(amMinPY,quantile(amMinPY),include.lowest = T)]


## Loss Aversion
#1
current[retCO < -2, list(T, name, amMinT,amMinTCat,amMaxT,amMaxTCat,amCPCat )][,table(amMaxTCat)] #exclude yili/wanke
current[retCO < -2, list(T, name, amMinT,amMinTCat,amMaxT,amMaxTCat,amCPCat )][,table(amMinTCat)]

#check CO based on amMax
current[, mean(retCO), keyby=list(amMaxTCat)]
current[, mean(retCO), keyby=list(amMinTCat,amMaxTCat)][order(-V1)]
current[retCO>3,list(t,name, retCO,retCC) ][order(-retCO)]

current[, mean(percentile)]


