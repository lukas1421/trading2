
#Day max
daymax<-cyb[,{m<-mget(names(.SD)[grep("^H",names(.SD))]); names(m)[m==max(unlist(m[1:(length(m)-1)]))][1]}, list(Date) ]
cyb[,dayMaxT:=chgTime(daymax$V1)]

daymaxV <- cyb[,{m<-mget(names(.SD)[grep("^H",names(.SD))]); max(unlist(m[1:(length(m)-1)]))}, list(Date) ]
cyb[,daymax := daymaxV$V1]

#am max
ammax<-cyb[,{m<-mget(names(.SD)[grep("^H(9|10|11)",names(.SD))]); names(m)[m==max(unlist(m[1:(length(m)-1)]))][1]}, list(Date) ]
cyb[,amMaxT:=chgTime(ammax$V1),]

ammaxV <- cyb[,{m<-mget(names(.SD)[grep("^H(9|10|11)",names(.SD))]); max(unlist(m[1:(length(m)-1)]))}, list(Date) ]
cyb[,ammax := ammaxV$V1]

#pm max
pmmax<-cyb[,{m<-mget(names(.SD)[grep("^H(13|14|15)",names(.SD))]); names(m)[m==max(unlist(m[1:(length(m)-1)]))][1]}, list(Date) ]
cyb[,pmMaxT:=chgTime(pmmax$V1),]

pmmaxV <- cyb[,{m<-mget(names(.SD)[grep("^H(13|14|15)",names(.SD))]); max(unlist(m[1:(length(m)-1)]))}, list(Date) ]
cyb[,pmmax := pmmaxV$V1]



#Day min
daymin <- cyb[,{m<-mget(names(.SD)[grep("^L",names(.SD))]); names(m)[m==min(unlist(m[1:(length(m)-1)]))][1]}, list(Date) ]
cyb[, dayMinT:=chgTime(daymin$V1)]

dayminV <- cyb[,{m<-mget(names(.SD)[grep("^L",names(.SD))]); min(unlist(m[1:(length(m)-1)]))}, list(Date) ]
cyb[,daymin := dayminV$V1]

#am min
ammin<-cyb[,{m<-mget(names(.SD)[grep("^L(9|10|11)",names(.SD))]); names(m)[m==min(unlist(m[1:(length(m)-1)]))][1]}, list(Date) ]
cyb[,amMinT:=chgTime(ammin$V1),]

amminV <- cyb[,{m<-mget(names(.SD)[grep("^L(9|10|11)",names(.SD))]); min(unlist(m[1:(length(m)-1)]))}, list(Date) ]
cyb[,ammin := amminV$V1]

#pm min
pmmin<-cyb[,{m<-mget(names(.SD)[grep("^L(13|14|15)",names(.SD))]); names(m)[m==min(unlist(m[1:(length(m)-1)]))][1]}, list(Date) ]
cyb[,pmMinT:=chgTime(pmmin$V1),]

pmminV <- cyb[,{m<-mget(names(.SD)[grep("^L(13|14|15)",names(.SD))]); min(unlist(m[1:(length(m)-1)]))}, list(Date) ]
cyb[,pmmin := pmminV$V1]


cyb[, Date:=ymd(Date)]

cyb[, percentileYCat:=cut(percentileY, quantile(percentileY, na.rm = T),include.lowest = T)]
cyb[, retPMCOY:=shift(retPMCO,1)]
cyb[, retPMCOYCat:=cut(retPMCOY, quantile(retPMCOY,na.rm = T),include.lowest = T)] 
 
#ammaxcat
cyb[, ammaxcat:= cut(amMaxT,quantile(amMaxT,na.rm=T),include.lowest = T )]
cyb[, ammincat:= cut(amMinT, quantile(amMinT,na.rm=T),include.lowest = T)]
cyb[,pmmaxcat:=cut(pmMaxT, quantile(pmMaxT, na.rm=T),include.lowest = T)]
cyb[,pmmincat:=cut(pmMinT, quantile(pmMinT,na.rm=T),include.lowest = T)]

cyb[,list(Date,daymax,ammax,pmmax, daymin,ammin, pmmin, dayMaxT, amMaxT, amMinT, dayMinT,pmMaxT,pmMinT)]

cyb[,amClosePercentile:=(amClose-ammin)/(ammax-ammin)]
cyb[,percentile:=(Close-daymin)/(daymax-daymin)]
cyb[,pmClosePercentile:=(Close-pmmin)/(pmmax-pmmin)]

cyb[, amClosePercentileCat := cut(amClosePercentile,quantile(amClosePercentile,na.rm=T),include.lowest = T)]
cyb[, openPercentile:=(Open-ammin)/(ammax-ammin)]


#strategy
cyb[, lsam:= ifelse(weekday==1, ifelse(amFirst10>0.002 & retOPC > -0.02, (retAMCO-amFirst10)/retAMCO,0),ifelse(weekday==3,ifelse(retOPC>0.00146,0,0) ,0) )]

cyb[, lspm:= ifelse(weekday==1, ifelse(amFirst10> 0.002 & retOPC> -0.02, 1, 0), 
                      ifelse(weekday==2, ifelse(percentileY<0.9 & amClosePercentile>0.2 & retAMCO > -0.03 & retOPC > -0.01,ifelse(retPMCOY < -0.004,1,ifelse(retPMCOY > 0.005, 0.2, 0.5)),0), 
                             ifelse(weekday==3, ifelse(percentileY < 0.85 & amClosePercentile < 0.8, ifelse(retPMCOY < -0.004, 1, 0.5),0),
                                    ifelse(weekday==4,ifelse(retOPC < 0.01, 
                                                             (ifelse(retPMCOY < 0, ifelse(pmFirst10>0,(retPMCO-pmFirst10)/(retPMCO),0), ifelse(pmFirst10<0,-0.5*(retPMCO-pmFirst10)/(retPMCO),0))),0) 
                                           ,ifelse(percentileY<0.4, ifelse(pmFirst10 > -0.004,(retPMCO-pmFirst10)/retPMCO,0), 
                                                   ifelse(openPercentile<0.85 & amClosePercentile <0.95 & pmFirst10 > 0 & amMinT<11.25 & percentileY < 0.9,(retPMCO-pmFirst10)/retPMCO,0)))))),]



cyb[, lsday:= ifelse(weekday==1, ifelse(amFirst10>0,2,0), 
                    ifelse(weekday==2, ifelse(amFirst10>0 & retCOY<0,2,0), 
                           ifelse(weekday==3, ifelse(retCOY<0 & amFirst10>0, 2, 0),
                                  ifelse(weekday==4,ifelse(amFirst10>0 & retCOY<0,0,0), 
                                         ifelse(amFirst10>0 & retCOY>0,1,0))))),]



#Data crunching was already done in Excel
cyb[is.na(percentileY),percentileY:=0.5,]
cyb[,percentileYCat:=cut(percentileY,quantile(percentileY),include.lowest = T)]
cyb[,closeYPercentile]

cyb[,retPMCOY:=shift(retPMCO,1)]
cyb[is.na(retPMCOY),retPMCOY:=0,Date,]
cyb[, retPMCOYCat:= cut(retPMCOY, quantile(retPMCOY),include.lowest = T)]
cyb[, retPMCOCat:= cut(retPMCO, quantile(retPMCO),include.lowest = T)]
cyb[ ,ampmChgPercentile:=(Close-amClose)/(dayMax-dayMin),]



aug22[ ,pmLowToClosePercentile := (Close-pmMin)/(dayMax-dayMin)]
aug22[amClosePercentile<0,amClosePercentile:=0, ]
aug22[amClosePercentile>1,amClosePercentile:=1, ]
cyb[,amClosePercentile:=(amClose-amMin)/(amMax-amMin)]
cyb[is.na(amClosePercentile),amClosePercentile:=0]
cyb[, amClosePercentileCat:= cut(amClosePercentile, quantile(amClosePercentile), include.lowest = T)]
cyb[,percentileYCat := cut(percentileY, quantile(percentileY), include.lowest = T)]
cyb[,percentileCat := cut(percentile, quantile(percentile), include.lowest = T)]

aug22[,retPMCLCat := cut(retPMCL, quantile(retPMCL), include.lowest = T)]
aug22[,pmClosePercentileCat:= cut(pmClosePercentile, quantile(pmClosePercentile),include.lowest = T)]

cyb[, amFirst5Cat:= cut(amFirst5, quantile(amFirst5),include.lowest = T)]
cyb[, amFirst10Cat:= cut(amFirst10, quantile(amFirst10),include.lowest = T)]
cyb[, ammaxCat := cut(amMaxT1,quantile(amMaxT1), include.lowest = T)]
cyb[, amminCat := cut(amMinT1, quantile(amMinT1), include.lowest = T)]
aug22[, pmClosePercentileY := c(0,pmClosePercentile)[1:4083]]
aug22[,pmClosePercentileYCat := cut(pmClosePercentileY, quantile(pmClosePercentileY), include.lowest = T)]
cyb[, pmFirst5Cat:= cut(pmFirst5, quantile(pmFirst5),include.lowest = T)]
cyb[, pmFirst10Cat:= cut(pmFirst10, quantile(pmFirst10),include.lowest = T)]


cyb[,closeY:=shift(Close,1)]
cyb[,closeYAMPercentile:=(closeY-amMin)/(amMax-amMin)]
cyb[is.na(closeYAMPercentile),closeYAMPercentile:=0.5]
cyb[,closeYAMPercentileCat:=cut(closeYAMPercentile,quantile(closeYAMPercentile),include.lowest = T)]


cyb[,openPercentileCat:=cut(openPercentile,quantile(openPercentile),include.lowest = T)]



#helper
chgTime <- function(x) {
  t <- str_sub(x,2)
  t2 <- as.numeric(str_sub(t,1,-3))+as.numeric(str_sub(t,-2))/60
}