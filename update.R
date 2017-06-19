

calcSharp <- function(x) {
  
  #ifelse(length(x[x!=0]) > 0, x<-x[x!=0], 0);
  
  m <- mean(x)
  cum <- tail(cumprod(1+x),1)
  max <- max(x)
  min <- min(x)
  sd <- sd(x)
  dd <- sd(x[x<m])
  
  
  dd2 <- sqrt(sum(x[x<m]^2)/length(x[x<m]))
  dd3 <- DownsideDeviation(x, MAR=m)
  ud <- sd(x[x>m])
   
  return(list(mean=sprintf("%.4f",mean(x)),cum = sprintf("%.4f",cum), max=sprintf("%.4f",max), min=sprintf("%.4f",min), 
              sd=sprintf("%.4f",sd),ud=sprintf("%.4f",ud), dd=sprintf("%.4f",dd),dd2=sprintf("%.4f",dd2),dd3=sprintf("%.4f",dd3),
              sr = sprintf("%.4f",m/sd*sqrt(252)), sortino1=sprintf("%.4f",m/dd*sqrt(252)),sortino2=sprintf("%.4f",m/dd2*sqrt(252)),
              sortino3=sprintf("%.4f",m/dd3*sqrt(252)),sortino4=sprintf("%.4f",SortinoRatio(x)*sqrt(252))))
}




jun23[, lspm:= ifelse(weekday==1, ifelse(retOPC > -0.003 & amFirst10>0,1,0), 
                             ifelse(weekday==2, ifelse(retPMCOY < -0.004, 1,ifelse(retPMCOY > 0.005, -0.5, 0.5)), 
                                    ifelse(weekday==3, ifelse(pmFirst10>0 & retPMCOY < -0.004, (retPMCO-pmFirst10)/(retPMCO), 0),
                                           ifelse(weekday==4, ifelse(retPMCOY < -0.006, 1, -0.5), ifelse(retOPC > -0.002 & amFirst10>0,1,0))) )) , ]

#current strategy
jun23[, lspm:= ifelse(weekday==1, ifelse(pmFirst10>0,(retPMCO-pmFirst10)/(retPMCO) ,0), 
                      ifelse(weekday==2, ifelse(retPMCOY < -0.004,ifelse(pmFirst10>0,(retPMCO-pmFirst10)/(retPMCO),1),ifelse(retPMCOY > 0.005, -0.2, 0.5)), 
                             ifelse(weekday==3, ifelse(retPMCOY < -0.004, 1, 0.5),
                                    ifelse(weekday==4,ifelse(pmFirst10>0 & retPMCOY < -0.004, (retPMCO-pmFirst10)/(retPMCO), -1) 
                                           , ifelse(amFirst10>0 & pmFirst10>0,(retPMCO-pmFirst10)/(retPMCO) ,0))) )) , ]




jun23[, lspm:= ifelse(weekday==1, 1, 
                      ifelse(weekday==2, ifelse(retPMCOY < -0.004,,ifelse(retPMCOY > 0.005, -0.2, 0.5)), 
                             ifelse(weekday==3, ifelse(retPMCOY < -0.004, 1, 0.5),
                                    ifelse(weekday==4,ifelse(pmFirst10>0 & retPMCOY < -0.004, (retPMCO-pmFirst10)/(retPMCO), -1) 
                                           , ifelse(amFirst10>0 & pmFirst10>0,(retPMCO-pmFirst10)/(retPMCO) ,0))) )) , ]





#current AM strategy
jun23[, lsam:= ifelse(weekday==1, ifelse(amFirst10>0, (retAMCO-amFirst10)/retAMCO,-(retAMCO-amFirst10)/retAMCO),ifelse(weekday==3,ifelse(retOPC>0.00146,1,0) ,0) )]


#current AM strategy Jul9:
jun23[, lsam:= ifelse(weekday==1, ifelse(amFirst10> 0.00164, (retAMCO-amFirst10)/retAMCO,ifelse(amFirst10 < -0.00435, -1,0)),ifelse(weekday==3,ifelse(retOPC>0.00146,1,0) ,0) )]

#current PM strategy Jul9
jun23[, lspm:= ifelse(weekday==1, ifelse(pmFirst10> 0 & amClosePercentile > 0.1 & amFirst5 > -0.01, (retPMCO-pmFirst10)/retPMCO, 0), 
                ifelse(weekday==2, ifelse(percentileY<0.9 & amClosePercentile>0.2 & retAMCO > -0.03 & retOPC > -0.01,ifelse(retPMCOY < -0.004,1,ifelse(retPMCOY > 0.005, 0.2, 0.5)),0), 
                         ifelse(weekday==3, ifelse(percentileY < 0.85 & amClosePercentile < 0.8, ifelse(retPMCOY < -0.004, 1, 0.5),0),
                                ifelse(weekday==4,ifelse(retOPC < 0.01 &  abs(pmFirst1) < 0.005, 
                                      (ifelse(retPMCOY < 0, ifelse(pmFirst10>0,(retPMCO-pmFirst10)/(retPMCO),0), ifelse(pmFirst10<0,-0.5*(retPMCO-pmFirst10)/(retPMCO),0))),0) 
                                         ,ifelse(percentileY<0.4, ifelse(pmFirst10 > -0.004,(retPMCO-pmFirst10)/retPMCO,0), 
                                               ifelse(openAMPercentile<0.85 & amClosePercentile <0.95 & pmFirst10 > 0 & amMinT1<11.25 & percentileY < 0.9,(retPMCO-pmFirst10)/retPMCO,0)))))),]

#current PM strategy Jul10
#take away wednesday amCLosePercentile<0.8
aug22[, lspm:= ifelse(weekday==1, ifelse(pmFirst10> 0.000 & amClosePercentile > 0.1 & amMaxT1>9.6 & amFirst5 > -0.01 & amMax>CloseY, (retPMCO-pmFirst10)/retPMCO, ifelse(amMax<CloseY & amClosePercentile<0.2 & pmFirst10<0, -0.5*(retPMCO-pmFirst10)/retPMCO,0)), 
      ifelse(weekday==2, ifelse(percentileY<0.226, ifelse(abs(retOPC) < 0.03 & openPercentile<0.8,1,0)*ifelse(amClosePercentile<0.5 & pmMaxT1Y<14,2,1),
        ifelse(percentileY<0.8,ifelse(amClosePercentile>0.2 & retAMCO > -0.03 & retOPC > -0.01,1,0), 0 )), 
           ifelse(weekday==3, ifelse(percentileY < 0.85, ifelse((amMinT1>9.7 & amClosePercentile>0.9),0.25,1)*ifelse(amMax<CloseY,0,1)*ifelse(retPMCOY < -0.004, 1, 0.5),0),
              ifelse(weekday==4,ifelse(retOPC < 0.01, 
                   (ifelse(percentileY<0.6, ifelse(pmFirst10>0,(ifelse(amMinT1<10.3,2,1))*(retPMCO-pmFirst10)/(retPMCO),0), ifelse(percentileY>0.85 & pmFirst10<0,-0.5*(retPMCO-pmFirst10)/(retPMCO),0))),0) 
                           ,ifelse(percentileY<0.4, ifelse(pmFirst10 > -0.004,(retPMCO-pmFirst10)/retPMCO,0), 
                                 ifelse(openAMPercentile<0.85 & amClosePercentile <0.95 & pmFirst10 > 0 & amMinT1<11.25 & percentileY < 0.9,(retPMCO-pmFirst10)/retPMCO,0)))))),]

maotai[, ls:= ifelse(weekday==1, ifelse(percentileY>0.5,1,0), 
                      ifelse(weekday==2, ifelse(percentileY>0.5,1,0), 
                             ifelse(weekday==3, ifelse(percentileY>0.5, 1, 1),
                                    ifelse(weekday==4,ifelse(percentileY<0.77, 1,-1) 
                                           ,ifelse(weekday==5,ifelse(percentileY>0.5,1,0),0))))), ]
maotai[is.na(ls),ls:=0]
maotai[is.na(percentileY),percentileY:=0.5]

maotai[D>ymd("2006-5-26"),calcSharp(retCO*ls),keyby=list(weekday)]
maotai[D>ymd("2006-5-26"),calcSharp(retCO*ls),keyby=list(weekday)]


#max/min pm
a<-jun23[weekday=="5", 
  {
    
    n<-names(.SD)[grep("^L(13|14|15)", names(.SD))];
    um<-(mget(n)); 
    um<-um[1:which(names(um)==paste0("L",1340))]; 
    maxl<- max(unlist(um)); minl<-min(unlist(um));
    
    #max
    n1 <- names(.SD)[grep("^H(13|14|15)",names(.SD))];
    um1 <- mget(n1);
    um1 <- um1[1:which(names(um1)==paste0("H",1316))];
    maxh <- max(unlist(um1)); minh <- min(unlist(um1));
    
    res<-lapply(um, function(x) (x-minl)/(maxl-minl)); 
    res2 <- lapply(um1, function(x) (x-minh)/(maxh-minh))
    
    c(res[length(res)],res2[length(res2)],V1=log(Close/O1340)*10000,pmcoyc=retPMCOYCat, pmcl = retPMCL, pmch=retPMCH, percentiley=percentileY, ammaxcat=ammaxCat,ammincat=amminCat,ammaxt=amMaxT1,ammint=amMinT1,am10=amFirst10*10000,pm10=pmFirst10*10000, amclosep=amClosePercentile, closeP=percentile) 
  }, keyby=Date ][H1316==1 & L1340==0,][order(as.numeric(-percentiley))][,mean(V1),keyby=list(ammincat)]


#max/min am
jun23[weekday=="1", 
      {
        n<-names(.SD)[grep("^L(9|10|11)", names(.SD))];
        um<-(mget(n)); 
        um<-um[1:which(names(um)==paste0("L",942))]; 
        maxl<- max(unlist(um)); minl<-min(unlist(um));
        
        #max
        n1 <- names(.SD)[grep("^H(9|10|11)",names(.SD))];
        um1 <- mget(n1);
        um1 <- um1[1:which(names(um1)==paste0("H",949))];
        maxh <- max(unlist(um1)); minh <- min(unlist(um1));
        
        res<-lapply(um, function(x) (x-minl)/(maxl-minl)); 
        res2 <- lapply(um1, function(x) (x-minh)/(maxh-minh))
        
        c(res[length(res)],res2[length(res2)], toAMClose=log(amClose/O949)*10000,toPMClose=log(Close/O949)*10000,amperc=100*amClosePercentile,ampmchg=100*ampmChgPercentile, pmco=retPMCO*10000, ammax=ammaxCat,ammin=amminCat,amMaxT=amMaxT1,amMinT=amMinT1,am10=amFirst10*10000,pm10=pmFirst10*10000) 
      }, keyby=Date ][L942==0 & H949==1,][,list(mean(toAMClose),mean(pmco), mean(toPMClose))]





getAMHighPercentile <- function(l, time) {
  n<-names(l)[grep("^H(9|10|11)", names(l))]; 
  print(n);
  print(n[2])
  um <- get(n[2])
  #um <- get("H931")
  #um<-(mget(n, envir = sys.frame(-1))); 
  print(um);
  um<-um[1:which(names(um)==paste0("H",940))]; 
  maxl<- max(unlist(um)); 
  minl<-min(unlist(um)); 
  res<-lapply(um, function(x) (x-minl)/(maxl-minl)); 
  res[length(res)] ;
}

aug22[,retPMCOY:=shift(retPMCO,1)]
aug22[is.na(retPMCOY),retPMCOY:=0,Date,]
aug22[, retPMCOYCat:= cut(retPMCOY, quantile(retPMCOY),include.lowest = T)]
aug22[, retPMCOCat:= cut(retPMCO, quantile(retPMCO),include.lowest = T)]
aug22[ ,ampmChgPercentile:=(Close-amClose)/(dayMax-dayMin),]



aug22[ ,pmLowToClosePercentile := (Close-pmMin)/(dayMax-dayMin)]
aug22[amClosePercentile<0,amClosePercentile:=0, ]
aug22[amClosePercentile>1,amClosePercentile:=1, ]
aug22[is.na(amClosePercentile),amClosePercentile:=0]
aug22[, amClosePercentileCat:= cut(amClosePercentile, quantile(amClosePercentile), include.lowest = T)]
aug22[,percentileYCat := cut(percentileY, quantile(percentileY), include.lowest = T)]
aug22[,percentileCat := cut(percentile, quantile(percentile), include.lowest = T)]
aug22[is.na(percentileY),percentileY:=0.5]
aug22[,retPMCLCat := cut(retPMCL, quantile(retPMCL), include.lowest = T)]
aug22[,pmClosePercentileCat:= cut(pmClosePercentile, quantile(pmClosePercentile),include.lowest = T)]
aug22[, amFirst5Cat:= cut(amFirst5, quantile(amFirst5),include.lowest = T)]
aug22[, amFirst10Cat:= cut(amFirst10, quantile(amFirst10),include.lowest = T)]
aug22[, ammaxCat := cut(amMaxT1,quantile(amMaxT1), include.lowest = T)]
aug22[, amminCat := cut(amMinT1, quantile(amMinT1), include.lowest = T)]
aug22[, pmClosePercentileY := c(0,pmClosePercentile)[1:4083]]
aug22[,pmClosePercentileYCat := cut(pmClosePercentileY, quantile(pmClosePercentileY), include.lowest = T)]

aug22[,closeY:=shift(Close,1)]
aug22[,closeYAMPercentile:=(closeY-amMin)/(amMax-amMin)]
aug22[is.na(closeYAMPercentile),closeYAMPercentile:=0.5]
aug22[,closeYAMPercentileCat:=cut(closeYAMPercentile,quantile(closeYAMPercentile),include.lowest = T)]

aug22[,openPercentileCat:=cut(openPercentile,quantile(openPercentile),include.lowest = T)]

#compute expected max and min given current maxT, minT, currentime
#unconditional
computeAM <- function(currTime,maxT, minT, wkdy) {
  #break into 10 quantiles
  print(jun23[weekday==wkdy,quantile(amMaxT1,seq(0,1,0.1))])
  print(jun23[weekday==wkdy,quantile(amMinT1,seq(0,1,0.1))])
}

aug22[,amClosePercentileTR:=(amClose-min(dayMinY,amMin))/(max(dayMaxY,amMax)-min(dayMinY,amMin)), keyby=Date]
aug22[is.na(amClosePercentileTR),amClosePercentileTR:=0.5]
aug22[,amClosePercentileTRCat:=cut(amClosePercentileTR,quantile(amClosePercentileTR),include.lowest = T)]


oct27[, lspm:= ifelse(weekday==1, ifelse(dayMaxT1Y>14.5,1,0), 
                      ifelse(weekday==2, ifelse(dayMaxT1Y<10,1,0), 
                             ifelse(weekday==3, ifelse(dayMaxT1Y<14.5, 1, 0.5),
                                    ifelse(weekday==4,ifelse(dayMaxT1Y<14.5, 1, -1) 
                                           , ifelse(dayMaxT1Y<11.1,1,0))) )) , ]

oct27[is.na(lspm),lspm:=0 ]


latest[, lspm:= ifelse(weekday=="1", ifelse(retCOY>0,1,0), 
                      ifelse(weekday=="2", ifelse(retCCY < 0,1,0), 
                             ifelse(weekday=="3", ifelse(percentileY<0.95,1,0) ,
                                    ifelse(weekday=="4",0 
                                           , ifelse(retCOY<0,1,0))))) , ]

latest[, ls:= ifelse(weekday=="1", ifelse(CloseY>ma20,1,0), 
                       ifelse(weekday=="2", ifelse(retCCY < 0,1,0), 
                              ifelse(weekday=="3", ifelse(percentileY<0.95,1,0) ,
                                     ifelse(weekday=="4",0 
                                            , ifelse(retCOY<0,1,0))))) , ]