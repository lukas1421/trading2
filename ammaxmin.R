


#amfirst5 cat 
#

wkdy <- 4
openCat <- "[-0.0248,-0.0017]"
yCat <- "[0,0.226]"
t <- seq(9.5, 11.4, 0.1)
df1<- data.frame(t)

#am
for(i in 1:length(df1$t)) {
  df1$ammax[i]<- median(amMaxT1[amMaxT1>df1$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat ]) 
  df1$ammin[i]<- median(amMinT1[amMinT1>df1$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat])
}

  
#pm
wkdy <- 4
openCat <- "[-0.0248,-0.0017]"
yCat <- "[0,0.226]"
t <- seq(13, 14.9, 0.1)
dfpm<- data.frame(t)

#am
for(i in 1:length(dfpm$t)) {
  dfpm$pmmaxL[i]<- median(pmMaxT1[pmMaxT1>dfpm$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat & amClosePercentileCat== "[0,0.2]"]); 
  dfpm$pmminL[i]<- median(pmMinT1[pmMinT1>dfpm$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat & amClosePercentileCat== "[0,0.2]"]);
  dfpm$pmmaxM1[i]<- median(pmMaxT1[pmMaxT1>dfpm$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat & amClosePercentileCat== "(0.2,0.5]"]); 
  dfpm$pmminM1[i]<- median(pmMinT1[pmMinT1>dfpm$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat & amClosePercentileCat== "(0.2,0.5]"]);
  dfpm$pmmaxM2[i]<- median(pmMaxT1[pmMaxT1>dfpm$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat & amClosePercentileCat== "(0.5,0.8]"]); 
  dfpm$pmminM2[i]<- median(pmMinT1[pmMinT1>dfpm$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat & amClosePercentileCat== "(0.5,0.8]"]);
  dfpm$pmmaxH[i]<- median(pmMaxT1[pmMaxT1>dfpm$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat & amClosePercentileCat== "(0.8,1]" ]); 
  dfpm$pmminH[i]<- median(pmMinT1[pmMinT1>dfpm$t[i] & weekday==wkdy & amFirst5Cat==openCat & percentileYCat == yCat & amClosePercentileCat== "(0.8,1]"]);
}


#j <- seq(-0.0688, 0.0837, 0.0001)




for (i in 1999:2016) {

yearStart <- i
yearEnd <- i
wkdy = 2

j <- seq(-0.03, 0.05, 0.0001)
res1 <- rep(0,length(j))

for (k in 1:length(j)) {
  ls <- 0
  ls <- ifelse(retPMCOY[weekday==wkdy & Date>ymd(paste(yearStart,"/1/1",sep="")) 
                        & Date<ymd(paste(yearEnd,"/12/31",sep=""))]<j[k],1,-1);

  res1[k]<-(tail(cumprod(1+retPMCO[weekday==wkdy & Date>ymd(paste(yearStart,"/1/1",sep="")) 
                                   & Date<ymd(paste(yearEnd,"/12/31",sep="")) ]*ls),1))
}
print(i)
print(max(res1))
print(j[max(res1)==res1])
plot(j,res1)
}



#retPMCL
j <- seq(0, 0.08, 0.0001)

ls1 <- rep(0,length(j))
res1 <- rep(0,length(j))

for (i in 1:length(j)) {
  ls <- 0
  ls <- ifelse(retPMCLY<j[i],1,-1);
  
  #res1[i] <<- 
  #print(j[i])
  res1[i]<-(tail(cumprod(1+retPMCO*ls),1))
  
}


#custom
j <- seq(0, 0.08, 0.0001)


ls1 <- rep(0,length(j))
res1 <- rep(0,length(j))


  ls <- 0
  
  if(retPMCOY<0.0035) {
    ls = 1
  } else {
    if(weekday == 1 | weekday==3 ){
      ls = 1
    } else {
      ls = -1
    }
    
  }
  
  ls <- ifelse(retPMCOY<0.0035, 1, ifelse(weekday==1 | weekday==3, 1,-1))
  
  may8Data$ls <- ifelse(weekday==1, ifelse(retPMCOY<0.0364,1,-1),ifelse(weekday==2,ifelse(retPMCOY<0.0077,1,-1) ,
    ifelse(weekday==3,ifelse(retPMCOY<0.0026,1,-1) , ifelse(weekday==4,ifelse(retPMCOY<0.0003,1,-1) , ifelse(weekday==5, ifelse(retPMCOY<0.0032, 1,-1),-1)))))
  
  may8Data$ratio <- ifelse(ls>=0, 1, ifelse(amFirst5>0, 0.8, 1 ))
  
  
  may8Data$lsThresh <- ifelse(weekday==1, 0.0364, ifelse(weekday==2, 0.0077, ifelse(weekday==3, 0.0026, ifelse(weekday==4, -0.0104,0.0032))))
  
  
  
  
  lsam <- ifelse(weekday==2 | weekday==4, -1, ifelse(weekday==1 | weekday==3, ifelse(percentileY<0.6, -1, 1), ifelse(percentileY>0.6,-1,1)))
  
  
