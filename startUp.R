


ddply(may8Data[weekday==2,],.(percentileYCat), summarise, 
      amMed=sprintf("%.4f",median(retAMCO)), pmMed=sprintf("%.4f",median(retPMCO)),
      coMed=sprintf("%.4f",median(retCO)), amCum=sprintf("%.4f",tail(cumprod(1+retAMCO),1)),
      pmCum=sprintf("%.4f",tail(cumprod(1+retPMCO),1)),dayCum=sprintf("%.4f",tail(cumprod(1+retCO),1)),
      length(retPMCOYCat), pmMin = median(pmMinT1), pmMax = median(pmMaxT1))

ddply(may8Data[weekday==2,],.(percentileYCat, amFirst5Cat), summarise, 
      amMed=sprintf("%.4f",median(retAMCO)), pmMed=sprintf("%.4f",median(retPMCO)),
      coMed=sprintf("%.4f",median(retCO)), amCum=sprintf("%.4f",tail(cumprod(1+retAMCO),1)),
      pmCum=sprintf("%.4f",tail(cumprod(1+retPMCO),1)),dayCum=sprintf("%.4f",tail(cumprod(1+retCO),1)),
      length(retPMCOYCat), pmMin = median(pmMinT1), pmMax = median(pmMaxT1))

ddply(may8Data[weekday==1,],.(retPMCOYCat, amMaxT1PercentCat), summarise, 
      amMed=sprintf("%.4f",median(retAMCO)), pmMed=sprintf("%.4f",median(retPMCO)),
      coMed=sprintf("%.4f",median(retCO)), amCum=sprintf("%.4f",tail(cumprod(1+retAMCO),1)),
      pmCum=sprintf("%.4f",tail(cumprod(1+retPMCO),1)),dayCum=sprintf("%.4f",tail(cumprod(1+retCO),1)),
      length(retPMCOYCat), pmMin = median(pmMinT1), pmMax = median(pmMaxT1))


#whole hour 
ddply(d, .(weekday), summarise, am=sprintf("%.4f",mean(retAMCO)), am1h=sprintf("%.4f",mean(retAMFirstHour)), am2h=sprintf("%.4f",mean(retAMLastHour)),tam=sprintf("%.4f",t.test(retAMCO)$p.value),tam1h=sprintf("%.4f",t.test(retAMFirstHour)$p.value),tam2h=sprintf("%.4f",t.test(retAMLastHour)$p.value), pm=sprintf("%.4f",mean(retPMCO)),pm1h=sprintf("%.4f",mean(retPMFirstHour)),pm2h=sprintf("%.4f",mean(retPMLastHour)), tpm=sprintf("%.4f",t.test(retPMCO)$p.value),tpm1h=sprintf("%.4f",t.test(retPMFirstHour)$p.value),tpm2h=sprintf("%.4f",t.test(retPMLastHour)$p.value))

#


#min/max time
ddply(d, .(weekday, amFirst5Cat),summarise, amMin=median(amMinT1), amMax=median(amMaxT1), pmMin=median(pmMinT1),pmMax=median(pmMaxT1))

#am halves detail return
ddply(d[weekday==2,],.(amFirst5Cat), summarise, am1h = sprintf("%.4f",mean(retAMFirstHalf)), am2h = sprintf("%.4f",mean(retAMSecondHalf)), am3h = sprintf("%.4f",mean(retAMThirdHalf)), am4h=sprintf("%.4f",mean(retAMFourthHalf)))

#AM/PMhalves return
ddply(d[weekday==2,],.(percentileYCat,amClosePercentileCat,amFirst5Cat), summarise, 
    am1h = sprintf("%.4f",mean(retAMFirstHalf)), am2h = sprintf("%.4f",mean(retAMSecondHalf)), 
    am3h = sprintf("%.4f",mean(retAMThirdHalf)), am4h=sprintf("%.4f",mean(retAMFourthHalf)),
    am=sprintf("%.4f",mean(retAMCO)),pm1h = sprintf("%.4f",mean(retPMFirstHalf)), pm2h = sprintf("%.4f",mean(retPMSecondHalf)),
    pm3h = sprintf("%.4f",mean(retPMThirdHalf)), pm4h=sprintf("%.4f",mean(retPMFourthHalf)), pm=sprintf("%.4f",mean(retPMCO)))

#PM Half with t-stat



#15 min conditional max
ddply(d[weekday==2,],.(retPMCOYCat,amClosePercentileCat), summarise, uncond=median(pmMaxT1[pmMaxT1>13]), m1315=median(pmMaxT1[pmMaxT1>13.25]), m1330=median(pmMaxT1[pmMaxT1>13.5]), m1345=median(pmMaxT1[pmMaxT1>13.75]),m1400=median(pmMaxT1[pmMaxT1>14]),m1415=median(pmMaxT1[pmMaxT1>14.25]),m1430=median(pmMaxT1[pmMaxT1>14.5]),m1445=median(pmMaxT1[pmMaxT1>14.75])   )

#15 min conditional min
ddply(d[weekday==2,],.(retPMCOYCat,amClosePercentileCat), summarise, uncondMax=sprintf("%.2f",median(pmMaxT1[pmMaxT1>13])),uncondMin=sprintf("%.2f",median(pmMinT1[pmMinT1>13])), max1315=sprintf("%.2f",median(pmMaxT1[pmMaxT1>13.25])),min1315=sprintf("%.2f",median(pmMinT1[pmMinT1>13.25])), max1330=sprintf("%.2f",median(pmMaxT1[pmMaxT1>13.5])),min1330=sprintf("%.2f",median(pmMinT1[pmMinT1>13.5])), max1345=sprintf("%.2f",median(pmMaxT1[pmMaxT1>13.75])),min1345=sprintf("%.2f",median(pmMinT1[pmMinT1>13.75])),max1400=sprintf("%.2f",median(pmMaxT1[pmMaxT1>14])),max1415=sprintf("%.2f",median(pmMaxT1[pmMaxT1>14.25])),max1430=sprintf("%.2f",median(pmMaxT1[pmMaxT1>14.5])),max1445=sprintf("%.2f",median(pmMaxT1[pmMaxT1>14.75]))   )


cppFunction(
  ' double sumC(NumericVector x) {
      int n = x.size();
      double total = 0 ;
      for (int i = 0; i<n; ++i) {
        total+= x[i];
      
      }
      return total;
      }
  '
  
)

sumR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}

fibR <- function(n) {
  if (n==1 | n==0) {
    #print(n)
    1
  } else {
    #print(n)
    fibR(n-1)+fibR(n-2)
  }
}
  

cppFunction(
  ' int fibC(int x) {
    if (x == 0 || x==1) {
      return 1 ;
    } else {
      return fibC(x-1)+fibC(x-2);
    }
  }
  '
)
cppFunction(
  ' double fibC1(int x) {
  double temp1 = 1;
  double temp2 = 1;
  double total = 0;
  
  if (x == 0 || x == 1) {
  return 1;
  }    
  
  
  for(double i = 2 ; i < x+1; i++) {
  total = temp1+temp2;
  temp2= temp1;
  temp1= total;
  }
  return total;
  }
  '
)

cppFunction(
  ' double percentileRank1(int x,  NumericVector vec) {

    double counter = 1;

    for(int i = 1 ; i < vec.size(); i++) {
      if (x > vec[i]) counter ++;    
    }
    return counter/vec.size();
  }
  '
)

a<-(aug22[Date>ymd("2014-9-15") & Date<ymd("2016-10-20"),list(Date,Open,High,Low,Close)])
chartSeries(xts(a[,list(Open,High,Low,Close)],order.by = a$Date))
