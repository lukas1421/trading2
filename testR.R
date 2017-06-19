c()y=10
#comment
h=hist(dailyPricesData$dayMaxT1,xlim=c(9.5,15), breaks = c(9.5,10,10.5,11,11.5,12,13,13.5,14,14.5,15), col='BLUE', labels=TRUE)
h$density=h$counts/sum(h$counts)*100
plot(h,freq=FALSE, labels=TRUE)
hist(as.data.frame(dailyPricesData[which(dayMaxT1>14 & dayMinT1<10),])$dayMinT1)
mean(retCC[weekday==5 & c(0,weekday)[-length(c(0,weekday))]==4 & c(0,0,weekday)[1:length(weekday)]==3])
hist(dayMaxT1[weekday==5 & c(0,weekday)[-length(c(0,weekday))]==4 
              & c(0,0,weekday)[1:length(weekday)]==3 & newBullIndicator==1],labels=TRUE, freq=T)

library(caret)
names(getModelInfo())
names(dailyPricesData)[!names(dailyPricesData) %in% c('retCCY','monD','tueD') ]

gbm_1<- train(dailyPricesData[names(dailyPricesData) %in% c('retCCY','monD','tueD','wedD','thuD','retOPC','newBullIndicator')], dailyPricesData$crash003,method='gbm')

rpart_1<- train(dailyPricesData[names(dailyPricesData) %in% c('retCCY','monD','tueD','wedD','thuD','retOPC','newBullIndicator')], dailyPricesData$crash003,method='rpart')
  
logit_1 <- glm(dailyPricesData$crash003~retCCY+monD+tueD+wedD+thuD+friD+retOPC+amMaxT1+amMinT1+newBullIndicator, family="binomial")

t = ctree(dailyPricesData$lowAfter1400~retCCY+retOPC+monD+tueD+wedD+thuD+friD+newBullIndicator+amMaxT1+amMinT1+retCLY+retCLY2, controls=ctree_control(mincriterion = 0.99))

by(subset(apr8data[c('retCC','retCO','retOPC','retCL','retCH')],percentileY>0.8),list(retOPC[percentileY>0.8]>0, weekday[percentileY>0.8]), function(m) sapply(m, function(x) {c(mean(x),median(x),tail(cumprod(1+x),1), length(x),sd(x))}))

for(i in 2000:2014) { print(i); (fivenum(retCC[weekday==4 & Date > as.Date(ISOdate(i,1,1))]))}

for(i in 2000:2014) { print(i); by(subset(apr8data[c('retCC','retCO')], weekday==4 & Date > as.Date(ISOdate(i,1,1))),weekday, function(m) sapply(m, function(x) {c(mean(x),median(x),tail(cumprod(1+x),1), length(x),sd(x))}))}


m <- ksvm(apr17Data$percentileCat  ~ amFirst5+retOPC+retCHY+amClosePercentile+thuDummy, data=apr17Data)
p <- predict(m, apr17Data[c('amFirst5','retOPC','retCHY','amClosePercentile','thuDummy')], type= "response")
table(apr17Data$percentileCat,p)
table(apr17Data$percentileCat)

ggplot(apr17Data,aes(retPMCO))+geom_density(alpha=0.4)+facet_grid(weekday~retOPCCat)+geom_text(data=ddply(apr17Data,.(weekday,retOPCCat),summarise,med=sprintf("%.4f",median(retPMCO))),aes(x=-0.03, y=63, label=med))
> ddply(apr17Data,.(weekday,retOPCCat),summarise,med=sprintf("%.4f",median(retPMCO)))




