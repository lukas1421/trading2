#Update
may8Data$weekday <- factor(may8Data$weekday)
may8Data$amFirst5Cat
may8Data$percentileYCat <- cut(may8Data$percentileY, c(0,0.226,0.6,0.905,1),include.lowest = T)

