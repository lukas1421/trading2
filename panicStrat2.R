
# get moving average and deviation from moving average

library(data.table)
library(zoo)

symb <- "sz399006"
d <- getDataPureD(symb)
print(d)
maPeriod <- 20
d[, eval(paste0("ma",maPeriod)):= rollmean(C,maPeriod,align = "right",fill = NA), by=list(D)]
d[, eval(paste0("deviation",maPeriod)):= get(paste0("ma",maPeriod))/L-1]
