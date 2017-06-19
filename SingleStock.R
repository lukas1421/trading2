#Single stock
shennong<-fread("F:/002299.csv")
shennong[,D:=ymd(D)]
shennong[,retCO:=log(C/O)]
shennong[,percentile:=(C-L)/(H-L)]
shennong[,percentileY:=shift(percentile,1)]
shennong[is.na(percentileY), percentileY:=0.5]
shennong[is.na(retCC),retCC:=0]