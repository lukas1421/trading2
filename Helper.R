

findFirstOverCloseY <- function(Date) {
  return(list(Date));
}

#first time that crosses below amlow, out of all the cases where pmMin<amMin
#gets all mins of pm
a<-jun23[, c(amMin=amMin,mget(names(jun23)[grep("^L(13|14|15)", names(jun23))])), keyby=Date]

b<-a[,{t<-unlist(.SD); ifelse(is.na(which(t[2:122]<t[1])[1]), "LNA", names(t)[2:122][which(t[2:122]<t[1])[1]]);  } ,keyby=Date]

jun23[, LNA:=0]

jun23[, list(b$Date[.GRP], b$V1[.GRP], get(b$V1[.GRP]), Close, weekday),keyby=Date][V2!="LNA",]

jun23[, list(b$Date[.GRP], b$V1[.GRP], get(b$V1[.GRP]), Close, weekday,amFirst10, retPMCOYCat, amMax, CloseY),keyby=Date][V2!="LNA",
  ][,calcSharp(log(Close/V3)), keyby=list(weekday, retPMCOYCat, amMax<CloseY)]


percentileRank <- function(x, vec) {
    rank <- sum(x>vec)/length(vec)
    
    #normRank <- 
    return(list(rank=round(rank,2), mean=round(mean(vec)*10000,0)))
}
