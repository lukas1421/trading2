# get ptf sharpe and incremental sharpe

calcSSSharpe <- function(symb) {
  d<-getDataPure(symb)
  d[,ret:=(C/shift(C,1)-1)]
  mean <- d[D>ymd("20161231"), mean(ret,na.rm=T)]
  sd <- d[D>ymd("20161231"), sd(ret,na.rm=T)]
  #print(paste0(" mean ",mean," sd ",sd))
  return(list(SR=(mean/sd), mean=mean, sd=sd))
}

ptf <- c("sz002415","sh600036","sh600660","sz000418","sh601238","sz002008","sh600519","sh600104","sz000651","sh601628")

ptfSmallTest<- c("sz002415","sh600036")

lapply("sh600900",calcSSSharpe)


#compare all sharpe
compareAllSharpYtd <- function() {
  d<- fread("C:\\Users\\LUke\\Desktop\\Trading\\test.txt",header = FALSE)
  d<- d[, c(V2,calcSSSharpe(V1)), keyby=list(V1)]
  return(d)
}

calcDailyMean <- function(symb) {
  d<-getDataPure(symb)
  d[,ret:=(C/shift(C,1)-1)]
  mean <- d[D>ymd("20161231"), mean(ret,na.rm=T)]
  return(mean)
}

calcDailyMeanSD <- function(symb) {
  #symbName <- (substitute(symb))
  
  d<-getDataPure(symb)
  d[,ret:=(C/shift(C,1)-1)]
  mean <- d[D>ymd("20161231"), mean(ret,na.rm=T)]
  sd <- d[D>ymd("20161231"), sd(ret,na.rm=T)]
  return(list(symb=symb,mean=(mean),sd=sd))
}

calcDailyHLSD <- function(symb) {
  d<-getDataPure(symb)
  d[,HLret:=H/L-1]
  d[, ret:=C/shift(C,1)-1]
  mean <- d[D>ymd("20161231"), mean(ret,na.rm=T)]
  sd <- d[D>ymd("20161231"), sd(ret,na.rm=T)]
  hlsd <- d[D>ymd("20161231"), sd(HLret,na.rm=T)]
  return(list(symb=symb,mean=(mean),sd=sd,hlsd=hlsd))
}

genReturnMatrix <- function(symb) {
  d<-getDataPure(symb)
  d[, ret:=C/shift(C,1)-1]
  res<-d[D>ymd("20161231"),list(D,ret)]
  names(res) <- c("D", symb)
  return(res)
}




res <- data.table()
for(x in ptf) {
  temp <- genReturnMatrix(x)
  if(length(res)==0) {
    res <- temp 
  } else {
    res <- merge(res, temp,by = "D")
  }
}

checkCorrel <- function(symb1,symb2) {
  d1 <- genReturnMatrix(symb1)
  d2 <- genReturnMatrix(symb2)
  res <- merge(d1,d2,by = "D")
  return(res[, cor(get(symb1),get(symb2))])
}


retMat <- matrix(nrow=10, ncol = 1)
dimnames(retMat) <- list(ptf)
for(i in seq_along(ptf)) {
  retMat[i,1] <- calcDailyMean(ptf[i])
}

generateCorMat <- function(ptf) {
  corMat <- matrix(nrow = 10,ncol = 10)
  dimnames(corMat) <- list(ptf,ptf)
  names(corMat) <- 
    for(i in seq_along(ptf)) {
      for(j in seq_along(ptf)) {
        print(paste0("i j ", i, j, ptf[i],ptf[j]))
        if(i==j) {
          corMat[i,j] <- var(res[, get(ptf[i])])
        } else {
          corMat[i,j] <- cov(res[,get(ptf[i])], res[, get(ptf[j])])
          print(cov(res[,get(x)], res[, get(y)]))
        }
      }
    }
  return(corMat)
}

weightMatrix <- matrix(0.1,nrow = 10)

t(weightMatrix) %*% corMat %*% weightMatrix   

t(weightMatrix) %*% retMat 



findSharpeForTanPtf <- function(ptf) {
  
  #ptf <- c("sz002415","sh600036","sh600660","sz000418","sh601238","sz002008","sh600519","sh600104","sz000651","sh601628")
  corMat<-generateCorMat(ptf)
  
  sigma.inv.mat <- solve(corMat)
  one.vec <- rep(1, nrow(corMat))
  rf <- 0
  mu.minus.rf <- retMat - rf*one.vec
  top.mat <- sigma.inv.mat%*% mu.minus.rf
  bot.val <- as.numeric(t(one.vec) %*% top.mat)
  t.vec <- top.mat[,1]/bot.val
  mean <- crossprod(t.vec,retMat)
  sd<-sqrt(as.numeric(t(t.vec) %*% corMat %*% t.vec ))
  print(t.vec)
  return(list(mean=mean,sd=sd,sr=mean/sd))
}


