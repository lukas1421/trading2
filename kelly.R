calcKelly<-function(x) {
  
  #ifelse(length(x[x!=0]) > 0, x<-x[x!=0], 0);
  
  
  m <- mean(x)
  downMean <- mean(x[x<0])
  upMean <- mean(x[x>0])
  maxUp <- max(x)
  maxDown <- min(x)
  probUp <- sum(x>0)/length(x)
  probDown <- sum(x<0)/length(x)
  kelly <- probUp - (probDown)/(abs(upMean/downMean))
  
  return(list(count=length(x),mean=sprintf("%.4f",m),upMean = sprintf("%.4f",upMean), downMean=sprintf("%.4f",downMean), maxUp=sprintf("%.4f",maxUp),
              maxDown=sprintf("%.4f",maxDown), probUp=sprintf("%.4f",probUp), 
              probDown=sprintf("%.4f",probDown),kelly=sprintf("%.4f",kelly), levFull=(abs(kelly/maxDown)),levHalf =(abs(kelly/2/maxDown)), 
              levQuarter=(abs(kelly/4/maxDown))))
}

#beta
