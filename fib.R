fib <- function(n) {
  if(n<=1) {
    return(1)
  } else{ 
    return(fib(n-1)+fib(n-2))
  }
}

fib1 <- function(n,val1,val2, res) {
  if(n<=1) {
    return(1)
  } else {
    return(fib1(n-1, ))
  }
  
}


adder1<- function(n,res) {
  if(n==0) {
    return(res)
  } else {
    return(adder1(n-1, n+res))
  }
}

fact1 <- function(n, res) {
  if(n==0) {
    return(0)
  } else if(n==1) {
    return(res)
  } else {
    return (fact1(n-1, n*res))
  }
}