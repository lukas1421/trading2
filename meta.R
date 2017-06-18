
nested <- function() {
  y<- 5
  function(x) x
}

f <- nested()


nested <- function(z) {
  as.function(alist(x=,y=z,x+y))
}
nested(2)(3)

f <- function(x=5) {
  y <- 2*x
  sys.function()
}

sub <- function(x) {
  #substitute(x)
  match.call()
}

bind <- function(...) {
  bindings <- eval(substitute(alist(...)))
  scope <- parent.frame()
  structure(list(bindings=bindings, scope = scope), class="bindings")
}

f <- function() {
  env <- environment()
  print(environment())
  print(parent.frame())
  print(parent.frame(2))
  print(parent.env(env))
  parent.env(env) <- parent.frame()
  print(parent.env(env))
  print(parent.frame())
  x
}
  
g <- function(x) f()
g(2)
  

f<- function(x) {
  function(y) {
    substitute(x+y)
  }
}

h <- f(3)
h(2)




