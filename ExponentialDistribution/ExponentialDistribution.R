Exp<-function(nsim,l=.5,X0=1){
  X=c(X0,numeric(nsim))
  for(i in 1:(nsim)){
    u<-runif(1)
    X[i+1] <- -(1/l*log(1-u))
  }
  return (X)
}
