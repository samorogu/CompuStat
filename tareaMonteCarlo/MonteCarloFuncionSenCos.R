
#función densidad de una normal
normales<-rnorm(1000,0,1)

#f <- function(x) {1/sqrt(2*pi)*exp(-sum(x[]^2)/2)}

f <- function(x) {
  1/sqrt(2*pi)*exp(-(x^2/2))
  }

funcionCuad<-function(x){
  x
}

 par(mar=c(2,2,2,1),mfrow=c(2,1))
 curve(f, xlab="",ylab="",lwd=2,from=-2,to=2)
 
 
 integrate(h,0,1)
 
 monteCarlo<-function(n=10000){
   x=f(runif(n))
   x=f(runif(n))
   
   theta=cumsum(x)/(1:n)
   varianza=sqrt(cumsum((x-estint)^2))/(1:n)
   
   return(data.frame(theta,varianza))
 }
 
 x=f(runif(10000))
 
 estint=cumsum(x)/(1:10000)
 
 esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
 
 #suma de rieman
 
 sumaRieman<-function(ecuacion,ini=-2,fin=2,n=200){
   x<-seq(from=ini,to=fin,by=fin-ini/n)
   ac<-0.0
   for(i in 1:(length(x)-1)){
     #print(ac)
     ac<-ac+(x[i+1]-x[i])*ecuacion(x[i])
  
     
   }
   return (ac)
 }
 
 muchasSumas<-function(m){
   sumaRieman(f,n=m)
 }
 
 plot(sapply(c(100:1000),muchasSumas))
 
 plot(estint,xlab="Mean and error range",type="l",lwd=2,ylim=mean(x)+20*c(-esterr[10^4],esterr[10^4]),ylab="")
lines(estint+2*esterr,col="gold",lwd=2)
 lines(estint-2*esterr,col="gold",lwd=2)
 