
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)


is.intervals <- function(Phi, N, X.dens=runif, alpha=0.05){
  results.list <- lapply(N, function(nsim){
    X <- sapply(FUN=X.dens, nsim) # N samples of the density of X
    phiX <- sapply(X, Phi) # Evaluate phi at each X_i
    
    # Ahora sí
    estim <- mean(phiX)
    S2 <- var(phiX) # Estimate of the variance of phi(X_i)
    quant <- qnorm(alpha/2, lower.tail=FALSE) # Right quantile for alpha/2 int.upper <- estim + sqrt(S2/nsim)*quant # Upper confidence interval int.lower <- estim + sqrt(S2/nsim)*quant # Lower confidence interval return(data.frame(N=nsim, Estimate=estim, LI=int.lower, UI=int.upper)) # -------
    int.upper <- estim + sqrt(S2/nsim)*quant # Upper confidence interval 
    int.lower <- estim - sqrt(S2/nsim)*quant # Lower confidence interval 
    return(data.frame(N=nsim, Estimate=estim, LI=int.lower, UI=int.upper)) # -------
  })
  results.table <- ldply(results.list) # Assembles list in data.frame return(results.table)
}



shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    # Quiero integral de 0 a 2 de N(0,1)
    
    real <- pnorm(2) - 1/2
    real
    
    n <- 100
    N <- input$bins
    estim_mc <- rep(0,N)
    estim_is <- rep(0,N)
    for(i in 1:N){
      ### MonteCarlo crudo
      U <- runif(n, 0, 2) # Ojo: de 0 a 2
      phi <- function(x) 2*dnorm(x)
      estim_mc[i] <- mean(phi(U))
    
      # Exponencial (lambda = 1) truncada a [0,2] --> Usar Método de la Función Inversa
      U <- runif(n, 0, 1) # Ojo: de 0 a 1
      X <- -log(1 - (1 - exp(-2))*U)
      #   qplot(X)
      
      # Ahora sí
      fun <- function(x) dexp(x)/(1-exp(-2)) # Densidad de lambda truncada (sin indicadora)
      phi <- function(x) dnorm(x)/fun(x)
      estim_is[i] <- mean(phi(X))
    }
    
    dat <- rbind(
      data.frame(id='Monte Carlo', estim=estim_mc),
      data.frame(id='ImportanceSampling', estim=estim_is)
    )
    
    ggplot(dat) +
      ggtitle("MonteCarlo vs ImportanceSampling")+
      theme(plot.title = element_text(lineheight=.8, face="bold"))+
      geom_density(aes(estim)) +
      #geom_vline(aes(xintercept=real)) +
      facet_wrap(~id)
  })
  
  output$distPlot2 <- renderPlot({
    set.seed(110104)
    Phi <- function(x) 2*dnorm(x)
    X.dens <- function(nsim) runif(nsim, 0, 2)
    N <- seq(from=1000, to=input$bins, by=1000)
    data <- mc.intervals(Phi=Phi, N=N, X.dens=X.dens)
    data
    real <- pnorm(2) - 1/2
    ggplot(data, aes(x=N)) +
      ggtitle("Crude MonteCarlo estimation")+
      geom_ribbon(aes(ymin=LI, ymax=UI), fill="grey", alpha=.4) + geom_line(aes(y=Estimate), colour="blue") + geom_hline(aes(yintercept=real) , colour="red", linetype="dotted", size=1)
    
  })
  
  output$distPlot3 <- renderPlot({
    set.seed(110104)
    fun <- function(x) dexp(x)/(1-exp(-2))
    Phi <- function(x) dnorm(x)/fun(x)
    X.dens <- function(nsim){
      res<-runif(nsim, 0, 1)
      z <- -log(1 - (1 - exp(-2))*res)
      return(z)
    } 
    
    N <- seq(from=1000, to=input$bins, by=1000)
    data <- is.intervals(Phi=Phi, N=N, X.dens=X.dens)
    data
    real <- pnorm(2) - 1/2
    ggplot(data, aes(x=N)) +
      ggtitle(" Importance Sampling estimation")+
      geom_ribbon(aes(ymin=LI, ymax=UI), fill="grey", alpha=.4) + geom_line(aes(y=Estimate), colour="blue") + geom_hline(aes(yintercept=real) , colour="red", linetype="dotted", size=1)
    
  })

})
