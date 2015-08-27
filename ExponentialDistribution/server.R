
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

Exp<-function(nsim,l=.5){
  X=c(1,numeric(nsim)-1)
  for(i in 1:(nsim)){
    u<-runif(1)
    X[i+1] <- -1*(log(1-u)/l)
  }
  return (X)
}


shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- Exp(input$simulaciones,input$lambda)
    bins <- seq(min(x), max(x), length.out = input$simulaciones + 1)

    # draw the histogram with the specified number of bins
    hist(x, col = 'darkgray', border = 'white')
    tabl<-t(t(sort(x,decreasing=FALSE))) 
    output$table <- renderTable(tabl)
  })

})
