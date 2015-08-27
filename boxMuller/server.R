
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    n<- input$bins
    
    u = runif(n)
    v = runif(n)
    
    x=rep(0,n)
    y=rep(0,n)
    
    for (i in 1:n){
      x[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
      y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
    }
    

    # draw the histogram with the specified number of bins
    hist(c(x,y), col = 'lightblue', border = 'white')

  })

})
