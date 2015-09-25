
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("ImportanceSampling vs crude MonteCarlo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "N of simulations:",min = 100, max = 20000,value = 10000)
      #sliderInput("alpha", "alpha:",min = .01, max = .2,value = .05)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot2"),
      plotOutput("distPlot3")
    )
  )
))
