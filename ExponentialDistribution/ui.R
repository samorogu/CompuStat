
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Histograma Distribución exponencial"),

  # Sidebar with a slider input for number of bins
  sidebarLayout( sidebarPanel(sliderInput("simulaciones","simulaciones:", min = 1, max = 5000,value = 1000),
  numericInput("lambda", "lambda",value = .5),
  tableOutput("table")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
