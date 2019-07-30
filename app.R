#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)
library(visNetwork)
# library(pacman)

bk_color <- "#252525"
grey_color <- "#565656"

source("_textprocessor.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  includeCSS("styles.css"),
  
  # Application title
  titlePanel("CohViz-E"),
  
  # tags$div(id = "clippath",
  #   HTML("<strong>Raw HTML!</strong>")
  # ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textAreaInput("caption", "Your Text", "Paste your text here",
                    height = "400px"),
      sliderInput("obs", "Threshold:",
                  min = 1, max = 10, value = 1
      ),
      actionButton("button", "Analyze text")
    ),
    

    
    # Show a plot of the generated distribution
    mainPanel(
      visNetworkOutput("graph", height = "80vh")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$button, {
    output$graph <- renderVisNetwork({
      
      process_text(isolate(input$caption), isolate(input$obs))
      
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

