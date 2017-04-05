library(shiny)
library(visNetwork)
fluidPage(
  # Application title
    titlePanel("Concept Mining"),
    
    sidebarPanel(
      selectInput("selection", "Choose a group:",
                  choices = nps_cats)
    ),
    
  mainPanel(
    visNetworkOutput("network",height = "1000px")
    verbatimTextOutput("shiny_return")
  )
)
