devtools::install_github("datastorm-open/visNetwork")

require(visNetwork)
require(shiny)

nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
                    group = sample(LETTERS[1:3], 15, replace = TRUE))

edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
                    to = trunc(runif(15)*(15-1))+1)

server <- function(input, output, session) {
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges, 
               height = "100%", width = "100%",
               main = "") %>%
      visEvents(click = "function(nodes){
                Shiny.onInputChange('click', nodes.nodes[0]);
                ;}"
      )
})
  
  output$shiny_return <- renderPrint({
    visNetworkProxy("network") %>%
      visNearestNodes(target = input$click)
  })
  }

ui <- fluidPage(
  visNetworkOutput("network"), 
  verbatimTextOutput("shiny_return")  
)

shiny::shinyApp(ui = ui, server = server)