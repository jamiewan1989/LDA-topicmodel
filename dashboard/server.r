library(visNetwork)
library(shiny)
library(igraph)

server <- function(input, output,session) {
  
  nps_cats <-list("promoter" = "promoter","detractor" = "detractor")
    
  observe({
    backbone <- readRDS(file = sprintf("%s.rda", input$selection))
    g_backbone_vis <- toVisNetworkData(backbone)
    output$network <- renderVisNetwork({
      nodes = g_backbone_vis$nodes
      visNetwork(nodes, edges = g_backbone_vis$edges)
    })
  })

}
