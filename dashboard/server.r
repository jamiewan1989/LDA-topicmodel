library(visNetwork)
library(shiny)
library(igraph)


nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
                    group = sample(LETTERS[1:3], 15, replace = TRUE))

edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
                    to = trunc(runif(15)*(15-1))+1)


server <- function(input, output,session) {
  
  nps_cats <-list("promoter" = "promoter","detractor" = "detractor")
    
  observe({
    backbone <- readRDS(file = sprintf("%s.rda", input$selection))
    g_backbone_vis <- toVisNetworkData(backbone)
    output$network <- renderVisNetwork({
      nodes = g_backbone_vis$nodes
      visNetwork(nodes, edges = g_backbone_vis$edges)%>%
         visEvents(click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}"
          )
    })
    output$shiny_return <- renderPrint({
    visNetworkProxy("network") %>%
      visNearestNodes(target = input$click)
    })
  })

}
