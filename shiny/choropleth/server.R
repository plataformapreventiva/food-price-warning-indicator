#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  mexico <- readShapeSpatial("/Users/agutierrez/Documents/maestria/lineal-generalized-models/food-price-warning-indicator/shape/final.shp")
  output$distPlot <- renderPlot({
    
      
      n <- input$n

    
    
    
    
      array <- names(mexico)[seq(3,length(names(mexico)))]
      interest <- mexico[[array[n]]]
      print(interest)
      col.min <- min(interest, na.rm=TRUE)
      col.max <- max(interest, na.rm=TRUE)
      max(interest, na.rm=TRUE)
      span <- col.max - col.min
      colors <- 9
      jump <- span / colors
      shades <- shading(breaks = seq(col.min,col.max,jump), cols = brewer.pal(colors, "Reds"))
      choropleth(mexico, v = interest, shades)
    
    
    
  })
  
})
