#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(GISTools)
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  mexico <- readShapeSpatial("./shape/final.shp")
  output$distPlot <- renderPlot({
    
      
      year <- input$year
      month <- input$month
      
      if(year==2016 && month > 10)
      {
        month <- 10
      }
      array <- paste('X',year,".",str_pad(month, 2, pad = "0"),".01",sep="")
      print(array)
      
      interest <- mexico[[array]]
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
