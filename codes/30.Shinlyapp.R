library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(rgdal)


can_lake <- st_read("./check_arcGIS/CA/can_lakes.shp")



ui = fluidPage(
  
  titlePanel("Properties distribution of Canada aroun Lakes"),
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(),
    mainPanel = mainPanel(leafletOutput(outputId = 'map'))
    
  )
)

server = function(input, output){
  
  map_df = reactive({

  }
  
  )
  
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles()%>%
      addPolygons(data = can_lakes, # borders of all counties
                  color = "blue", 
                  fill = NA, 
                  weight = 1.5 )
      #addCircleMarkers(data = map_df(), radius = ~sqrt(Dwllngs))
    
    
  })
  
  
  
}

shinyApp(ui, server)


