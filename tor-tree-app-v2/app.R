# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# about: Shiny app to explore Toronto Street trees from OpenDataTo      #
# author: Fraser MCLAUGHLIN (fraser.mclaughlin@outlook.com)             #
# date: April 2022                                                        #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(leaflet.extras)
library(RColorBrewer)



# read data:
trees<-st_as_sf(read_csv("data/treesSF.csv"), coords = c("X", "Y"), crs = 4326)

# colours:                          
pal <- colorBin(palette = "Greens", domain = trees$DBH_TRUNK, bins = c(0,1,2,4,8,16,32,64,200))


# Define UI ----
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 14, left = 66,
                selectInput("select", label = "Select Species", 
                            choices = unique(trees$COMMON_NAME), 
                            selected = "Cedar, white"),
                sliderInput("rangeDBH", "DBH (cm)", min(0), max(trees$DBH_TRUNK),
                            value = range(trees$DBH_TRUNK), step = 5, sep = ""
                ),
                
                
  ),
  # https://shiny.rstudio.com/articles/tag-glossary.html
  absolutePanel(left = 6, bottom = 0,
                p("Street tree data from", tags$a(href="https://open.toronto.ca/dataset/street-tree-data/", "Open Data Toronto")),
                p("Source code", tags$a(href="https://github.com/frasemcl/open-data-to", "on GitHub"))
  ),
  absolutePanel(bottom = 21, right = 6,
                img(src = "FO_LOGO.svg", height = 20)
  )
)



# Define server logic ----
server <- function(input, output, session) {
  
  filteredData <- reactive({
    trees %>%
      filter(trees$DBH_TRUNK >= input$rangeDBH[1] &
               trees$DBH_TRUNK <= input$rangeDBH[2] &
               trees$COMMON_NAME == input$select)
  })
  
  output$mymap <- renderLeaflet({
    leaflet(trees) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-79.3804178128054, 43.7078201134622, zoom = 11) %>%
      # addLegend("bottomleft", pal = pal, values = ~DBH_TRUNK,
      #           title = "Tree Trunk<br>DBH (cm)",
      #           opacity = 1) %>% 
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = T, hideMarkerOnCollapse = T)) %>%
      addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                         autoCenter = TRUE, maxZoom = NULL, 
                                         setView = FALSE))
  })
  
  observe({
    
    leafletProxy("mymap", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(radius =  ~radius,
                       stroke = T,
                       color = "#696969",
                       weight = 0.5,
                       opacity = 0.85,
                       popup = ~popup,
                       fillColor = ~pal(DBH_TRUNK),
                       fillOpacity = .75)
  })
  
}


# Run the app ----
shinyApp(ui, server)