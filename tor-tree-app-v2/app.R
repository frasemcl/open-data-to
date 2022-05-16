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
library(shinyWidgets)

#TO FIXXX - while editing I need to change my working dir to
# setwd('C:/Users/fmclaughlin/Documents/Projects/GitHub/open-data-to/tor-tree-app-v2')
#then back to
# setwd('C:/Users/fmclaughlin/Documents/Projects/GitHub/open-data-to')

# read data:
trees<-st_as_sf(read_csv("data/treesSF.csv"), coords = c("X", "Y"), crs = 4326)

# colours:                          
pal <- colorBin(palette = "Greens", domain = trees$DBH_TRUNK, bins = c(0,1,2,4,8,16,32,64,200))


# Define UI ----
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 14, left = 66,
                pickerInput("select", label = "Select Species", 
                            choices = unique(trees$COMMON_NAME), 
                            selected = unique(trees$COMMON_NAME),
                            options = list(`actions-box` = TRUE),
                            multiple = T),
                selectizeInput("select2", label = "Select Street(s)", 
                            choices = sort(unique(trees$STREETNAME)), 
                            selected = c("BLOOR ST E", "BLOOR ST W", "DANFORTH AVE"), multiple = TRUE,
                            options = NULL),
                
                
                sliderInput("rangeDBH", "Select DBH (cm) Range", min(0), max(trees$DBH_TRUNK),
                            value = range(trees$DBH_TRUNK), step = 5, sep = ""
                ),
                
                
  ),
  # https://shiny.rstudio.com/articles/tag-glossary.html
  absolutePanel(right = 6, bottom = 6,
                p("Street tree data from", tags$a(href="https://open.toronto.ca/dataset/street-tree-data/", "Open Data Toronto"),
                  "Source code", tags$a(href="https://github.com/frasemcl/open-data-to", "on GitHub"), style = "font-size: 80%")
  )
  # absolutePanel(bottom = 21, right = 6,
  #               img(src = "FO_LOGO.svg", height = 20)
  # )
)



# Define server logic ----
server <- function(input, output, session) {
  
  filteredData <- reactive({
    trees %>%
      filter(trees$DBH_TRUNK >= input$rangeDBH[1] &
               trees$DBH_TRUNK <= input$rangeDBH[2] &
               trees$STREETNAME %in% input$select2 &
               trees$COMMON_NAME %in% input$select)
  })
  
  
  ##TRY selectize server side? https://shiny.rstudio.com/articles/selectize.html
  
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
