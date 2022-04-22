library(tidyverse)
library(opendatatoronto)
library(leaflet)
library(leaflet.extras)
library(sf)
library(RColorBrewer)

###################################################
# Fraser McLaughlin         ```````````# April 2022

# FORMAT DATA FOR SHINY TORONTO STREET TREE MAP APP
###################################################


# found the following using: https://open.toronto.ca/dataset/street-tree-data/

# get package
package <- show_package("6ac4569e-fd37-4cbc-ac63-db3624c5f6a2")
# # get all resources for this package
resources <- list_package_resources("6ac4569e-fd37-4cbc-ac63-db3624c5f6a2")
# my chosen resource to download, id from 'resources'
streetTrees<-get_resource('b65cd31d-fabc-4222-83ef-8ddd11295d2b')

# Didn't use the following from city's instructions:
# # identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
# datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
# # load the first datastore resource as a sample
# data <- filter(datastore_resources, row_number()==1) %>% get_resource()


# Filtering out the biggest and smallest trees to make size more manageable and remove anomalies
streetTreesFilt<-streetTrees %>% 
  filter(!is.na(DBH_TRUNK), DBH_TRUNK>0, DBH_TRUNK<=150) 
  # arrange(desc(DBH_TRUNK))
# biggest<-streetTreesFilt %>% 
#   slice_head(n =10)


# # Testing to get this sub right
# geom <- "-79.386029074939, 43.7266807113692"
# (Long<-sub(", .*", "", geom))
# (Lat<-sub(".*, ", "", geom))

#Formatting the data and selecting columns I'm interested in:
treesSFprocess<-streetTreesFilt %>%
  filter(!is.na(COMMON_NAME), COMMON_NAME != "") %>%
  arrange(COMMON_NAME) %>% 
  mutate(geom = sub("\\).*", "", sub(".*\\(", "", geometry)) ) %>% 
  mutate(Longitude=sub(", .*", "", geom),
         Latitude=sub(".*, ", "", geom)) %>% 
  select(OBJECTID, ADDRESS, STREETNAME, COMMON_NAME, DBH_TRUNK, Longitude, Latitude)

#CSV to Simple Feature, and mutating two new columns to use with Leaflet
treesSF<-treesSFprocess %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  mutate(popup = paste0("<strong>Species: </strong>",COMMON_NAME,
                        "<br><strong>Address: </strong>",ADDRESS, " ",STREETNAME,
                        "<br><strong>DBH (cm): </strong>",DBH_TRUNK
                        ),
         radius = ifelse(DBH_TRUNK <=10, 3,
                         ifelse((DBH_TRUNK > 10 & DBH_TRUNK <=50),4,
                                ifelse((DBH_TRUNK > 50 & DBH_TRUNK <=100),5,
                                       ifelse((DBH_TRUNK > 100 & DBH_TRUNK <=150),6,7))))
  )


# LEAFLET
# This will actually be called in Shiny App, but I like to explore what it will look like here first:

# colours: https://rstudio.github.io/leaflet/colors.html
# https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=5
# display.brewer.all()
# pal <- colorNumeric(palette = colorRamp(c("#66c2a4", "#006d2c"), interpolate = "spline"), domain = treesSF$DBH_TRUNK)
pal <- colorNumeric(palette = "Greens", domain = treesSF$DBH_TRUNK)

leaflet(treesSF[1:10000,]) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(radius =  ~radius,
                   stroke = T,
                   color = "#696969",
                   weight = 0.5,
                   opacity = 0.85,
                   popup = ~popup,
                   fillColor = ~pal(DBH_TRUNK),
                   fillOpacity = .75) %>% 
  addLegend("bottomleft", pal = pal, values = ~DBH_TRUNK,
            title = "Tree Trunk<br>DBH (cm)",
            opacity = 1) %>% 
  leaflet.extras::addSearchOSM(options = searchOptions(collapsed = T, hideMarkerOnCollapse = T)) %>%
  addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                                 autoCenter = TRUE, maxZoom = NULL, 
                                                 setView = FALSE))
  
  # https://github.com/bhaskarvk/leaflet.extras/issues/158



 
# EXPORT back to a CSV, that will be read into the Shiny App:
st_write(treesSF, "./data/treesSF.csv", layer_options = "GEOMETRY=AS_XY")
# Or here, depending on where my wd is at the time:
# st_write(treesSF, "./shiny-tree-app/data/treesSF.csv", layer_options = "GEOMETRY=AS_XY")




#Process work:
ggplot(streetTreesFilt, mapping = aes(x = DBH_TRUNK))+
  geom_histogram()






