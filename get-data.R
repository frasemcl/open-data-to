library(tidyverse)
library(opendatatoronto)
library(leaflet)
library(leaflet.extras)
library(sf)
library(RColorBrewer)

###################################################
# Fraser McLaughlin           ```````````# May 2022

# FORMAT DATA FOR SHINY TORONTO STREET TREE MAP APP
###################################################


# found the following using: https://open.toronto.ca/dataset/street-tree-data/

# get package
package <- show_package("6ac4569e-fd37-4cbc-ac63-db3624c5f6a2")
# # get all resources for this package
resources <- list_package_resources("6ac4569e-fd37-4cbc-ac63-db3624c5f6a2")
# my chosen resource to download, id from 'resources'
streetTrees<-get_resource('b65cd31d-fabc-4222-83ef-8ddd11295d2b')


#Set up a csv for user to choose which species they want to map. I am interested in native and naturalized trees. 
treeSpec<-as_tibble(unique(streetTrees$COMMON_NAME))
treeSpec<-treeSpec %>%
  rename(species = value) %>% 
  add_column(include = 0) %>% 
  arrange(species) %>% 
  filter(!is.na(species), species != '')
#Output the species for user to manually edit to include only species they want:
write.csv(treeSpec, 'process/selectSpecies.csv')
  
##########
# TO DO:##
##########
#Open up 'selectSpecies.csv' in the process folder and change from 0 to 1 all species you wan to map:
#WARNING your app will be very slow if you try to include all of them. 
#once that is done, save as 'selectedSpecies.csv' and proceed

selected <- read_csv('process/selectedSpecies.csv')
selected <- selected %>% 
  filter(include == 1)
streetTreesFilt <- selected$species


#Formatting the data and selecting columns I'm interested in, clunky first pass at dealing with the lat/long.
#Google street view suggests DBH above 200 is measurement error.
treesSFprocess<-streetTrees %>%
  filter(COMMON_NAME %in% streetTreesFilt,
         !is.na(DBH_TRUNK),
         DBH_TRUNK <= 200,
         DBH_TRUNK > 0) %>%
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
         radius = ifelse(DBH_TRUNK <=10, 5,
                         ifelse((DBH_TRUNK > 10 & DBH_TRUNK <=50),6,
                                ifelse((DBH_TRUNK > 50 & DBH_TRUNK <=100),7,
                                       ifelse((DBH_TRUNK > 100 & DBH_TRUNK <=150),8,9))))
  )


# LEAFLET
# This will actually be called in Shiny App, but I like to explore what it will look like here first:

# colours: https://rstudio.github.io/leaflet/colors.html
# https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=5
# display.brewer.all()
# pal <- colorNumeric(palette = colorRamp(c("#66c2a4", "#006d2c"), interpolate = "spline"), domain = treesSF$DBH_TRUNK)
pal <- colorBin(palette = "Greens", domain = treesSF$DBH_TRUNK, bins = c(0,1,2,4,8,16,32,64,200))


leaflet(treesSF[1:10000,]) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(radius =  ~radius,
                   stroke = T,
                   color = "#696969",
                   weight = 0.5,
                   opacity = 0.85,
                   popup = ~popup,
                   fillColor = ~pal(DBH_TRUNK),
                   fillOpacity = .75)%>% 
  # addLegend("bottomleft", pal = pal, values = ~DBH_TRUNK,
  #           title = "Tree Trunk<br>DBH (cm)",
  #           opacity = 1) 
  leaflet.extras::addSearchOSM(options = searchOptions(collapsed = T, hideMarkerOnCollapse = T)) %>%
  addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                                 autoCenter = TRUE, maxZoom = NULL, 
                                                 setView = FALSE))
  
  # https://github.com/bhaskarvk/leaflet.extras/issues/158



 
# EXPORT back to a CSV, that will be read into the Shiny App:
# You may have to delete the file first if it already exists.
st_write(treesSF, "./shiny-tree-app/data/treesSF.csv", layer_options = "GEOMETRY=AS_XY")




#Process work:
# ggplot(treesSF, mapping = aes(x = DBH_TRUNK))+
#   geom_histogram()






