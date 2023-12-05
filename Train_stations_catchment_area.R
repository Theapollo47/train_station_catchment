libs <- c('viridis', 'sf','stars','tidyverse','raster',
          'terra','ggplot2','osmdata','httr','showtext','leaflet') #needed libraries

installed_libs <- libs %in% rownames(installed.packages())

if(any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
  } 

invisible(lapply(libs,library,character.only = T))

install.packages('remotes')


remotes::install_github('GIScience/openrouteservice-r')

 #load data

nigeria <- st_read('\\NGA_adm0.shp') #Nigeria administrative boundary

nga_pop_url <- "https://data.worldpop.org/GIS/Population/Individual_countries/NGA/Nigeria_100m_Population/NGA_ppp_v2c_2020_UNadj.tif" 

nga_pop <- raster(nga_pop_url) #population data

#Extract Train stations within Nigeria

building_tags <- c('station') #open street maps tag for train station

get_osm_stations <- function() {
  bbox <- sf::st_bbox(nigeria)
  train_stations <- bbox |>
    opq() |>
    add_osm_feature(
      key = c("railway"),
      value = building_tags
    ) |>
    osmdata::osmdata_sf()
  
  return(train_stations)
} #create a function to extract train stations in Nigeria

train_stations_main <- get_osm_stations() #Assign to a new object

train_stations_main <- train_stations_main$osm_points #assign points as new column

#Extract Railway lines within Nigeria
railway_tags <- c('light_rail','monorail','rail') #OSM tags for railway types
get_osm_railways <- function() {
  bbox <- st_bbox(nigeria)
  rails <- bbox |>
    opq() |>
    add_osm_feature(
      key = "railway",
      value = railway_tags
    ) |>
    osmdata::osmdata_sf()
  
  return(rails)
}

nigeria_rails <- get_osm_railways()

nigeria_rails_main <-nigeria_rails$osm_lines

#extract lines of interest (standard guage)
standard_track_gauge<-nigeria_rails_main %>% dplyr::filter(gauge == 1435) #filter standard gauge railway lines


#extract train stations along lines of interest
standard_track_gauge_utm <- st_transform(standard_track_gauge,crs=32633) #change coordinate reference system to utmWGS84
train_stations_utm <- st_transform(train_stations_main,crs=32633)

rail_buffer <- st_buffer(standard_track_gauge_utm,dist=1000)

rail_buff_union <- st_union(rail_buffer)

stations_online <- sf::st_intersection(train_stations_utm,rail_buff_union) #extract stations that fall within buffer

stations_oi <- c('Kaduna - Rigasa')

stations_interest <-stations_online %>% dplyr::filter(name %in% stations_oi | wikidata == 'Q110556255') 
stations_interest_wgs <- st_transform(stations_interest,crs=4326) #transform to a geographic coordinate system

stations_interest_coord <- as.data.frame(st_coordinates(stations_interest_wgs))

additional_stations <- data.frame(X = c(3.3736285,3.8966097),Y=c(6.5016828,7.5593900))

stations_interest_coord <- bind (stations_interest_coord,additional_stations)

#Generate Isochrones

api_key <- '5b3ce3597851110001cf62480a2c024e6d0c4102b7da8660e05e147d'

isochrones <- openrouteservice::ors_isochrones(
  locations = stations_interest_coord,
  profile = "driving-car" ,
  range = 1200,
  interval=300,
  api_key = api_key,
  output = 'sf'
)

#crop population density of catchment areas

catchment_pop <-raster::crop(raster::mask(nga_pop,isochrones),isochrones)


#Interactive Map

viridis_palette <- viridis(100) #colour palette

reversed_palette <- rev(viridis_palette) # Reverse the color palette

leaff <- leaflet() %>% 
  setView(lng = 7.342506, lat = 9.046976, zoom = 10) %>% 
  addProviderTiles('OpenStreetMap.Mapnik') %>% 
  addRasterImage(catchment_pop, opacity = 0.7, color = reversed_palette) %>% 
  addLegend(
    position = 'bottomright',
    pal = colorNumeric(palette = 'viridis', domain = values(catchment_pop),reverse = TRUE),
    values = na.omit(values(catchment_pop)),
    opacity = 0.7,
    title = 'People Who Can Drive To Station in 20 Mins') %>% 
    addMarkers(lng = c(7.354936,7.342506), lat = c(10.548712,9.046976),
               popup = c('Kaduna-Rigasa Station','Idu Station'))


leaff






