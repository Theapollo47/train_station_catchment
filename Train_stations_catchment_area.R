libs <- c('viridis', 'sf','stars','tidyverse','raster',
          'terra','ggplot2','osmdata','httr','leaflet','gt') #needed libraries

installed_libs <- libs %in% rownames(installed.packages())

if(any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
  } 
getwd()
invisible(lapply(libs,library,character.only = T))

install.packages('remotes')


remotes::install_github('GIScience/openrouteservice-r')

 #load data

nigeria <- st_read('\\NGA_adm0.shp') #Nigeria administrative boundary

nga_pop_url <- "https://data.worldpop.org/GIS/Population/Individual_countries/NGA/Nigeria_100m_Population/NGA_ppp_v2c_2020_UNadj.tif" 

nga_pop <- raster(nga_pop_url) #population data
nga_pop <- rast("\\Users\\User\\Documents\\GIS\\Train_stations_catchment_area\\NGA_ppp_v2c_2020_UNadj.tif")
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
standard_track_gauge_light_rail<-nigeria_rails_main %>% dplyr::filter(gauge %in% c(1435,1067)) #filter standard gauge railway lines


#extract train stations along lines of interest
standard_track_gauge_utm <- st_transform(standard_track_gauge_light_rail,crs=32633) #change coordinate reference system to utmWGS84
train_stations_utm <- st_transform(train_stations_main,crs=32633)

rail_buffer <- st_buffer(standard_track_gauge_utm,dist=1000)

rail_buff_union <- st_union(rail_buffer)

stations_online <- sf::st_intersection(train_stations_utm,rail_buff_union) #extract stations that fall within buffer

stations_oi <- c('Abuja Metro','Kubwa','Kukwaba II','Gbazango')

stations_interest <-stations_online %>% dplyr::filter(name %in% stations_oi | wikidata == 'Q110556255') 
stations_interest_wgs <- st_transform(stations_interest,crs=4326) #transform to a geographic coordinate system

stations_interest_coord <- as.data.frame(st_coordinates(stations_interest_wgs))

additional_stations <- data.frame(name =c("Wupa","Gwagwa","Deidei","Kagini","Bassanjiwa"),X = c( 7.3948998,7.2851857,7.2872407,7.2921787,7.2824573),Y=c(9.0246825,9.0898296, 9.1061639, 9.1246465, 9.0136807)) 
additional_stations <- st_as_sf(additional_stations,coords = c("X","Y"), crs = 4326)

stations_interest_coord <- bind (stations_interest_coord,additional_stations)

#Generate Isochrones

api_key <- '***************' #type your api key

isochrones <- openrouteservice::ors_isochrones(
  locations = stations_interest_coord,
  profile = "foot-walking" ,
  range = 600,
  interval=300,
  api_key = api_key,
  output = 'sf'
)

#second Isochrnone due to API constraints
isochrones2 <- openrouteservice::ors_isochrones(
  locations = additional_stations,
  profile = "foot-walking" ,
  range = 600,
  interval=300,
  api_key = api_key,
  output = 'sf'
)

common_crs <- CRS('+init=epsg:4326')

#create a buffer of 800m and 400m for walkshed
station_buffer<- st_buffer(stations_interest,dist=800)
station_buffer2 <- st_buffer(additional_stations, dist = 800)


station_buffer_400 <- st_buffer(stations_interest, dist = 400)
station_buffer2_400 <- st_buffer(additional_stations, dist = 400)

#Populate a column with respective distances of each buffer for the purpose of displaying on legend
station_buffer <- station_buffer %>%  mutate(distance_t = 800)
station_buffer2 <-station_buffer2 %>%  mutate(distance_t = 800)


station_buffer_400 <- station_buffer_400 %>%  mutate(distance_t = 400)
station_buffer2_400 <- station_buffer2_400 %>%  mutate(distance_t =400)

station_buffer <-st_transform(station_buffer,common_crs)
station_buffer2 <st_transform(station_buffer2,common_crs)

station_buffer_400 <-st_transform(station_buffer_400,common_crs)
station_buffer_400 <- st_crs(station_buffer_400,station_buffer)

st_crs(station_buffer) == st_crs(station_buffer_400)

#Bind rows for walkshed
buffer_merged <- bind_rows(station_buffer,stati,station_buffer2,station_buffer2_400)


buffer_merged$distance_t <- factor(buffer_merged$distance_t)

buffer_merged <-buffer_merged %>% 
  select(name,distance_t)

buffer_grouped <-  buffer_merged %>% 
  dplyr::group_by(distance_t) 

#Bind rows for zonal statistics
buffer_merged2 <- bind_rows(station_buffer,station_buffer2)


#carry out zonal statistics for number of people within each walk shed
Zonal_sta <-terra::zonal(nga_pop,vect(buffer_merged2),sum,na.rm = T)

buffer_merged2$pop_dens <- Zonal_sta$NGA_ppp_v2c_2020_UNadj
buffer_merged2 <- buffer_merged2 %>% select(name,pop_dens)

buffer_merged2$pop_dens <- round(buffer_merged2$pop_dens)

#Make a table showing the population of people within each walk shed
buffer_merged2_df <- st_drop_geometry(buffer_merged2)


catchment_population <- gt(buffer_merged2_df) %>% 
  cols_label(name ="Station Name",
             pop_dens = "Catchment Population") %>% 
  tab_header(title = "Abuja Rail Mass Transit",
             subtitle = "How many people can take a 5 minute walk to a station?") %>% 
  tab_options(data_row.padding = px(6),
              heading.align = 'left',
              column_labels.background.color = '#114B5F',
              heading.title.font.size = px(26),
              ) %>% ab_style(
                style = list(
                  cell_text(font = "italic")
                ),
                locations = cells_title(groups = "subtitle")
              ) %>% 
  tab_source_note(md("*Author:@VictorA47*"))


col_pal <- leaflet::colorFactor("viridis",
                        domain = buffer_grouped$distance_t,
                        reverse = T
                        )
#Create a walk shed for all stations on the light rail line

leaflet(buffer_grouped) %>% 
  
  addPolygons(fill = T,
              stroke = T,
              weight = -3,
              fillColor = ~pal_fact(buffer_grouped$distance_t) ,
              fillOpacity = .5)%>% 
                
  addProviderTiles('OpenStreetMap.Mapnik') %>% 
  addLegend("bottomright",
            values = buffer_grouped$distance_t,
            labels = buffer_grouped$distance_t,
            title = "Area Within Walking Distance(m)",
            pal = pal_fact)
