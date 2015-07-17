library(ggmap)
library(RgoogleMaps)
library(rgdal)
library(ggplot2)
library(stringr)


create_city_basemap <- function(cityname) {
  # Creates a map of a location
  
  centre_map <- geocode(cityname, override_limit = TRUE)
  city_mapdata <- get_map(c(lon=centre_map$lon, lat=centre_map$lat),zoom = 10, maptype = "terrain", source = "google")
  return(city_mapdata)
}

create_school_mapdata <- function(filepath, shapefile_name){
  # Returns the mapdata (area polygons) for school zones
  
  schools_shapefile = readOGR(dsn=filepath, layer=shapefile_name)
  t_shapefile<- spTransform(schools_shapefile, CRS("+proj=longlat +datum=WGS84"))
  return(t_shapefile)
}

get_addresses <- function(address_input_df){
  # Reads a csv file with addresses and adds their latitude and longitude
  addresses <- paste(str_trim(address_input_df$ADDRESS),"New York", address_input_df$ZIP_CODE, sep=" ")
  latlong_df <- geocode(addresses)
  address_df <- cbind(address_input_df, latlong_df)
  coordinates(address_df) <- ~ lon + lat
  return(address_df)
}

Main <- function() {
  filepath <- "2013_2014_School_Zones_8May2013"
  shapefile <- "ES_Zones_2013-2014"
  input_addresses_file <- "street_addresses.csv"

  school_zone_boundaries <- create_school_mapdata(filepath, shapefile)
  
  #address_input_df<- read.csv(filename)
  load('../sales.RData')
  address_input_df <- filter(fullSales, ZIP_CODE == 11215 | ZIP_CODE == 11217)
  address_data <- get_addresses(address_input_df)

  proj4string(address_data) <- proj4string(school_zone_boundaries)
  
  # Match each address to a school zone. 
  matched_school_zones <- over(address_data, school_zone_boundaries)
  
  # create the final merged df with both address and school zone information
  merged_data <- cbind(address_data, matched_school_zones)
  write.csv(merged_data, "park_slope_addresses_with_school_zones.csv")

  # Mapping school zones, for fun
  schools_df <- fortify(school_zone_boundaries)
  nyc_map <- create_city_basemap("New York, NY")
  nyc_school_map <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='red', data=school_zone_boundaries, alpha=.5)

}

