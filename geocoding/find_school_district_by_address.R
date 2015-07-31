library(ggmap)
library(RgoogleMaps)
library(rgdal)
library(ggplot2)
library(stringr)


create_city_basemap <- function(cityname, long=NULL, lat=NULL) {
  # Creates a map of a location
  if (!is.null(long)){
    centre_map = data.frame(lon=c(long), lat=c(lat))
  }
  else{
    centre_map <- geocode(cityname)
  }
  
  city_mapdata <- get_map(c(lon=centre_map$lon, lat=centre_map$lat),zoom = 12, maptype = "terrain", source = "google")
  return(city_mapdata)
}

create_school_mapdata <- function(filepath, shapefile_name){
  # Returns the mapdata (area polygons) for school zones
  
  shapefile = readOGR(dsn=filepath, layer=shapefile_name)
  t_shapefile<- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))
  return(t_shapefile)
}

get_addresses <- function(filename){
  # Reads a csv file with addresses and adds their latitude and longitude
  address_input_df<- read.csv(filename)
  addresses <- paste(str_trim(address_input_df$street_address),address_input_df$zipcode, sep=" ")
  latlong_df <- geocode(addresses)
  address_df <- cbind(address_input_df, latlong_df)
  coordinates(address_df) <- ~ lon + lat
  return(address_df)
}

Main <- function() {
  filepath <- "2013_2014_School_Zones_8May2013"
  shapefile <- "ES_Zones_2013-2014"
  input_addresses_file <- "street_addresses.csv"

  boundaries <- load_shapefile(filepath, shapefile)

  proj4string(address_data) <- proj4string(boundaries)  
  address_data <- get_addresses(input_addresses_file)
  
  # Match each address to a school zone. 
  matched_areas <- over(address_data, school_boundaries)
  
  # create the final merged df with both address and school zone information
  merged_data <- cbind(address_data, matched_areas)
  write.csv(merged_data, "matched_areas_with_points.csv")
  # Mapping school zones, for fun
  plot_df <- fortify(boundaries)
  nyc_map <- create_city_basemap("New York, NY")
  nyc_school_map <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='red', data=boundaries, alpha=.5)

}


