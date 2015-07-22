library(readxl)
require(RODBC)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(devtools)
library(easyGgplot2)

source("find_school_district_by_address.R")
## Renaming the first row name


#filepath to zone distribution 2013-2014
filepath <- "2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)
#************************************************** adding DBN******************************************************
#address_data <- get_addresses(input_addresses_file)
#complete_listing data was used
address_data <- complete_listings[!(is.na(complete_listings$latitude)| is.na(complete_listings$longitude)), ]
coordinates(address_data)<- ~longitude + latitude
proj4string(address_data) <- proj4string(school_zone_boundaries)

# Match each address to a school zone. 
matched_school_zones <- over(address_data, school_zone_boundaries)

# create the final merged df with both address and school zone information
merged_data <- cbind(address_data, matched_school_zones)

#*****************************************************************************************************************
# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)

boundariesandschools2 <- merge(school_zone_boundaries@data, merged_data, by = "DBN", all.y=TRUE)
boundariesandschools2 <- boundariesandschools2[!is.na(boundariesandschools2$price),]
elementary_ps2 <- inner_join(schools_df, boundariesandschools2)

# Create base map for NYC
nyc_map <- create_city_basemap("New York, NY", -74.00, 40.71)
# Plot English mean scale score
nyc_school_map_price_apt <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= log(price)), 
                                                    size=.2, color="black", 
                                                    data=elementary_ps2, alpha=.8) + ggtitle("Complete Listing") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_price_apt

#sold_listing data was used
address_data <- complete_listings[!(is.na(sold_listings$latitude)| is.na(sold_listings$longitude)), ]
coordinates(address_data)<- ~longitude + latitude
proj4string(address_data) <- proj4string(school_zone_boundaries)

# Match each address to a school zone. 
matched_school_zones <- over(address_data, school_zone_boundaries)

# create the final merged df with both address and school zone information
merged_data <- cbind(address_data, matched_school_zones)

#*****************************************************************************************************************
# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)

boundariesandschools3 <- merge(school_zone_boundaries@data, merged_data, by = "DBN", all.y=TRUE)
boundariesandschools3 <- boundariesandschools3[!is.na(boundariesandschools3$price),]
elementary_ps3 <- inner_join(schools_df, boundariesandschools2)

# Create base map for NYC
nyc_map <- create_city_basemap("New York, NY", -74.00, 40.71)
# Plot English mean scale score
nyc_school_map_sold <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= log(price)), 
                                                          size=.2, color="black", 
                                                          data=elementary_ps3, alpha=.8) + ggtitle("Sold Data") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_sold

ggplot2.multiplot(nyc_school_map_price_apt, nyc_school_map_sold, col = 2)
