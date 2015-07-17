## Example code to plot a map
## Colors school zones by the borough in which they lie

source("find_school_district_by_address.R")

filepath <- "2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)
schools_df <- merge(schools_df, school_zone_boundaries)

# Create base map for NYC
nyc_map <- create_city_basemap("New York, NY", -74.00, 40.71)
# Plot polygons for school zones over the base map
nyc_school_map <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill=BORO), 
                                                size=.2,color='red', 
                                                data=schools_df, alpha=.5) 
# Show map!
nyc_school_map
