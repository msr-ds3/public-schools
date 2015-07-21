library(readxl)
require(RODBC)
schooltarget <- read_excel("schools/englishscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)

source("find_school_district_by_address.R")
library(RColorBrewer)
library(dplyr)

filepath <- "2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)


#lschools <- data.frame(DBN = elementaryschools$DBN, Environment = elementaryschools$`Environment Rating`)
boundariesandschools <- merge(school_zone_boundaries@data, schooltarget, by = "DBN", all.y=TRUE)


boundariesandschools <- boundariesandschools[!is.na(boundariesandschools$`Mean Scale Score`),]

elementary_ps <- inner_join(schools_df, boundariesandschools)

# Create base map for NYC
nyc_map <- create_city_basemap("New York, NY", -74.00, 40.71)
# Plot polygons for school zones over the base map
nyc_school_map <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `Mean Scale Score`), 
                                                size=.2, color="black", 
                                                data=elementary_ps, alpha=.8) +

scale_fill_continuous(low="blue", high="green")

# Show map!
nyc_school_map = nyc_school_map + scale_fill_brewer(palette = "GnBu")

nyc_school_map
