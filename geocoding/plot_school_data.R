#Load libraries needed
require(RODBC)
library(RColorBrewer)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
#install_github("easyGgplot2", "kassambara")
library(easyGgplot2)

#**************************Plotting maps with Google maps*************************************************************
source("find_school_district_by_address.R")

#filepath to zone distribution 2013-2014
filepath <- "2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)
boundariesandschools <- merge(school_zone_boundaries@data, schooldata, by = "DBN", all.y=TRUE)
boundariesandschools <- boundariesandschools[!is.na(boundariesandschools$`% Asian`) &
                                               !is.na(boundariesandschools$`% Black`) & !is.na(boundariesandschools$`% White`) &
                                               !is.na(boundariesandschools$`% Hispanic`),]

#join school_zone_boundaries with English test
elementary_ps <- inner_join(schools_df, boundariesandschools)
nyc_map <- create_city_basemap("New York, NY", -74.00, 40.71)
# Plot by Asian distribution in school districts
nyc_school_map_asian <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% Asian`), 
                                                      size=.2, color="black", 
                                                      data=elementary_ps, alpha=.8) + ggtitle("Asian") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_asian

# Plot by Black distribution in school districts
nyc_school_map_black <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% Black`), 
                                                      size=.2, color="black", 
                                                      data=elementary_ps, alpha=.8) + ggtitle("Black") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_black

# Plot by White distribution in school districts
nyc_school_map_white <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% White`), 
                                                      size=.2, color="black", 
                                                      data=elementary_ps, alpha=.8) + ggtitle("White") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_white

# Plot by Black distribution in school districts
nyc_school_map_hispanic <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% Hispanic`), 
                                                         size=.2, color="black", 
                                                         data=elementary_ps, alpha=.8) + ggtitle("Hispanic") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_hispanic

#plot asian,black,hispanic and white maps in one page
ggplot2.multiplot(nyc_school_map_asian, nyc_school_map_black, nyc_school_map_white, nyc_school_map_hispanic, cols=2)

