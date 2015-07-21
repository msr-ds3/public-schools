library(readxl)
require(RODBC)
library(RColorBrewer)
library(dplyr)

source("find_school_district_by_address.R")

#read english and math data
schooltarget_english <- read_excel("../schools/englishscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)
schooltarget_math <- read_excel("../schools/mathscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)

#filepath to zone distribution 2013-2014
filepath <- "2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)


#lschools <- data.frame(DBN = elementaryschools$DBN, Environment = elementaryschools$`Environment Rating`)
boundariesandschools <- merge(school_zone_boundaries@data, schooltarget_english, by = "DBN", all.y=TRUE)
boundariesandschools <- boundariesandschools[!is.na(boundariesandschools$`Mean Scale Score`),]

#join school_zone_boundaries with English test
elementary_ps <- inner_join(schools_df, boundariesandschools)

#join school_zone_boundaries with math test
boundariesandschools2 <- merge(school_zone_boundaries@data, schooltarget_math, by = "DBN", all.y=TRUE)
boundariesandschools2 <- boundariesandschools2[!is.na(boundariesandschools2$`Mean Scale Score`),]
elementary_ps2 <- inner_join(schools_df, boundariesandschools2)

#********************************Mapping English and Math test scores*************************************************

# Create base map for NYC
nyc_map <- create_city_basemap("New York, NY", -74.00, 40.71)
# Plot English mean scale score
nyc_school_map_eng <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `Mean Scale Score`), 
                                                size=.2, color="black", 
                                                data=elementary_ps, alpha=.8) +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_eng

# Plot Math mean scale score
nyc_school_map_math <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `Mean Scale Score`), 
                                                size=.2, color="black", 
                                                data=elementary_ps2, alpha=.8) +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
# Show map!
nyc_school_map_math

#show math and english score distribution on one page to see the contrast
elementary_ps$subject <- "English"
elementary_ps2$subject <- "Math"
elementary_ps3 <- rbind(elementary_ps, elementary_ps2)

nyc_school_map <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= rank(`Mean Scale Score`)/length(`Mean Scale Score`)), 
                                                size=.2, color="black", 
                                                data=elementary_ps3, alpha=.8) +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile")) +
  facet_wrap(~ subject, ncol=2)
#show map
nyc_school_map