#Load libraries needed
#install_github("easyGgplot2", "kassambara")
#require(RODBC)

library(RColorBrewer)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
library(easyGgplot2)

#Supporting files to plot
source("find_school_district_by_address.R")
load('~/public-schools/schools.RData')


#filepath to zone distribution 2013-2014
filepath <- "2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Change to a data.frame for easy plotting
schoolboundaries <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)

#Add supporting files to the school data file
schooldata <- merge(school_zone_boundaries@data, schooldata, by = "DBN", all.y=TRUE)

#Delete repeating data
#schooldata <- subset(schooldata, select = c(BORO, `Shape_Leng`, Shape_Area, ESID_NO, Label, id,DBN, streeteasy_id, 
 #                                           `School Name`,`School Type`, District , Primary.Address, City, Zip, 
  #                                          `Achievement Rating`, `Environment Rating`, `% Female`, 
   #                                         `% Male`,  `% Asian`, `% Black`, `% Hispanic`, `% White`, `% Poverty`, `Mean Scale Score Math`, 
    #                                        `Mean Scale Score English`))

#join school data with their boundaries
coordinateschools <- inner_join(schoolboundaries, schooldata)

#Remove intermediate file
rm(schoolboundaries)

#Adding basemap
nyc_map <- create_city_basemap("New York, NY", -74.00, 40.71)

####################################################################
######################ACTUAL PLOTTING###############################
####################################################################

                                                  ###PLOTTING THE PERCENTAGE OF STUDENTS BY RACE##

# Plot by Asian distribution in school districts
nyc_school_map_asian <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% Asian`), 
                                                      size=.2, color="black", 
                                                      data=coordinateschools, alpha=.8) + ggtitle("Asian") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
#nyc_school_map_asian

# Plot by Black distribution in school districts
nyc_school_map_black <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% Black`), 
                                                      size=.2, color="black", 
                                                      data=coordinateschools, alpha=.8) + ggtitle("Black") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
#nyc_school_map_black

# Plot by White distribution in school districts
nyc_school_map_white <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% White`), 
                                                      size=.2, color="black", 
                                                      data=coordinateschools, alpha=.8) + ggtitle("White") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
#nyc_school_map_white

# Plot by Black distribution in school districts
nyc_school_map_hispanic <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% Hispanic`), 
                                                         size=.2, color="black", 
                                                         data=coordinateschools, alpha=.8) + ggtitle("Hispanic") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
#nyc_school_map_hispanic

#plot asian,black,hispanic and white maps in one page
ggplot2.multiplot(nyc_school_map_asian, nyc_school_map_black, nyc_school_map_white, nyc_school_map_hispanic, cols=2)

##################################

                                                ###PLOTTING  ENGLISH AND MATH TEST SCORES###

# Plot English mean scale score
school_english_test <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `Mean Scale Score English`), 
                                                    size=.2, color="black", 
                                                    data=coordinateschools, alpha=.8) +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile")) + ggtitle("English Scores")
#display math test distribution map
#school_english_test

# Plot Math mean scale score
school_math_test <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `Mean Scale Score Math`), 
                                                     size=.2, color="black", 
                                                     data=coordinateschools, alpha=.8) +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile")) + ggtitle("Math Scores")
# Show map!
#school_math_test

#Show English and Math in one page

ggplot2.multiplot(school_math_test, school_english_test)

#######################################

                                                      ###PLOTTING ENVIRONMENT AND ACHIEVEMENT RATING####

#Plot environment data
environment_map <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `Environment Rating`), 
                                                size=.2, color="black", 
                                                data=coordinateschools, alpha=.8)

environment_map = environment_map + scale_fill_brewer(palette = "RdBu") + ggtitle("Environment Rating")


# Show map!
#environment_map

#Plot for Achievement rating
achievement_map <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `Achievement Rating`), 
                                                size=.2, color="black", 
                                                data=coordinateschools, alpha=.8)

achievement_map = achievement_map + scale_fill_brewer(palette = "RdBu") + ggtitle("Achievement Rating")

# Show map!
#achievement_map 

#######################################################

ggplot2.multiplot(environment_map, achievement_map)
