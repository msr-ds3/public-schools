################################################################
##                      Mapping The Data                      ##
################################################################
# This file maps the original data and also data acquired from
  # model_sales_and_schools. To see the prediction data on the map,
  # please run that file first to get the prediction data.

# Grab source and libraries
source("find_school_district_by_address.R")
library(RColorBrewer)
library(dplyr)

# Get file paths for necessary shape files
filepath <- "2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Load necessary data frames for plotting
load("../complete_data.RData")

# This comes from the model_sales_and_schools file.
load("../streeteasy/plotDataWNeighborhoods.RData")

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)

# Group on what we plan to plot by
eP <- schools_zone_sales %>% group_by(DBN) %>% summarize(num = n())
eA <- schools_zone_sales %>% group_by(DBN) %>% summarize(meanSales = mean(price))

# Get school boundaries in each thing
boundariesandschools <- merge(school_zone_boundaries@data, eP, by = "DBN", all.y=TRUE)
boundariesandmeansales <- merge(school_zone_boundaries@data, eA, by = "DBN", all.y=TRUE)
boundariesandsales <- merge(school_zone_boundaries@data, plot_data, by = "DBN", all.y = TRUE)

# Inner join to the school data for plotting.
salesB <- inner_join(schools_df, boundariesandschools)
meanSales <- inner_join
fakeSalesB <- inner_join(schools_df, boundariesandsales)

# Create base map for NYC
nyc_map <- create_city_basemap("New York, NY")

#####################################################
# Plot polygons for school zones over the base map
#####################################################

# Map for number of sales by DBN
ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill = num), 
                                                size=.2, color="black", 
                                                data=salesB, alpha=.8) + ggtitle("Number of Sales By School District") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Number of Sales"))


##################################
# Plots for fake data by bedroom
##################################

# Studios
ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill = X1), 
                              size=.2, color="black", 
                              data = filter(fakeSalesB, bedrooms == 0), alpha=.8) + 
                              ggtitle("Predicted Price Per Sqft of Studios by district") + 
  scale_colour_brewer("clarity") +
  scale_x_continuous(limits = c(-74.05, -73.8))

# 1 BR
ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill = X1), 
                              size=.2, color="black", 
                              data = filter(fakeSalesB, bedrooms == 1), alpha=.8) + 
  ggtitle("Predicted Price Per Sqft of 1BRs by district") + 
  scale_colour_brewer("clarity")  +
  scale_x_continuous(limits = c(-74.05, -73.8))

# 2 BR
ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill = X1), 
                              size=.2, color="black", 
                              data = filter(fakeSalesB, bedrooms == 2), alpha=.8) + 
  ggtitle("Predicted Price Per Sqft of 2BRs by district") + 
  scale_colour_brewer("clarity")  +
  scale_x_continuous(limits = c(-74.05, -73.8))

# 3 BR, just Park Slope
ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill = X1), 
                              size=.2, color="black", 
                              data = filter(fakeSalesB, bedrooms == 3, neighborhood == "Park Slope"), alpha=.8) + 
  ggtitle("Predicted Price Per Sqft of 3BR by district") + 
  scale_x_continuous(limits = c(-74.05, -73.8)) + scale_colour_brewer("clarity")
