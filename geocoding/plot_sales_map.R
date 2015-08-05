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
load("../streeteasy/plotData.RData")
load("../streeteasy/fakeDataWPremiums.RData")

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)

# Group on what we plan to plot by
eP <- schools_zone_sales %>% group_by(DBN) %>% summarize(num = n())

# Get school boundaries in each thing
boundariesandschools <- merge(school_zone_boundaries@data, eP, by = "DBN", all.y=TRUE)
stratByBeds <- merge(school_zone_boundaries@data, plot_data, by = "DBN", all.y=TRUE)
predictions <- merge(school_zone_boundaries@data, fakeDataWPremiums, by = "DBN", all.y = TRUE)

# Inner join to the school data for plotting.
salesB <- inner_join(schools_df, boundariesandschools)
predB <- inner_join(schools_df, predictions)
fakeSalesB <- inner_join(schools_df, stratByBeds)

# Create base map for NYC
nyc_map <- create_city_basemap("New York, NY")
park_slope_map <- create_city_basemap("Park Slope, NY", zoom = 14)


######################################
# Plots for fake data by the premium #
######################################

predC <- mutate(predB, binPremium = cut(premium, 
                                        labels = c('< -$150', '-$150 - $0', '$0 - $150', '> $150'), 
                                        breaks = c(min(premium), -150, 0, 150, max(premium))))

premiumMap <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill = binPremium), 
                              size=.2, color="black", 
                              data = predC, alpha=.8) + 
  ggtitle("Premium Costs for Each School Zone\n") +
  scale_fill_brewer(palette = "RdBu") + guides(fill = guide_legend(title = "Premium")) +
  xlab('') + ylab('') + 
  theme(axis.ticks = element_blank(), axis.text.x=element_blank(), axis.text.y = element_blank(),
        legend.position = c(.2, .8))
ggsave(premiumMap, file = "../figures/premiumMap.pdf", width = 5, height = 5)
ggsave(premiumMap, file = "../figures/premiumMap.png", width = 5, height = 5)


## Park Slope Map Only
parkSlopePremiumMap <- ggmap(park_slope_map) + geom_polygon(aes(x=long, y=lat, group=group, fill = binPremium), 
                                            size=.2, color="black", 
                                            data = filter(predC, neighborhood == "Park Slope"), alpha=.8) + 
  ggtitle("Premium Costs for Park Slope School Zones\n") +
  scale_fill_brewer(palette = "RdBu") + guides(fill = guide_legend(title = "Premium")) +
  xlab('') + ylab('') + 
  theme(axis.ticks = element_blank(), axis.text.x=element_blank(), axis.text.y = element_blank(),
        legend.position = c(.8, .2))
ggsave(parkSlopePremiumMap, file = "../figures/parkSlopePremiumMap.pdf", width = 5, height = 5)
ggsave(parkSlopePremiumMap, file = "../figures/parkSlopePremiumMap.png", width = 5, height = 5)


## For 2 BR 2 Bath
premiums2Bed2Baths <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill = binPremium), 
                                            size=.2, color="black", 
                                            data = filter(predC, bedrooms == 2 & baths == 2), alpha=.8) + 
  ggtitle("Premiums on 2Bed/2Bath Apts for Each School Zone\n") +
  scale_fill_brewer(palette = "RdBu") + guides(fill = guide_legend(title = "Premium")) +
  xlab('') + ylab('') + 
  theme(axis.ticks = element_blank(), axis.text.x=element_blank(), axis.text.y = element_blank(),
        legend.position = c(.2, .8))
ggsave(premiums2Bed2Baths, file = "../figures/premiums2Bed2Baths.pdf", width = 5, height = 5)
ggsave(premiums2Bed2Baths, file = "../figures/premiums2Bed2Baths.png", width = 5, height = 5)

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
