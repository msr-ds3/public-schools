#By: Glenda Ascencio, Riley H                                             July 22, 2015

#########################################################################################
##                                Get and Clean Sold Listings in NYC                             ##
#########################################################################################

##loading libraries
library(dplyr)
library(readxl)
library(tidyr)

source('../geocoding/plot_map_example.R')

#################################################################
## NOTE: This must be run from the streeteasy directory!!! ######
#################################################################

##  Loading the Sold Listing in NYC
data_dir <- "."

## Renaming the first row name
columns <- c('url','unit_type','price','status','neighborhood','borough','school_name',
            'address_num', 'address_street', 'unit', 'city', 'zip_code', 'latitude', 
            'longitude', 'building_desc', 'sqft','bedrooms','baths', 'total_rooms', 
            'maintenance_fee','taxes', 'start_date', 'end_date', 'days_on_the_market', 
            'source', 'listing_agent','closing_price', 'tax_type', 'tax_expiration') 

# Store the data in a data frame, then rename the columns appropriately
sold_listings <- data.frame()
for (borough in c("manhattan","brooklyn","bronx","queens", "statenisland")) {
  csv <- sprintf("%s_sold.csv", borough)
  tmp <- read.csv(csv, stringsAsFactors = FALSE, sep=',', header=F)
  sold_listings <- rbind(sold_listings, tmp)
}
colnames(sold_listings) <- columns

# remove duplicate listings:
#   remove the school name and listing agent
#   replace empty zip codes with NAs
#   then uniquify the data frame
#   this leaves just a handful of duplicates on closing price
sold_listings <- sold_listings %>%
  select(-school_name, -listing_agent) %>%
  mutate(zip_code=ifelse(zip_code == "", NA, zip_code)) %>%
  unique

##########################################
###### MERGE DATA WITH SCHOOL DATA #######
##########################################

# Get the functions from find_school file
source("../geocoding/find_school_district_by_address.R")

#Declare the filepath to school zone distribution for 2013-2014
filepath <- "../geocoding/2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Store only those listings with an actual latitude and longitude value
latLongListings <- sold_listings[!is.na(sold_listings$latitude) & !is.na(sold_listings$longitude), ]

# Make these into coordinate objects for the purpose of projection
coordinates(latLongListings) <- ~ longitude + latitude

# Project so we can get the school zone based on the shape files and lat/long
proj4string(latLongListings) <- proj4string(school_zone_boundaries)

# Match each address to a school zone
matched_school_zones <- over(latLongListings, school_zone_boundaries)

# Finally, bind the matched addresses to the school zone information
sold_listings <- cbind(latLongListings, matched_school_zones)


###############################################################################
##                          Data Cleaning                                    ##
###############################################################################
##### Run from the streeteasy directory again. ###########
## Removing all the useless data, that finds any NA values or sqft = 0
complete_listings <- sold_listings[!is.na(sold_listings$bedrooms), ]
complete_listings <- complete_listings[!is.na(complete_listings$sqft), ]
complete_listings <- complete_listings[complete_listings$sqft > 0, ]

## Add avg price per square foot as a column
complete_listings$price_per_sqft <- complete_listings$price/complete_listings$sqft

##### SAVE THE FILES
save(sold_listings, complete_listings, file = sprintf('%s/streeteasy_sales.RData', data_dir))
