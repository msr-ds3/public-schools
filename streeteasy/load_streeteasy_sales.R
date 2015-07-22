#By: Glenda Ascencio, Riley H                                             July 22, 2015

#########################################################################################
##                                Get and Clean Sold Listings in NYC                             ##
#########################################################################################

##loading libraries
library(dplyr)
library(ggplot2)

##  Loading the Sold Listing in NYC
data_dir <- "."

## Renaming the first row name
columns <- c('url','unit_type','price','status','neighborhood','borough','school_name',
            'address_num', 'address_street', 'unit', 'city', 'zip_code', 'latitude', 
            'longitude', 'building_desc', 'sqft','bedrooms','baths', 'total_rooms', 
            'maintenance_fee','taxes', 'start_date', 'end_date', 'days_on_the_market', 
            'source', 'listing_agent','closing_price', 'tax_type', 'tax_expiration') 

sold_listings <- data.frame()
for (borough in c("manhattan","brooklyn","bronx","queens")) {
  csv <- sprintf("%s_sold.csv", borough)
  tmp <- read.csv(csv, stringsAsFactors = FALSE, sep=',', header=F)
  sold_listings <- rbind(sold_listings, tmp)
}
colnames(sold_listings) <- columns


########################################################################################
##                         Basic Statistics On The Sold Listing in NYC                ##
#########################################################################################

##Take some statistics from the sold listing file in NYC
summary(sold_listings)

#######################################################################################
##                          Data Cleaning                                           ##
######################################################################################

## Removing all the useless data, that finds any NA values or sqft = 0
complete_listings <- sold_listings[!is.na(sold_listings$bedrooms), ]
complete_listings <- complete_listings[!is.na(complete_listings$sqft), ]
complete_listings <- complete_listings[complete_listings$sqft > 0, ]

## Add avg price per square foot as a column
complete_listings$price_per_sqft <- complete_listings$price/complete_listings$sqft

##### SAVE THE FILES
save(sold_listings, complete_listings, file = sprintf('%s/streeteasy_sales.RData', data_dir))

