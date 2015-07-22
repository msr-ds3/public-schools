#By: Glenda Ascencio, Riley H                                             July 22, 2015

#########################################################################################
##                                Get and Clean Sold Listings in NYC                             ##
#########################################################################################

##loading libraries
library(dplyr)
library(ggplot2)

##  Loading the Sold Listing in NYC
setwd("~/public-schools/Sold_listing_in_Manhattan")
data_dir <- "."

## Renaming the first row name
vector <- c('url','unit_type','price','status','neighborhood','borough','school_name',
            'address_num', 'address_street', 'unit', 'city', 'zip_code', 'latitude', 
            'longitude', 'building_desc', 'sqft','bedrooms','baths', 'total_rooms', 
            'maintenance_fee','taxes', 'start_date', 'end_date', 'days_on_the_market', 
            'source', 'listing_agent','closing_price', 'tax_type', 'tax_expiration') 

sold_listing_in_manh <- data.frame(read.csv("data/manhattan_sold.csv"), stringsAsFactors = FALSE, sep=',')
colnames(sold_listing_in_manh) <- vector

##View the sold_listing
View(sold_listing_in_manh)
names(sold_listing_in_manh)

########################################################################################
##                         Basic Statistics On The Sold Listing in NYC                ##
#########################################################################################

##Take some statistics from the sold listing file in NYC
summary(sold_listing_in_manh)

#######################################################################################
##                          Data Cleaning                                           ##
######################################################################################

## Removing all the useless data, that finds any NA values or sqft = 0
sold_manh_cleaned <- sold_listing_in_manh[!is.na(sold_listing_in_manh$bedrooms), ]
sold_manh_cleaned <- sold_manh_cleaned[!is.na(sold_manh_cleaned$sqft), ]
sold_manh_cleaned <- sold_manh_cleaned[sold_manh_cleaned$sqft > 0, ]
sold_manh_cleaned <- sold_manh_cleaned[0:-1, 0:29]

## Incrementing bedrooms by 1 so that a studio is "one room", and 1 br is "1 br+1 room"
sold_manh_cleaned$bedrooms <- sold_manh_cleaned$bedrooms + 1

## Add avg price per square foot as a column
sold_manh_cleaned$price_per_sqft <- sold_manh_cleaned$price/sold_manh_cleaned$sqft

## Add avg price per room as a column (including bathrooms)
sold_manh_cleaned$price_per_room <- sold_manh_cleaned$price/(sold_manh_cleaned$bedrooms+sold_manh_cleaned$baths)

View(sold_manh_cleaned)

##### SAVE THE FILES
save(sold_listing_in_manh, sold_manh_cleaned, file = sprintf('%s/SoldInManhData.RData', data_dir))

