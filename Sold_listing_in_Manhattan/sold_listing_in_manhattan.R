#By: Glenda Ascencio                                                     July 22, 2015

#########################################################################################
##                                     Sold Listing in NYC                             ##
#########################################################################################

##loading libraries
library(dplyr)
library(ggplot2)

##  Loading the Sold Listing in NYC
data_dir <- "."

## Renaming the first row name
vector <- c('url','unit_type','price','status','neighborhood','borough','school_name',
            'address_num', 'address_street', 'unit', 'city', 'zip_code', 'latitude', 
            'longitude', 'building_desc', 'sqft','bedrooms','baths', 'total_rooms', 
            'maintenance_fee','taxes', 'start_date', 'end_date', 'days_on_the_market', 
            'source', 'listing_agent','closing_price', 'tax_type', 'tax_expiration') 

sold_listing_in_manh <- data.frame(read.csv("*/manhattan_sold.csv"), stringsAsFactors = FALSE, sep=',')
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
############ Give this a new name to reflect what it stores ################
sold_manh_cleaned <- sold_listing_in_manh[!is.na(sold_listing_in_manh$bedrooms), ]
sold_manh_cleaned <- sold_manh_cleaned[!is.na(sold_manh_cleaned$sqft), ]
sold_manh_cleaned <- sold_manh_cleaned[sold_manh_cleaned$sqft > 0, ]
sold_manh_cleaned <- sold_manh_cleaned[0:-1, 0:29]

## Incrementing bedrooms by 1 so that a studio is "one room", and 1 br is "1 br+1 room"
sold_manh_cleaned$bedrooms <- sold_manh_cleaned$bedrooms + 1

## Add avg price per square foot as a column
sold_manh_cleaned$price_per_sqft <- sold_manh_cleaned$price/sold_manh_cleaned$sqft

## Add avg price per bedroom as a column
sold_manh_cleaned$price_per_bed <- sold_manh_cleaned$price/sold_manh_cleaned$bedrooms

View(sold_manh_cleaned)

##### SAVE THE FILES
save(sold_listing_in_manh, sold_manh_cleaned, file = sprintf('%s/SoldInManhData.RData', data_dir))

########################################################################################
##                  Answering Some Questions On The Manhattan Sold Listing Data       ##
#########################################################################################

###1) What is the average price per school zone
aver_price_per_school_zone <- sold_listing_in_manh %>% 
  group_by(school_name) %>%
  summarise(av_price_zone = mean(price))

View(aver_price_per_school_zone)

##2) Plot the manhattan sold listing average price per school zone
max = max(aver_price_per_school_zone$av_price_zone)
ggplot(data = aver_price_per_school_zone, aes(x = school_name, y = av_price_zone)) + 
  geom_point()+
  xlab('List of Boroughs') + 
  scale_y_continuous("Sold Listing per School Zone", 
                     limits = c(0,max),
                     breaks = seq(1,max,by=max / 20)) +
  ggtitle('Manhattan Sold Listing Average Price Per School Zone')+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1), 
        legend.background = element_rect(fill = "transparent"))

###3) Find the maximum and the top values from average price per school zone
max_av_listing <- arrange(aver_price_per_school_zone, desc(av_price_zone))
View(max_av_listing) #ps6-manhattan = 3902287.5


###4) Find the average price per square feet
aver_price_per_sz_sqft_beds <- sold_manh_cleaned %>% 
  group_by(school_name, bedrooms) %>%
  summarise(av_price_zone_bed = mean(price_per_sqft))
View(aver_price_per_sz_sqft_beds)

###5) Plot the average price vs each square feet per school and beedroom
# Limit to 25000 per sqft, and 0 to 4 bedrooms.
    # Note that 0 bedrooms is now 1 bedroom due to incrementing to avoid divide by 0 errors
ggplot(data = filter(aver_price_per_sz_sqft_beds, av_price_zone_bed < 25000 & bedrooms <= 5), aes(x = school_name, y = av_price_zone_bed, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('School Names') + 
  ylab('Price Per Sq Ft') + 
  ggtitle('Price Per Sqft By School Zone and Bedroom Count')+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1), 
        legend.background = element_rect(fill = "transparent"))+facet_wrap(~bedrooms)


###6) How many sold listing are there per school zone and unit type?
listing_per_sz_and_ut <- sold_listing_in_manh %>% 
  group_by(school_name, unit_type) %>% 
  summarize(per_sz_and_ut= n())

View(listing_per_sz_and_ut)

###7) Plot the sold listing per school zone and unit type
ggplot(data = listing_per_sz_and_ut, aes(x = school_name, y = per_sz_and_ut, color = unit_type)) + 
  geom_point()+
  xlab('List Of The School Names') + 
  ylab('Sold Listing Per School Zone') + 
  ggtitle('Sold Listing Per School Zone and Unit Type')+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1), 
        legend.background = element_rect(fill = "transparent"))

###9) Find the average price per bedroom

aver_price_per_bedroom <- sold_manh_cleaned %>% 
group_by(school_name, bedrooms) %>%
  summarise(price_per_bed = n())

View(aver_price_per_bedroom)

###10) Plot the average price per bedroom
ggplot(data = aver_price_per_bedroom, aes(x = school_name, y = price_per_bed, color = bedrooms)) + 
  geom_point()+
  xlab('School Names') + 
  ylab('Average Price Per Bedroom') + 
  ggtitle('Average Price Per Bedroom By School Zone')+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1), 
        legend.background = element_rect(fill = "transparent"))

