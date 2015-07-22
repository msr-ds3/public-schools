#By: Glenda Ascencio                                                     July 22, 2015

#########################################################################################
##                                     Sold Listing in NYC                             ##
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

###3) Find the maximum value from average price per school zone
max_av_listing <- arrange(aver_price_per_school_zone, desc(av_price_zone))
View(max_av_listing) #ps6-manhattan = 3902287.5


###4) What is the average price vs each square feet per school and beedroom
  ## Removing all the useless data, that finds any NA values or sqft = 0
av_price_per_sqft_bed <- sold_listing_in_manh[!is.na(sold_listing_in_manh$bedrooms), ]
av_price_per_sqft_bed <- av_price_per_sqft_bed[!is.na(av_price_per_sqft_bed$sqft), ]
av_price_per_sqft_bed <- av_price_per_sqft_bed[av_price_per_sqft_bed$sqft > 0, ]
View(av_price_per_sqft_bed)

  ## Incrementing bedrooms by 1 so that a studio is "one room", and 1 br is "1 br+1 room"
av_price_per_sqft_bed$bedrooms <- av_price_per_sqft_bed$bedrooms + 1
View(av_price_per_sqft_bed)

###5) Try if you can to plot the average price vs each square feet per school and beedroom



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