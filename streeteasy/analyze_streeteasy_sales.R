#By: Glenda Ascencio, Riley H                                             July 22, 2015
library(plyr)
library(dplyr)
library(ggplot2)

##### Load Listings for Manhattan
load("streeteasy_sales.RData")

########################################################################################
##                  Answering Some Questions On The Manhattan Sold Listing Data       ##
#########################################################################################

###1) What is the average price per school zone
aver_price_per_school_zone <- sold_listings %>% 
  group_by(school_name, borough) %>%
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

plot_data <- aver_price_per_school_zone %>%
  filter(borough != "Staten Island") %>%
  ungroup() %>%
  mutate(school_name=reorder(school_name, av_price_zone))
ggplot(data=plot_data, aes(x=school_name, y=av_price_zone)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~ borough, scale="free", ncol=1) +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1), 
        legend.background = element_rect(fill = "transparent"))

###3) Find the maximum and the top values from average price per school zone
max_av_listing <- arrange(aver_price_per_school_zone, desc(av_price_zone))
View(max_av_listing) #ps6-manhattan = 3902287.5


###4) Find the average price per square feet
aver_price_per_sz_sqft_beds <- complete_listings %>% 
  group_by(school_name, borough, bedrooms) %>%
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
listing_per_sz_and_ut <- sold_listings %>% 
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
