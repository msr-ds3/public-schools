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
plot_data <- sold_listings %>% 
  group_by(school_name, borough) %>%
  summarise(av_price_zone = mean(price))

##2) Plot the average price per school zone
max = max(plot_data$av_price_zone)
ggplot(data = plot_data, aes(x = school_name, y = av_price_zone)) + 
  geom_point()+
  xlab('Schools') + 
  scale_y_continuous("Sell Price", 
                     limits = c(0,max),
                     breaks = seq(1,max,by=max / 20)) +
  facet_wrap(~ borough, scale="free", ncol=1) +
  ggtitle('Average Price Per School Zone')+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

plot_data <- plot_data %>%
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
plot_data <- arrange(plot_data, desc(av_price_zone))
View(plot_data) #ps6-manhattan = 3902287.5


###4) Find the average price per square feet
plot_data <- complete_listings %>% 
  group_by(school_name, borough, bedrooms) %>%
  summarise(av_price_zone = mean(price_per_sqft))
View(plot_data)

###5) Plot the average price vs each square feet per school and beedroom
# Limit to 5000 per sqft, and 0, 1, 2, or 3 bedrooms
# Not including Manhattan
plot_data <- plot_data %>%
filter(borough != "Staten Island") %>%
  ungroup() %>%
  mutate(school_name = reorder(school_name, av_price_zone))
ggplot(data = filter(plot_data, borough != "Manhattan" & av_price_zone < 5000 & (bedrooms == 3 | bedrooms == 0 | bedrooms == 1 | bedrooms == 2)), aes(x = school_name, y = av_price_zone, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('School Names') + 
  ylab('Price Per Sq Ft') + 
  ggtitle('Price Per Sqft By School Zone and Bedroom Count')+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1), 
        legend.background = element_rect(fill = "transparent"))+facet_wrap(~ borough)

#Include Manhattan
ggplot(data = filter(plot_data, av_price_zone < 5000 & (bedrooms == 3 | bedrooms == 0 | bedrooms == 1 | bedrooms == 2)), aes(x = school_name, y = av_price_zone, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('School Names') + 
  ylab('Price Per Sq Ft') + 
  ggtitle('Price Per Sqft By School Zone and Bedroom Count')+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1), 
        legend.background = element_rect(fill = "transparent"))+facet_wrap(~ borough)

