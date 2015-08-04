##########################################################################################
##            Answering Some Questions from the NYC Sold Listing Data                   ##
##########################################################################################

## Downloading the requered libraries
library(plyr)
library(dplyr)
library(ggplot2)

##### Load Listings for Manhattan
load('../complete_data.RData')

############################################################################################
##                           Plotting The NYC Sold Listing Data                           ##
############################################################################################

##1) Use a temporary df to store only complete listings with a DBN
df <- complete_listings[!is.na(complete_listings$DBN), ]

##2) Group by borough and DBN and get an average per square feet
plot_data <- df %>%
  group_by(DBN, borough) %>% 
  summarize(avg_price = mean(price/sqft))

##3) Plot the average price per school zone faceted
ggplot(data = filter(plot_data, avg_price < 5000), aes(x = DBN, y = avg_price)) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  facet_wrap(~ borough, scale="free", ncol=1) +
  ggtitle('Average Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##4) Plot the square feet average price per school zone, colored by each borough
ggplot(data = filter(plot_data, avg_price < 5000), aes(x = DBN, y = avg_price, color = as.factor(borough))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##5) Find the average price per bedrooms, bathrooms, borough and school dristrict
plot_data <- df %>%
  group_by(DBN, bedrooms, baths, BORO_NUM, sqft) %>% summarize(avg_price = mean(price/sqft))

## Order the data
plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, avg_price))

##plot the average price less than 5000, sqft less than 5000 for beds 0, 1, 2, and 3
ggplot(data = filter(plot_data, avg_price < 5000, sqft < 5000, (bedrooms == 0 |bedrooms == 1 | bedrooms == 2 | bedrooms ==3)), 
       aes(x = avg_price, y = sqft, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Avg Price') + 
  ylab('SQFT') +
  ggtitle('Average Price Per Square Foot') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))



##6) By district, check the avg price per square foot by bedroom
ggplot(data = filter(plot_data, avg_price < 5000, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = avg_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School District By number of Bedrooms') +
  facet_wrap( ~ borough) +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

## Separate each borough into its own mapping for better looks
ggplot(data = filter(plot_data, avg_price < 5000, BORO_NUM == 3, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = avg_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone By Bedroom Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


##7) Make a new column "total rooms"
plot_data$total_rooms = plot_data$bedrooms + plot_data$baths

plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, avg_price))

## Plot based on total rooms
ggplot(data = filter(plot_data, avg_price < 5000, BORO_NUM == 3, (total_rooms == 1 | total_rooms == 2 | total_rooms == 3 | total_rooms == 4 | total_rooms == 5 | total_rooms == 6)), aes(x = DBN, y = avg_price, color = as.factor(total_rooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone By Total Room Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))
###
#Median price Sold ;)
###

##8) Group by borough and DBN and get median average price
plot_data <- df %>%
  group_by(DBN, borough) %>% 
  summarize(med_price = median(price/sqft))

##9) Plot the average price per school zone faceted
ggplot(data = plot_data, aes(x = DBN, y = med_price)) + 
  geom_point()+
  xlab('List of Schools') + 
  ylab('Median Price') +
  facet_wrap(~ borough, scale="free", ncol=1) +
  ggtitle('Median Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##10) Plot the median average price per school zone, colored each borough
ggplot(data = plot_data, aes(x = DBN, y = med_price, color = as.factor(borough))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##11) Group by bedrooms, bathrooms, borough and school
plot_data <- df %>%
  group_by(DBN, bedrooms, baths, borough) %>% 
  summarize(med_price = median(price/sqft))

## Order the data
plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, med_price))

##12) By borough, check the median average price per square feet by bedroom
ggplot(data = filter(plot_data, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = med_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone By Bedroom Numbers') +
  facet_wrap( ~ borough) +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

## Separate each borough into its own mapping for better looks
ggplot(data = filter(plot_data, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = med_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone By Bedroom Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##13) Make a new column addint the total number of rooms
plot_data$total_rooms = plot_data$bedrooms + plot_data$baths

##Order the data
plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, med_price))

## Plot based on total rooms
ggplot(data = filter(plot_data, (total_rooms == 1 | total_rooms == 2 | total_rooms == 3 | total_rooms == 4 | total_rooms == 5 | total_rooms == 6)), aes(x = DBN, y = med_price, color = as.factor(total_rooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone By Total Room Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##14) Looking for all the names for each neighborhood and thier total sum
df3 <- complete_listings %>% group_by(neighborhood) %>% summarize(num = n())
View(df3)