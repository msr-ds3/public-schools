
#Glenda Ascencio                                              August 4, 2015

### Libraries
library(dplyr)
library(ggplot2)

### Set the working directory
setwd("~/public-schools/Glenda_R_Code")

### Load Listings for chool and the streeteasy data
load("complete_data.RData")

### Taking unnecesary colums from the sold_listing data
# => takes DBN, bathrooms, bedrooms, price and square feet
gs <- complete_listings[c(39, 16, 17, 3, 15)]

### Filtering through the rows with matching conditions
ga <- gs %>% 
  group_by(DBN, bedrooms, baths) %>% 
  summarize(avg_price = mean(price/sqft), med_price = median(price/sqft))

#############################################################################
###                 Quantiles Average Prices Per School Zone             ###
#############################################################################
summary(ga$avg_price)
quantile(ga$avg_price)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Maga. 
# 54.78  307.40  505.20  642.90  850.60 3804.00 
avq1 <- quantile(ga$avg_price)[2]
avq2 <- quantile(ga$avg_price)[3]
avq3 <- quantile(ga$avg_price)[4]

ga <- transform(ga, Avg_Price_Quartile = ifelse(
  #if
  avg_price <= avq1,
  #then
  "Q1",
  #else do another ifelse
  ifelse(
    avg_price > avq1 & avg_price <= avq2,
    "Q2",
    ifelse(
      avg_price > avq2 & avg_price <= avq3,
      "Q3",
      "Q4"
    )
  )
)
)
##View the average price 
View(ga)

### Graph the avg price per each quantile
ggplot(data = ga, 
       aes(x = DBN, y = avg_price, color = Avg_Price_Quartile)) + 
  geom_point()+
  xlab('School Zones') + 
  ylab('Average Price') +
  ggtitle('Average Price per each quantile In Each School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

#############################################################################
###                           Median Quantile Average Price               ###
#############################################################################
summary(ga$med_price)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Maga. 
# 54.78  299.00  499.40  633.40  838.70 3022.00 

ga <- transform(ga, Med_Price_Quartile = ifelse(
  #if
  med_price <= avq1,
  #then
  "Q1",
  #else do another ifelse
  ifelse(
    med_price > avq1 & med_price <= avq2,
    "Q2",
    ifelse(
      med_price > avq2 & med_price <= avq3,
      "Q3",
      "Q4"
    )
  )
)
)

### Graph the avg price per each quantile
ggplot(data = ga, 
       aes(x = DBN, y = med_price, color = Med_Price_Quartile)) + 
  geom_point()+
  xlab('School Zones') + 
  ylab('Medium  Price') +
  ggtitle('Medium Price per each quantile In Each School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


### Save the file
save(ga, file="shiny_app_df.RData")


#average price per school zone and shows them in different colors

# Premium price

#Premium Price: s the practice of keeping the price of a product or service 
#artificially high in order to encourage favorable perceptions among buyers,
#based solely on the price.