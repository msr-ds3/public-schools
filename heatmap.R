library(ggplot2)
library(ggmap)
library(dplyr)
library(scales)

#getting of NAs from price and sqft in sold_listings
sold_listings <- sold_listings[!is.na(sold_listings$price) &
                               !is.na(sold_listings$sqft) & 
                               sold_listings$price > 0  & 
                               sold_listings$sqft > 0 & 
                               !is.na(sold_listings$DBN),]


## Add avg price per square foot as a column
sold_listings$price_per_sqft <- sold_listings$price/sold_listings$sqft

#map sold_listing by price
sold_listings <- mutate(sold_listings, `Price per sqft` = pmin(price_per_sqft, 2000))
ny_map <- get_map(location = "New York", zoom = 11)
price_plot <-  ggmap(ny_map) +
    geom_point(data = sold_listings, aes(x = longitude, y = latitude, color = `Price per sqft`),size =2, alpha = 0.5) + 
    scale_color_gradient(limits = c(0,2000), low="blue", high="red", labels = dollar) +
    theme(axis.ticks = element_blank(), axis.text.x=element_blank(), axis.text.y = element_blank(),
    legend.position = c(.2, .8))+
    xlab("") +ylab("") + 
    ggtitle("Apartments Distribution by Price")
#to display
price_plot
  ggsave(plot = price_plot, file = "figures/plot_price.pdf", width = 5, height = 5)
  ggsave(plot = price_plot, file = "figures/plot_price.png", width = 5, height = 5)
  
  #****************************************************************************************************************#
  
  
  
  ############################################################################################################
    # heatmaps, density maps not used for presentations
  ############################################################################################################
  
  # to see data distribution
  ggmap(tartu_map, extent = "device") + geom_point(aes(x = longitude, y = latitude), colour = "red", 
                                                   alpha = 0.1, size = 2, data = schools_zone_sales)
  # density, heatmaps
  tartu_map_g_str <- get_map(location = "New York", zoom = 11)
  ggmap(tartu_map_g_str, extent = "device") + geom_density2d(data = schools_zone_sales, 
  aes(x = longitude, y = latitude), size = 0.3) + stat_density2d(data = sold_listings, 
                                                                                                                            aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                            bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
    scale_alpha(range = c(0, 0.3), guide = FALSE)
 # no google map just shape of NY filled by price
  require('RColorBrewer')
  YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
  pred.stat.bin.width <- ggplot(data = schools_zone_sales,
                                aes(x = longitude,
                                    y = latitude,
                                    z = price_per_sqft)) + 
                                    stat_summary2d(fun = median) + 
                                    scale_fill_gradientn(name = "Median",colours = YlOrBr, space = "Lab") + 
                                    coord_map()
  print(pred.stat.bin.width)
  
####################################################################################################################
  
  #schoool quality mean test score
  
####################################################################################################################
  elementary_ps2 <- elementary_ps2%>% mutate(DBN = as.factor(DBN))
  elementary_ps2$mean=rowMeans(elementary_ps2[,c("Mean Scale Score Math", "Mean Scale Score")], na.rm=TRUE)
  

 
 
