library(ggplot2)
library(ggmap)

# to see data distribution
tartu_map <- get_map(location = "New York", maptype = "satellite", zoom = 11)
ggmap(tartu_map, extent = "device") + geom_point(aes(x = longitude, y = latitude), colour = "red", 
                                                 alpha = 0.1, size = 2, data = schools_zone_sales)
# density, heatmaps
tartu_map_g_str <- get_map(location = "New York", zoom = 12)
ggmap(tartu_map_g_str, extent = "device") + geom_density2d(data = schools_zone_sales, 
    aes(x = longitude, y = latitude), size = 0.3) + stat_density2d(data = schools_zone_sales, 
    aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
    bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

#didn't work
#schools_zone_sales$price_cuts <- cut(schools_zone_sales$price_per_sqft, breaks=5)
#ggmap(tartu_map_g_str) + stat_density2d(data=schools_zone_sales, mapping=aes(x=longitude,
  #                      y=latitude, fill = as.factor(price_per_sqft)), alpha=0.3, geom="polygon") 
 
# not exactly, don't know why it doesn't catches price > 2500
ggmap(tartu_map_g_str) + geom_polygon(data = schools_zone_sales, aes(x = longitude, y = latitude, 
  fill = price_per_sqft), colour = NA, alpha = 0.5) + scale_fill_distiller(palette = "YlOrRd") 
 

#something crazy comes out but fill works
ggmap(tartu_map_g_str) + 
  stat_density2d(data=schools_zone_sales, mapping=aes(x=longitude, y=latitude, fill=..level..), geom="polygon", alpha=0.2) +
  scale_fill_gradient(low = "blue", high = "red") +
  geom_path(data=schools_zone_sales, aes(x=longitude, y=latitude, color=price_per_sqft), alpha=0.5) 
  #geom_text(data=schools_zone_sales, aes(x=longitude, y=latitude, label=paste0(streeteasy_id,"mi")))

  #map puts data by price
  theme_set(theme_bw(16))
  NYMap <- qmap("New York", zoom = 12, color = "bw")
  NYMap +
    geom_point(aes(x = longitude, y = latitude, colour = price_per_sqft, size = price_per_sqft),
               data = schools_zone_sales)
  #heatmap
  NYMap +
    stat_bin2d(aes(x = longitude, y = latitude),
      size = .5, bins = 30, alpha = 1/2,
      data = sold_listings) 
  
  
  pred.stat <- ggplot(data = schools_zone_sales,
                      aes(x = longitude,
                          y = latitude,
                          z = price_per_sqft)) + 
    stat_summary2d(fun = mean) 
  print(pred.stat)
  
 # no google map just shape of NY filled by price
  require('RColorBrewer')
  YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
  pred.stat.bin.width <- ggplot(data = schools_zone_sales,
                                aes(x = longitude,
                                    y = latitude,
                                    z = price_per_sqft)) + 
    stat_summary2d(fun = median) + 
    scale_fill_gradientn(name = "Median",colours = YlOrBr, space = "Lab") + coord_map()
  print(pred.stat.bin.width)
  