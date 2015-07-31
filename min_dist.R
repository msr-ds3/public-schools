library(spatstat)
#reading subway data and renaming columns' names
subway_data <- read.csv("GoogleLineNames.csv", header = TRUE)
colnames(subway_data)[5] <- "latitude"
colnames(subway_data)[6] <- "longitude"
subway_data$X = NULL
#finding only unique station_id values
subway_data = subway_data %>% distinct(station_id)

#finding distance between two points
#for houses data
houses <-  data.frame(schools_zone_sales$latitude, schools_zone_sales$longitude)
colnames(houses)[1] <-"latitude"
colnames(houses)[2] <-"longitude"
houses_dist <- ppp(df$latitude, df$longitude, c(min(df$latitude), max(df$latitude)), c(min(df$longitude),max(df$longitude)))

#for subway data
subway_stations <- data.frame(subway_data$latitude, subway_data$longitude) # this would be subway stops
colnames(subway_stations )[1] <-"latitude"
colnames(subway_stations )[2] <-"longitude"
subway_dist <- ppp(df2$latitude, df2$longitude, c(min(df2$latitude), max(df2$latitude)), c(min(df2$longitude),max(df2$longitude)))

#distance between subway and houses
dist_btwn_house_subway<-nncross(pobj, pobj2, k = 1:5) 
hist(mp$dist.1, breaks = seq(0,0.1,0.001))
cutoff<-0.0144

# for first feature
closest_stations<-mutate(dist_btwn_house_subway, num_closeby_stations = (dist.1<=cutoff) + (dist.2<=cutoff) + (dist.3<=cutoff) +
         (dist.4<=cutoff) + (dist.5<=cutoff))  

# for second feature
closest_stations =mutate(closest_stations, line1= ifelse(dist.1<=cutoff, as.character(subway_data[which.1,]$line_name),""), 
            line2= ifelse(dist.2<=cutoff, as.character(subway_data[which.2,]$line_name), ""),
            line3= ifelse(dist.3<=cutoff, as.character(subway_data[which.3,]$line_name), ""),
            line4= ifelse(dist.4<=cutoff, as.character(subway_data[which.4,]$line_name), ""),
            line5= ifelse(dist.5<=cutoff, as.character(subway_data[which.5,]$line_name), ""))

#three features (close_by_stations, dist.1, num_unique_lines)
combine_stations_lines <- mutate(closest_stations, all_lines = paste(line1,line2,line3,line4,line5, sep = ""))
combine_stations_lines <- mutate(combine_stations_lines, dummy_row = seq(1, nrow(combine_stations_lines)))
three_subway_features <- combine_stations_lines %>% group_by(dummy_row, all_lines, num_closeby_stations,dist.1) %>% 
summarise(num_unique_lines=length(unique(strsplit(paste0(all_lines, ""), split="")[[1]])))
schools_zone_sales<-cbind(schools_zone_sales,three_subway_features)


ggplot(schools_zone_sales, aes(x= num_closeby_stations)) +geom_bar()+facet_grid(.~borough)
#k<-filter(schools_zone_sales, num_closeby_stations == 0, borough == "Brooklyn")
