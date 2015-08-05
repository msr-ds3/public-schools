library(readxl)
require(RODBC)
library(RColorBrewer)
library(dplyr)

source("find_school_district_by_address.R")

#read english and math data
schooltarget_english <- read_excel("../schools/englishscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)
schooltarget_math <- read_excel("../schools/mathscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)
#renaming column name to merge 2 files together
colnames(schooltarget_english )[7] <-"english"
colnames( )[7] <-"math"
#joining english and math and calculating the mean of English and math
mean_test<- inner_join(schooltarget_english,schooltarget_math, by = "DBN")
mean_test$mean=rowMeans(mean_test[,c("math", "english")], na.rm=TRUE)
#creating new column to convert mean to discrete values
q<-quantile(mean_test$mean,probs = seq(0,1, by = 0.25), na.rm=TRUE)
mean_test$mean_disc = cut(mean_test$mean, q, names(q)[2:length(q)])
#filepath to zone distribution 2013-2014
filepath <- "2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)

#lschools <- data.frame(DBN = elementaryschools$DBN, Environment = elementaryschools$`Environment Rating`)
boundariesandschools <- merge(school_zone_boundaries@data, mean_test, by = "DBN", all.y=TRUE)
boundariesandschools <- boundariesandschools[!is.na(boundariesandschools$mean),]
#join school_zone_boundaries with English test
elementary_ps <- inner_join(schools_df, boundariesandschools)
#map for the mean score
NY_map <- get_map(location = "New York", zoom = 11)
#map for mean test score
nyc_school_map_test <- ggmap(NY_map) + geom_polygon(aes(x=long, y=lat, group=group, fill=mean_disc), 
                                                    size=.2, color="black", 
                                                    data=elementary_ps, alpha=.8) + 
                                                    scale_fill_brewer(palette = "RdBu", 
                                                    guide = guide_legend(title = "School Performance", reverse = TRUE, legend.position = c(.2, .8))) +
                                                    xlab("") +ylab("") + 
                                                    theme(axis.ticks = element_blank(), axis.text.x=element_blank(), axis.text.y = element_blank(),
                                                    legend.position = c(.2, .8))+
                                                    ggtitle("School Performance by Schools Zones")
#display
nyc_school_map_test

ggsave(plot = nyc_school_map_test, file = "figures/plot_test.pdf", width = 5, height = 5)
ggsave(plot = nyc_school_map_test, file = "figures/plot_test.png", width = 5, height = 5)
