# generate and output pairs of adjacent school zones

library(rgdal)
library(rgeos)

school_zones <- readOGR(dsn='2013_2014_School_Zones_8May2013/', layer='ES_Zones_2013-2014')
adjacent_zones <- gTouches(school_zones, byid = TRUE)
adjacent_pairs <- which(adjacent_zones, arr.ind = TRUE)
write.table(adjacent_pairs, file = "adjacent_zones.tsv", quote = FALSE, sep = "\t", 
	row.names = FALSE, col.names = FALSE)
