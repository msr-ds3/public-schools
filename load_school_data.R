#Load libraries needed
require(RODBC)
library(RColorBrewer)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
#install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
#################

#################################################################################
################↓↓↓↓↓↓YOU MUST DO THIS STEP MANUALLY↓↓↓↓↓↓###############
################CHANGE SCHOOL DIRECTORY TO A CSV FILE############################
#################################################################################


#Load file containing directory and basic info of all high schools
schooldirectory <- read.csv("schools/schooldirectory.csv", header = TRUE)

#Load file containing school quality report
schooltarget <- read_excel("schools/schoolratings.xlsx", col_names = TRUE, skip = 1)

#Renaming column to have same name
colnames(schooldirectory)[1] <- "DBN"

# create streeteasy identifier for each elementary school
# given by ps<school_num>-<borough>
schooldirectory <- schooldirectory %>%
  separate(DBN, c("borough_num","sep_chr","ps_num"), c(2,3), remove=F) %>%
  mutate(borough_num=as.numeric(borough_num),
         ps_num=as.numeric(ps_num),
         streeteasy_id=sprintf("ps%d-%s", ps_num, tolower(gsub(' ', '', City))))

# write the DBN and streeteasy id for each borough to a different file
for (city in unique(schooldirectory$City)) {
  df <- schooldirectory %>%
    filter(City == city) %>%
    select(DBN, streeteasy_id)
  write.csv(df, sprintf('schools/elementary_schools_%s.csv', tolower(gsub(' ', '', city))), row.names=F, quote=F) 
}

#Merge left
schooldata <- merge(x = schooltarget, y = schooldirectory, by = "DBN", all.x = TRUE)

env_levels <- c("Not Meeting Target","Approaching Target","Meeting Target","Exceeding Target")

#Changing into numeric Environment Rating
schooldata <- schooldata %>%
  mutate("Environment Rating"=ifelse(`Environment Rating` == "N/A", NA, `Environment Rating`),
         "Environment Rating"=factor(`Environment Rating`, env_levels))

#Changing into numeric Achievement Rating
schooldata <- schooldata %>%
  mutate("Achievement Rating"=ifelse(`Achievement Rating` == "N/A", NA, `Achievement Rating`),
         "Achievement Rating"=factor(`Achievement Rating`, env_levels))

#Remove unnecessary intermediate data frames
rm(schooldirectory, schooltarget)

#Keeping columns that we will  be using for the directory
schooldata <- schooldata %>%
  select(DBN, streeteasy_id, `School Name`,`School Type`, District , Primary.Address, City, Zip, `Achievement Rating`, `Environment Rating`)

#Removing charter schools that are unzoned
schooldata<-schooldata[!(schooldata$District==84),]


#Add demographics excel
demographics <- read_excel("schools/demographics.xlsx", col_names = TRUE, sheet = 4)

#Filter out to the most recent data
demographics <- demographics %>%  filter(Year == "2014-15")

#Keep only relevant columns
demographics <- demographics %>%
  select(DBN, `% Asian`, `% Black`, `% Hispanic`, `% White`,
         `% English Language Learners`, `% Poverty`)

#Merge to school data
schooldata <- merge(x = schooldata, y = demographics, by = "DBN", all.x = FALSE)

#filter to elementary and k8 schools only
schooldata <- schooldata %>%
  filter(`School Type` == "Elementary" | `School Type` == "K-8" )

#Remove intermediate data frames
rm(demographics, city, df)

# save everything
save(schooldata, file="schools.RData")

#**************************Plotting maps with Google maps*************************************************************
source("find_school_district_by_address.R")

#read english and math data
schooltarget_english <- read_excel("../schools/englishscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)
schooltarget_math <- read_excel("../schools/mathscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)

#filepath to zone distribution 2013-2014
filepath <- "2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Change to a data.frame for easy plotting
schools_df <- fortify(school_zone_boundaries)
school_zone_boundaries@data$id = rownames(school_zone_boundaries@data)
boundariesandschools <- merge(school_zone_boundaries@data, schooldata, by = "DBN", all.y=TRUE)
boundariesandschools <- boundariesandschools[!is.na(boundariesandschools$`% Asian`) &
                                               !is.na(boundariesandschools$`% Black`) & !is.na(boundariesandschools$`% White`) &
                                               !is.na(boundariesandschools$`% Hispanic`),]

#join school_zone_boundaries with English test
elementary_ps <- inner_join(schools_df, boundariesandschools)
nyc_map <- create_city_basemap("New York, NY", -74.00, 40.71)
# Plot by Asian distribution in school districts
nyc_school_map_asian <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% Asian`), 
                                                    size=.2, color="black", 
                                                    data=elementary_ps, alpha=.8) + ggtitle("Asian") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_asian

# Plot by Black distribution in school districts
nyc_school_map_black <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% Black`), 
                                                    size=.2, color="black", 
                                                    data=elementary_ps, alpha=.8) + ggtitle("Black") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_black

# Plot by White distribution in school districts
nyc_school_map_white <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% White`), 
                                                     size=.2, color="black", 
                                                     data=elementary_ps, alpha=.8) + ggtitle("White") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_white

# Plot by Black distribution in school districts
nyc_school_map_hispanic <- ggmap(nyc_map) + geom_polygon(aes(x=long, y=lat, group=group, fill= `% Hispanic`), 
                                                     size=.2, color="black", 
                                                     data=elementary_ps, alpha=.8) + ggtitle("Hispanic") +
  scale_fill_continuous(low="red", high="blue", guide = guide_legend(title = "Percentile"))
#display math test distribution map
nyc_school_map_hispanic

#plot asian,black,hispanic and white maps in one page
ggplot2.multiplot(nyc_school_map_asian, nyc_school_map_black, nyc_school_map_white, nyc_school_map_hispanic, cols=2)

