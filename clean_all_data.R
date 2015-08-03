###################################################################
##                    Get and Clean All Data                     ##
###################################################################

## Loading necessary libraries
library(dplyr)
library(readxl)
library(tidyr)
require(lubridate)

##  Loading the Sold Listing in NYC
data_dir <- "."

## Renaming the first row name
columns <- c('url','unit_type','price','status','neighborhood','borough','school_name',
             'address_num', 'address_street', 'unit', 'city', 'zip_code', 'latitude', 
             'longitude', 'building_desc', 'sqft','bedrooms','baths', 'total_rooms', 
             'maintenance_fee','taxes', 'start_date', 'end_date', 'days_on_the_market', 
             'source', 'listing_agent','closing_price', 'tax_type', 'tax_expiration') 

# Store the data in a data frame, then rename the columns appropriately
sold_listings <- data.frame()
for (borough in c("manhattan","brooklyn","bronx","queens", "statenisland")) {
  csv <- sprintf("streeteasy/%s_sold.csv", borough)
  tmp <- read.csv(csv, stringsAsFactors = FALSE, sep=',', header=F)
  sold_listings <- rbind(sold_listings, tmp)
}
colnames(sold_listings) <- columns

# remove duplicate listings:
#   remove the school name and listing agent
#   replace empty zip codes with NAs
#   then uniquify the data frame
#   this leaves just a handful of duplicates on closing price
sold_listings <- sold_listings %>%
  select(-school_name, -listing_agent) %>%
  mutate(zip_code=ifelse(zip_code == "", NA, zip_code)) %>%
  unique

####################################################################
##                    Get and Clean School Data                   ##
####################################################################

## Amit: changed it so that we do not need to manually convert.
schooldirectory = read_excel("schools/schooldirectory.xls")

# Load file containing school quality report
schooltarget <- read_excel("schools/schoolratings.xlsx", col_names = TRUE, skip = 1)

# Renaming column to have same name
colnames(schooldirectory)[1] <- "DBN"

# Create streeteasy identifier for each elementary school
# Given by ps<school_num>-<borough>
schooldirectory <- schooldirectory %>%
  separate(DBN, c("borough_num","sep_chr","ps_num"), c(2,3), remove=F) %>%
  mutate(borough_num=as.numeric(borough_num),
         ps_num=as.numeric(ps_num),
         streeteasy_id=sprintf("ps%d-%s", ps_num, tolower(gsub(' ', '', City))))

# Write the DBN and streeteasy id for each borough to a different file
for (city in unique(schooldirectory$City)) {
  df <- schooldirectory %>%
    filter(City == city) %>%
    select(DBN, streeteasy_id)
  write.csv(df, sprintf('schools/elementary_schools_%s.csv', tolower(gsub(' ', '', city))), row.names=F, quote=F) 
}

# Delete repeating column
schooldirectory$Location.Name <- NULL

# Merge with schooldirectory
schooldata <- merge(x = schooltarget, y = schooldirectory, by = "DBN", all.x = TRUE)

# Removing charter schools that are unzoned
schooldata<-schooldata[!(schooldata$District==84), ]


################################
#### Add demographics excel ####
################################
demographics <- read_excel("schools/demographics.xlsx", col_names = TRUE, sheet = 4)

# Filter out to the most recent data
demographics <- demographics %>% filter(Year == "2014-15")

# Remove extra column
demographics$`School Name` <- NULL

# Merge to school data
schooldata <- merge(x = schooldata, y = demographics, by = "DBN", all.x = FALSE)

# Filter to elementary and k8 schools only
schooldata <- schooldata %>%
  filter(`School Type` == "Elementary" | `School Type` == "K-8" )


#####################################
#### Add Math and English Scores ####
#####################################

# Loading English data
english <- read_excel("schools/englishscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)

# Subset by all grades
english <- english[english$Grade == "All Grades", ]

# Keep only the 2014 data
english <- english[english$Year == 2014, ]

# Drop all columns we don't need
english <- subset(english, select = c(DBN, `Mean Scale Score`))

# Change name
colnames(english)[2] <- "Mean Scale Score English"

# Loading Math data
math <- read_excel("schools/mathscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)

# Subset by all grades
math <- math[math$Grade == "All Grades", ]

# Keep only the 2014 data
math <- math[math$Year == 2014, ]

# Drop all columns we don't need
math <- subset(math, select = c(DBN, `Mean Scale Score`))

# Change name
colnames(math)[2] <- "Mean Scale Score Math"

# Add data to main schooldata
schooldata <- merge(schooldata, math, by = "DBN")
schooldata <- merge(schooldata, english, by = "DBN")

##################################
#  Load Environment Rating Data  #
##################################

# Remove Columns
schooldata <- schooldata %>% select(DBN, streeteasy_id, 
                                    `School Name`,`School Type`, District , `Primary Address`, City, Zip, 
                                    `Achievement Rating`, `Environment Rating`, `Total Enrollment`, `% Female`, 
                                    `% Male`,  `% Asian`, `% Black`, `% Hispanic`, `% White`, `% Poverty`, `Mean Scale Score Math`, `% English Language Learners`, 
                                    `Mean Scale Score English`)

# Recode all NAs 
schooldata <- schooldata %>%
  mutate("Environment Rating"=ifelse(`Environment Rating` == "N/A", NA, `Environment Rating`))
schooldata <- schooldata %>%
  mutate("Achievement Rating"=ifelse(`Achievement Rating` == "N/A", NA, `Achievement Rating`))

# Creating a list with all the levels
env_levels <- c("Not Meeting Target","Approaching Target","Meeting Target","Exceeding Target")

# Change from character to numeric
schooldata$`Achievement Rating`=factor(schooldata$`Achievement Rating`, env_levels)
schooldata$`Environment Rating`=factor(schooldata$`Environment Rating`, env_levels)

# Make DBN a factor instead of a char vector
schooldata$DBN <- as.factor(schooldata$DBN)

################################################
###### MERGE SALES DATA WITH SCHOOL DATA #######
################################################

# Get the functions from find_school file
source("geocoding/find_school_district_by_address.R")

#Declare the filepath to school zone distribution for 2013-2014
filepath <- "geocoding/2013_2014_School_Zones_8May2013"
shapefile <- "ES_Zones_2013-2014"

# Get school boundaries from NYC opendata shapefiles
school_zone_boundaries <- create_school_mapdata(filepath, shapefile)

# Store only those listings with an actual latitude and longitude value
latLongListings <- sold_listings[!is.na(sold_listings$latitude) & !is.na(sold_listings$longitude), ]

# Make these into coordinate objects for the purpose of projection
coordinates(latLongListings) <- ~ longitude + latitude

# Project so we can get the school zone based on the shape files and lat/long
proj4string(latLongListings) <- proj4string(school_zone_boundaries)

# Match each address to a school zone
matched_school_zones <- over(latLongListings, school_zone_boundaries)

# Finally, bind the matched addresses to the school zone information
sold_listings <- cbind(latLongListings, matched_school_zones)


###########################################
##             Data Cleaning             ##
###########################################

# Store the formula to make the start and end dates to dates.
parse_datetime <- function(s, format="%d %b %Y") {
  as.POSIXct(as.character(s), format=format) #It forces to be a data instead of a character
}

# Transform to date format
complete_listings <- transform(sold_listings, start_date = parse_datetime(start_date), end_date = parse_datetime(end_date))

# Limit to end_dates only after 2013.
complete_listings <- complete_listings[complete_listings$end_date >= "2013-01-01", ]

## Removing all the useless data, that finds any NA values or sqft = 0
complete_listings <- complete_listings[!is.na(complete_listings$bedrooms) & 
                                       !is.na(complete_listings$baths) & 
                                       !is.na(complete_listings$DBN) & 
                                       !is.na(complete_listings$borough) & 
                                       !is.na(complete_listings$sqft) &
                                       complete_listings$baths < 4  & 
                                       complete_listings$baths > 0 & 
                                       complete_listings$sqft > 100  & 
                                       complete_listings$sqft < 6000  & 
                                       complete_listings$bedrooms >= 0 & 
                                       complete_listings$bedrooms < 4, ]


## Add avg price per square foot as a column
complete_listings$price_per_sqft <- complete_listings$price/complete_listings$sqft



##############################################
##              Final Merging               ##
##############################################

# Merge with the school data on the DBN
complete_listings <- mutate(complete_listings, DBN = droplevels(DBN))
schools_zone_sales <- inner_join(schooldata, complete_listings, by = "DBN")

# There is a warning here due to the different levels between complete_listings and schooldata #
  # No need to worry, we'll fix in the next line.

# Convert DBN back to a factor
schools_zone_sales$DBN <- as.factor(schools_zone_sales$DBN)

# Remove unnecessary data frames
rm(demographics, df, schooldirectory, schooltarget, city, english, math, env_levels, tmp, matched_school_zones)

# SAVE THE FILES
  # sold_listings is the geocoded selling data, with NA lat/long values removed
  # complete_listings is the sold_listings file, getting rid of NA data or bad data in relevant
    #  fields, as well as data prior to 2013
  # schooldata is JUST school data with some relevant NA rows removed
  # schools_zone_sales is the final completely cleaned data frame
save(sold_listings, complete_listings, schooldata, schools_zone_sales, file="complete_data.RData")