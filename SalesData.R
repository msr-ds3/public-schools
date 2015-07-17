########################################
# load libraries
########################################
library(dplyr)
library(xlsx)
library(readxl)
library(ggplot2)
library(tidyr)

# set the data directory
data_dir <- '.'

# Upload the data in the sales.RData file, which is used below
load(sprintf('%s/sales.RData', data_dir))

########################################
# load and clean trip data
########################################
xlss <- Sys.glob(sprintf('%s/*.xls', data_dir))
vec <- c("BOROUGH", "NEIGHBORHOOD", "BUILDING_CATEGORY", "TAX_CLASS", 
  "BLOCK", "LOT", "EASEMENT", "BUILDING_CLASS", "ADDRESS", "APT_NUMBER", "ZIP_CODE", "RES_UNITS", "COM_UNITS", "TOTAL_UNITS", 
  "LAND_SQ_FT", "BUILD_SQ_FT", "YEAR_BUILT", "TAX_CLASS_AT_SALE", 
  "BUILD_CLASS_AT_SALE", "SALE_PRICE", "SALE_DATE")

# Load each year of sales data into one data frame
df2 <- data.frame()
for (xls in xlss) {
  tmp <- read_excel(xls, col_names = vec, skip = 5)
  df2 <- rbind(df2, tmp)
}

# Clear up extra NA data that shows up in the file
df2 <- na.omit(df2)

# Separate out the Building Code number from the text
states <- df2$BUILDING_CATEGORY
s <- substr(x = states, start = 1, stop = 2)
# Add the new building code number to the dataframe as an int
df2$BC_NUM <- as.numeric(s)
View(df2)

# Cut out all commercial buildings from the data
df3 <- df2[df2$BC_NUM<17 & df2$BC_NUM != 5 & df2$BC_NUM != 6,]

# Those with actual sales, not transfers for free
dfc <- df3[df3$SALE_PRICE > 0, ]


################################################################
### The names of these in the data file sales.Rdata are thus ###
################################################################
# The full sales data, without any removals
fullSales <- df2

# The sales only on housing buildings
homeSales <- df3

# Home Data that contains a real sale/no sales <= 0
trueHomeSales <- dfc
###########################


#################################
# Save the files
#################################

# To save the files yourself, otherwise you already have them
save(fullSales, homeSales, trueHomeSales, file = sprintf('%s/sales.RData', data_dir))

########################################
# Basic Stats on 2014-2015 data
########################################
summary(dfc)

## Average sale price by borough, only counting where sale price > 0.
avgPriceByBorough <- dfc %>% group_by(BOROUGH) %>% summarize(avgPrice = mean(SALE_PRICE))
avgPriceByBorough

## Average sale price by neighborhood, only counting where sale price > 0.
avgPriceByNeighborhood <- dfc %>% group_by(NEIGHBORHOOD) %>% summarize(avgPrice = mean(SALE_PRICE))
View(avgPriceByNeighborhood)

########################################
# PLOTTING AND MORE
########################################

# For non-zero residential sales only:
  # Plot number of sales vs sale price
    # Up to 5 million
ggplot(data = trueHomeSales, aes(x = SALE_PRICE)) + 
  geom_histogram() + xlab('Sale Price') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(labels=comma, limits = c(0, 5000000))

    # From 5 million to 100 million
ggplot(data = trueHomeSales, aes(x = SALE_PRICE)) + 
  geom_histogram() + xlab('Sale Price') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(labels=comma, limits = c(5000000, 100000000))

    # Over 100 million
ggplot(data = trueHomeSales, aes(x = SALE_PRICE)) + 
  geom_histogram() + xlab('Sale Price') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(labels=comma, limits = c(100000000, 400000000))

  # Plot number of sales vs sale price per square foot
    # Up to 2500 sq. ft
ggplot(data = trueHomeSales, aes(x = SALE_PRICE/BUILD_SQ_FT)) + 
  geom_histogram() + xlab('Sale Price Per Sq Ft') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(labels=comma, limits = c(0, 2500))

    # Land sq ft up to 2500.
ggplot(data = trueHomeSales, aes(x = SALE_PRICE/LAND_SQ_FT)) + 
  geom_histogram() + xlab('Sale Price Per Sq Ft') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(labels=comma, limits = c(0, 2500))

  # Number of sales vs square feet
    # Land sq ft, not including data w/none reported
ggplot(data = trueHomeSales, aes(x = LAND_SQ_FT)) + 
  geom_histogram() + xlab('Sale Price Per Sq Ft') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(labels=comma, limits = c(1, 15000))

    # Build sq ft
ggplot(data = trueHomeSales, aes(x = BUILD_SQ_FT)) + 
  geom_histogram() + xlab('Sale Price Per Sq Ft') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(labels=comma, limits = c(1, 15000))


###########################
# Add Data Frame Fields
###########################

##### Separate out the APT_NUM #####
df <- separate(df, ADDRESS, into = c("ADDRESS", "APTNUM"), sep = ",", remove = TRUE, extra = "drop")

# Change empty strings/NA to something. Might not be needed.
df$APTNUM[is.na(df$APTNUM)] <- " "
df$APT_NUMBER[df$APT_NUMBER == ""] <- " "

# Now merge into one column, making a unified APT_NUM column
df <- unite(df, col = "APT_NUM", c(10,11), sep = " ", remove = TRUE)

# Convert empty strings back to NA for making an isIndividual colum
df$APT_NUM[df$APT_NUM == "              "] <- NA

# Separate by individuality or not
df <- mutate(df, "isRes" = (BC_NUM <= 4 | BC_NUM == 7 | BC_NUM == 10 | BC_NUM == 14))

# Separate by residential or not
df <- mutate(df, "isIndv" = (BC_NUM <= 3 | (BC_NUM == 7 & !is.na(APT_NUM)) | (BC_NUM == 10 & !is.na(APT_NUM)) | (BC_NUM == 14 & !is.na(APT_NUM)) ))

# Save the data for later
save(df, file = sprintf('%s/factoredSales.RData', data_dir))

# Find how many sales were commercial vs residential
ggplot(data = df, aes(x = as.factor(isRes))) + 
  geom_histogram() + xlab('Is Residential?') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma)

# Find, by zip code, how many of sales each building type
# Various plots, all bad
ggplot(data = df, aes(x = as.factor(BC_NUM), fill = as.factor(ZIP_CODE))) + 
  geom_histogram() + xlab('Building Type') + 
       ylab('Number of Sales') + 
       scale_y_continuous(labels=comma) + facet_grid(ZIP_CODE ~ BOROUGH)
ggplot(data = df, aes(x = as.factor(BC_NUM), fill = as.factor(ZIP_CODE))) + 
       geom_histogram() + xlab('Building Type') + 
       ylab('Number of Sales') + 
       scale_y_continuous(labels=comma) + facet_grid(facets = . ~ BOROUGH)
ggplot(data = df, aes(x = as.factor(isRes), fill = as.factor(ZIP_CODE))) + 
       geom_histogram() + xlab('Building Type') + 
       ylab('Number of Sales') + 
       scale_y_continuous(labels=comma) + facet_grid(facets = . ~ BOROUGH)
ggplot(data = df, aes(x = as.factor(BC_NUM), fill = as.factor(BOROUGH))) + 
       geom_histogram() + xlab('Building Type') + 
       ylab('Number of Sales') + 
       scale_y_continuous(labels=comma) + facet_grid(facets = . ~ ZIP_CODE)
ggplot(data = df, aes(x = as.factor(BC_NUM), fill = as.factor(ZIP_CODE))) + 
       geom_histogram() + xlab('Building Type') + 
       ylab('Number of Sales') + 
       scale_y_continuous(labels=comma) + facet_grid(facets = . ~ BOROUGH)
ggplot(data = df, aes(x = as.factor(ZIP_CODE), fill = as.factor(BC_NUM))) + 
       geom_histogram() + xlab('Building Type') + 
       ylab('Number of Sales') + 
       scale_y_continuous(labels=comma) + facet_grid(facets = . ~ BOROUGH)
ggplot(data = df, aes(x = as.factor(ZIP_CODE), fill = as.factor(isRes))) + 
       geom_histogram() + xlab('Building Type') + 
       ylab('Number of Sales') + 
       scale_y_continuous(labels=comma) + facet_grid(facets = . ~ BOROUGH)
ggplot(data = df, aes(x = as.factor(isRes), fill = as.factor(ZIP_CODE))) + 
       geom_histogram() + xlab('Building Type') + 
       ylab('Number of Sales') + 
  scale_y_continuous(labels=comma) + facet_grid(facets = . ~ BOROUGH)

# Separate into 3 frames, including the true home sales and the home sales
dfL <- df[df$BC_NUM<17 & df$BC_NUM != 5 & df$BC_NUM != 6,]
dfP <- dfL[dfL$SALE_PRICE > 0, ]

# For residential sales:
# Get the number of sales by invididual vs multiple
ggplot(data = dfL, aes(x = as.factor(isIndv))) + 
  geom_histogram() + 
  xlab('Invididual') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma)

# Do again only counting non-zero sales
ggplot(data = dfP, aes(x = as.factor(isIndv))) + 
     geom_histogram() + xlab('Invididual?') + 
     ylab('Number of Sales') + 
     scale_y_continuous(labels=comma)


### Sales by zipcode
### Tried a few ways to do this and am not sure what else can be done ###
ggplot(data = dfP, aes(x = as.factor(ZIP_CODE), fill = as.factor(BOROUGH))) + 
  geom_histogram() + xlab('Zip Code') + 
  ylab('Number of Sales') + 
  scale_y_continuous(labels=comma)