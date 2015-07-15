########################################
# load libraries
########################################
library(dplyr)
library(xlsx)
library(readxl)

# set the data directory
data_dir <- '.'

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

#################################
# Save the files
#################################
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