## By: Riley

#########################################################
#
# Getting the distance to the nearest station in miles
#
#########################################################

#### Necessary Functions ####
## Returns distance btwn two lat/long pairs in KMs using the Haversine formula
getDfromLatLong <- function (lat1, lat2, lon1, lon2) {
  earthRadius = 6371 #Radius of the earth in km
  dLat = lat2 - lat1  # deg2rad below
  dLon = lon2 - lon1
  a = 0.5 - cos((dLat) * pi/180)/2 + 
    cos(lat1 * pi/180) * 
    cos(lat2 * pi/180) * 
    (1 - cos((dLon) * pi/180))/2
  return (earthRadius * 2 * asin(sqrt(a)))
}

## Just a helper function
distInKMs <- function (lat1, lat2, long1, long2) {
  return (getDfromLatLong(lat1, lat2, long1, long2))
}

######################
#     Load and Save Data      #
######################
# Load the necessary Rdata file
load('../compl_school.RData')

# Read subway info from a file, store
subways <- read.csv("GoogleLineNames.csv", header = TRUE)

# Make a matrix to store the lat/long for addresses
aptLatLongs <- matrix(nrow = length(schools_zone_sales$latitude), ncol = 2)
aptLatLongs[,1] <-  schools_zone_sales$latitude
aptLatLongs[,2] <- schools_zone_sales$longitude

# Make a matrix to store the lat/long for subway stations
trainLatLongs <- matrix(nrow = length(subways$stop_lat), ncol = 2)
trainLatLongs[,1] <- subways$stop_lat
trainLatLongs[,2] <- subways$stop_lon

# Helper variables for the calculation
count = 0
distanceInMiles = 0
# For each row of lat/long pairs in the matrix
for (i in 0:nrow(aptLatLongs)) {
  # Make vectors of lats and longs the size of the subway data
  vecLat <- rep(aptLatLongs[i, 1], nrow(trainLatLongs))
  vecLong <- rep(aptLatLongs[i, 2], nrow(trainLatLongs))
  # Do vector-wise distance calculation; return vector distances from an address
    # to every train station
  dist <- distInKMs(vecLat, trainLatLongs[ ,1], vecLong, trainLatLongs[ ,2])
  # The distance stored is the minimum of the above vector converted to miles
  distanceInMiles[i] <- min(dist, na.rm = T) * 0.621371
}

# Make into a data frame
aptData <- data.frame(distanceInMiles)
# Bind it to the original data
aptData  <- cbind(schools_zone_sales, aptData )

# Save for later use
save(aptData, file="aptSchoolSubwayData .RData")