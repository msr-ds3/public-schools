#
# uses the NYC geoclient API to geocode sales by BBL
#   https://developer.cityofnewyork.us/api/geoclient-api
#   https://api.cityofnewyork.us/geoclient/v1/doc
#
# requires app_id and app_key
#
library(plyr)
library(dplyr)
library(httr)

load('../sales.RData')

base_url <- 'https://api.cityofnewyork.us/geoclient/v1/bbl.json'
app_id <- 'YOUR_ID_HERE'
app_key <- 'YOUR_KEY_HERE'

# add borough name in readable format, needed for geoclient API
boroughs <- c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island')
fullSales <- mutate(fullSales, borough_name=boroughs[BOROUGH])

# get list of unique bbls
bbls <- unique(select(fullSales, BOROUGH, borough_name, BLOCK, LOT))

# construct url
num_bbls <- nrow(bbls)
results <- list()
for (i in 1:num_bbls) {
  tryCatch({
    query <- with(bbls[i,], sprintf('borough=%s&block=%d&lot=%d&app_id=%s&app_key=%s', borough_name, BLOCK, LOT, app_id, app_key))
    r <- GET(base_url, query=query)
    results[[i]] <- as.vector(content(r, "parsed")$bbl)
  },
  error=function(cond) {
    message(sprintf("could not retrieve request %d: %s %d %d", i, borough_name, BLOCK, LOT))
  }
  )

  if ((i-1) %% 1e3 == 0) {
    print(sprintf("saving %d of %d requests", i, num_bbls))
    save(results, bbls, file='geocoded_bbls.RData')
  }

  Sys.sleep(0.5)
}

# extract results, accounting for missing entries
parsed_results <- ldply(results, function(result) {
  tryCatch({
    data.frame(results[[i]][c("bblBoroughCodeIn","bblTaxBlockIn","bblTaxLotIn","latitudeInternalLabel","longitudeInternalLabel")])
  }, error=function(cond) {
  })
})

# write results to tsv
write.table(parsed_results, file="bbls_with_geo.tsv", sep="\t", row.names=F, quote=F)
