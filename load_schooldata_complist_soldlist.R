load('../public-schools/schools.RData') #gives schooldata file
load('../public-schools/streeteasy/streeteasy_sales.RData') #gives complete_listing and sold_listing data files
#to get rid on NAs and filter some unnecessary data
df <- complete_listings[!is.na(complete_listings$bedrooms) & 
                          !is.na(complete_listings$baths) & 
                          !is.na(complete_listings$DBN) & 
                          !is.na(complete_listings$borough) & 
                          !is.na(complete_listings$sqft) & 
                          !is.na(complete_listings$price_per_sqft) & 
                          complete_listings$baths < 4  & 
                          complete_listings$baths > 0 & 
                          complete_listings$sqft < 50000 & 
                          complete_listings$price_per_sqft < 6000  & 
                          complete_listings$bedrooms >= 0 & 
                          complete_listings$bedrooms < 4, ]
# add DBN to tthe table and join df with schooldata
df <- mutate(df, DBN = droplevels(DBN))
schooldata1<-inner_join(schooldata, df, by = "DBN")
save(schooldata1, file="compl_school.RData")
