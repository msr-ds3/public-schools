load('../public-schools/schools.RData')
load('../public-schools/streeteasy/streeteasy_sales.RData')

df <- complete_listings[!is.na(complete_listings$bedrooms) & 
                          !is.na(complete_listings$baths) & 
                          !is.na(complete_listings$DBN) & 
                          !is.na(complete_listings$borough) & 
                          !is.na(complete_listings$baths < 4 ) & 
                          !is.na(complete_listings$sqft < 50000 ) & 
                          !is.na(complete_listings$price_per_sqft < 6000 ) & 
                          complete_listings$bedrooms < 4, ]

df <- mutate(df, DBN = droplevels(DBN))
schooldata1<-inner_join(schooldata, df, by = "DBN")
