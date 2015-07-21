library(dplyr)
library(tidyr)

# read list of schools
schooldirectory <- read.csv("schools/schooldirectory.csv", header = TRUE)

# refer to first column as "district borough number" (DBN)
colnames(schooldirectory)[1] <- "DBN"

# create streeteasy identifier for each elementary school
# given by ps<school_num>-<borough>
elementary_schools <- schooldirectory %>%
  filter(Location.Category.Description == "Elementary" | Location.Category.Description == "K-8" ) %>%
  separate(DBN, c("borough_num","sep_chr","ps_num"), c(2,3), remove=F) %>%
  mutate(borough_num=as.numeric(borough_num),
         ps_num=as.numeric(ps_num),
         streeteasy_id=sprintf("ps%d-%s", ps_num, tolower(gsub(' ', '', City))))

# write the DBN and streeteasy id for each borough to a different file
for (city in unique(elementary_schools$City)) {
  df <- elementary_schools %>%
    filter(City == city) %>%
    select(DBN, streeteasy_id)
  write.csv(df, sprintf('schools/elementary_schools_%s.csv', tolower(gsub(' ', '', city))), row.names=F, quote=F) 
}
