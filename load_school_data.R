#Load libraries needed
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
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
  filter(Location.Category.Description == "Elementary" | Location.Category.Description == "K-8" ) %>%
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
  select(DBN, streeteasy_id, `School Name`, Primary.Address, City, Zip, District, `Achievement Rating`, `Environment Rating`)

#Removing charter schools that are unzoned
schooldata<-schooldata[!(schooldata$District==84),]

#Add demographics excel
demographics <- read_excel("schools/demographics.xlsx", col_names = TRUE, sheet = 4)

#Filter out to the most recent data
demographics <- demographics %>%  filter(Year == "2014-15")

#Keep only relevant columns
demographics <- demographics %>%
  select(DBN, `Total Enrollment`, `% Asian`, `% Black`, `% Hispanic`, `% White`,
         `% English Language Learners`, `% Poverty`)

#Merge to school data
schooldata <- merge(x = schooldata, y = demographics, by = "DBN", all.x = FALSE)

#Remove intermediate data frames

rm(df, demographics)

# save everything
save(schooldata, file="schools.RData")



