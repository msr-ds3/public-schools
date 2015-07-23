#Load libraries needed
require(RODBC)
library(RColorBrewer)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
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


#Removing charter schools that are unzoned
schooldata<-schooldata[!(schooldata$District==84),]


#Add demographics excel
demographics <- read_excel("schools/demographics.xlsx", col_names = TRUE, sheet = 4)

#Filter out to the most recent data
demographics <- demographics %>%  filter(Year == "2014-15")

#Merge to school data
schooldata <- merge(x = schooldata, y = demographics, by = "DBN", all.x = FALSE)

#filter to elementary and k8 schools only
schooldata <- schooldata %>%
  filter(`School Type` == "Elementary" | `School Type` == "K-8" )


#Delete all the NAs from the race percentages
schooldata <- schooldata[!is.na(schooldata$`% Asian`) &
                           !is.na(schooldata$`% Black`) & !is.na(schooldata$`% White`) &
                           !is.na(schooldata$`% Hispanic`) & !is.na(schooldata$`% Female`) &
                           !is.na(schooldata$`% Male`),]

#Loading English data
english <- read_excel("schools/englishscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)
#Subset by all grades
english <- english[english$Grade == "All Grades", ]
#Keep only the 2014 data
english <- english[english$Year == 2014, ]
#Drop all columns we don't need
english <- subset(english, select = c(DBN, `Mean Scale Score`))

#Loading Math data
math <- read_excel("schools/mathscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)
#Subset by all grades
math <- math[math$Grade == "All Grades", ]
#Keep only the 2014 data
math <- math[math$Year == 2014, ]
#Drop all columns we don't need
math <- subset(math, select = c(DBN, `Mean Scale Score`))

#Add data to main schooldata
schooldata <- merge(schooldata, math, by = "DBN")
schooldata <-merge(schooldata, english, by = "DBN")

#Remove intermediate data frams
rm(english, math)

#Rename the columns for easier understanding
colnames(schooldata)[16] <- "Mean Scale Score Math"
colnames(schooldata)[17] <- "Mean Scale Score English"

#Remove Columns
schooldata <- schooldata %>% select(DBN, streeteasy_id, 
          `School Name.y`,`School Type`, District , Primary.Address, City, Zip, 
          `Achievement Rating`, `Environment Rating`, `Total Enrollment`, `% Female`, 
          `% Male`,  `% Asian`, `% Black`, `% Hispanic`, `% White`, `% Poverty`, `Mean Scale Score Math`, 
          `Mean Scale Score English`)

#Remove other data frames
rm(demographics, df, schooldirectory, schooltarget, city)

# save everything
save(schooldata, file="schools.RData")