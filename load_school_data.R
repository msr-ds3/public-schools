#Load libraries needed
library(readxl)
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

#Delete repeating column
schooldirectory$Location.Name <- NULL

#Merge left
schooldata <- merge(x = schooltarget, y = schooldirectory, by = "DBN", all.x = TRUE)


#Removing charter schools that are unzoned
schooldata<-schooldata[!(schooldata$District==84),]


#Add demographics excel
demographics <- read_excel("schools/demographics.xlsx", col_names = TRUE, sheet = 4)

#Filter out to the most recent data
demographics <- demographics %>%  filter(Year == "2014-15")

#Remove deleting column
demographics$`School Name` <- NULL


#Merge to school data
schooldata <- merge(x = schooldata, y = demographics, by = "DBN", all.x = FALSE)


#filter to elementary and k8 schools only
schooldata <- schooldata %>%
  filter(`School Type` == "Elementary" | `School Type` == "K-8" )

#Loading English data
english <- read_excel("schools/englishscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)
#Subset by all grades
english <- english[english$Grade == "All Grades", ]
#Keep only the 2014 data
english <- english[english$Year == 2014, ]
#Drop all columns we don't need
english <- subset(english, select = c(DBN, `Mean Scale Score`))
#Change name
colnames(english)[2] <- "Mean Scale Score English"

#Loading Math data
math <- read_excel("schools/mathscores.xlsx", col_names = TRUE, sheet = 2, skip = 6)
#Subset by all grades
math <- math[math$Grade == "All Grades", ]
#Keep only the 2014 data
math <- math[math$Year == 2014, ]
#Drop all columns we don't need
math <- subset(math, select = c(DBN, `Mean Scale Score`))
#Change name
colnames(math)[2] <- "Mean Scale Score Math"

#Add data to main schooldata
schooldata <- merge(schooldata, math, by = "DBN")
schooldata <-merge(schooldata, english, by = "DBN")

#Remove intermediate data frams

#Remove Columns
schooldata <- schooldata %>% select(DBN, streeteasy_id, 
                                    `School Name`,`School Type`, District , Primary.Address, City, Zip, 
                                    `Achievement Rating`, `Environment Rating`, `Total Enrollment`, `% Female`, 
                                    `% Male`,  `% Asian`, `% Black`, `% Hispanic`, `% White`, `% Poverty`, `Mean Scale Score Math`, `% English Language Learners`, 
                                    `Mean Scale Score English`)

#Deleting all NAs 
schooldata <- schooldata %>%
  mutate("Environment Rating"=ifelse(`Environment Rating` == "N/A", NA, `Environment Rating`))
schooldata <- schooldata %>%
  mutate("Achievement Rating"=ifelse(`Achievement Rating` == "N/A", NA, `Achievement Rating`))

#Creating a list with all the level
env_levels <- c("Not Meeting Target","Approaching Target","Meeting Target","Exceeding Target")

#Change from character to numeric
schooldata$`Achievement Rating`=factor(schooldata$`Achievement Rating`, env_levels)
schooldata$`Environment Rating`=factor(schooldata$`Environment Rating`, env_levels)

#Remove other data frames
rm(demographics, df, schooldirectory, schooltarget, city, english, math, env_levels)

# save everything
save(schooldata, file="schools.RData")