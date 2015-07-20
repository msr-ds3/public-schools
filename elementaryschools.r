#Load libraries needed
library(readxl)
library(ggplot2)
library(dplyr)
library(reshape)
#################

#Load file containing directory and basic info of all high schools
schooldirectory <- read.csv("schools/schooldirectory.csv", header = TRUE)

#Load file containing school quality report
schoolratings <- read_excel("schools/schoolratings.xlsx", col_names = TRUE, skip = 1)

#Renaming column to have same name
colnames(schooldirectory)[1] <- "DBN"

#Deleting columns that we will not be using for the directory
schooldirectory <- schooldirectory %.% select(DBN, Primary.Address, City, Zip)

#Deleting columns that we will not be using for the quality report
schoolratings <- schoolratings %.% select(DBN, District, `School Name`, `Achievement Rating`, `Environment Rating`)

#Join Left 
allschools <- merge(x = schoolratings, y = schooldirectory, by = "DBN", all.x = FALSE)

#################
#MATCHING ELEMENTARY SCHOOLS

bronx <- read.csv("schools/elementary_schools_bronx.csv", header = TRUE)
bronx <- merge(x = bronx, y = allschools, by = "DBN", all.x = FALSE)

brooklyn <- read.csv("schools/elementary_k8_schools_brooklyn.csv", header = TRUE)
brooklyn <- merge(x = brooklyn, y = allschools, by = "DBN", all.x = FALSE)

manhattan <- read.csv("schools/elementary_k8_schools_manhattan.csv", header = TRUE)
manhattan <- merge(x = manhattan, y = allschools, by = "DBN", all.x = FALSE)

newyork <- read.csv("schools/elementary_k8_schools_newyork.csv", header = TRUE)
newyork <- merge(x = newyork, y = allschools, by = "DBN", all.x = FALSE)

queens <- read.csv("schools/elementary_k8_schools_queens.csv", header = TRUE)
queens <- merge(x = queens, y = allschools, by = "DBN", all.x = FALSE)

statenisland <- read.csv("schools/elementary_k8_schools_statenisland.csv", header = TRUE)
statenisland <- merge(x = statenisland, y = allschools, by = "DBN", all.x = FALSE)

####Binding all boroughs
elementaryschools <- rbind(bronx, brooklyn, manhattan, newyork, queens, statenisland)

#Changing rating levels into a numbers 
env_levels <- c("Not Meeting Target","Approaching Target","Meeting Target","Exceeding Target")

#Changing into numeric Environment Rating
elementaryschools <- elementaryschools %>%
  mutate("Environment Rating"=ifelse(`Environment Rating` == "N/A", NA, `Environment Rating`),
         "Environment Rating"=factor(`Environment Rating`, env_levels))

#Changing into numeric Achievement Rating
elementaryschools <- elementaryschools %>%
  mutate("Achievement Rating"=ifelse(`Achievement Rating` == "N/A", NA, `Achievement Rating`),
         "Achievement Rating"=factor(`Achievement Rating`, env_levels))

rm(schoolratings, schooldirectory, bronx, brooklyn, queens, manhattan, statenisland, newyork, allschools)