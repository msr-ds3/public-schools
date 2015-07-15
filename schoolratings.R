#Load libraries needed
library(readxl)
library(ggplot2)
#################
#Get Data 
#Download this file
#http://schools.nyc.gov/NR/rdonlyres/14B7086D-9EE8-42FB-9D10-2160BE72C1EA/0/2013_2014_All_Schools_SQR_Results_2015_01_20.xlsx
#and this one
#http://www.nyc.gov/html/doed/downloads/datasets/DOE_LocationMasterData_001.xls
#Change this file to a CSV file

#Load file containing directory and basic info of all high schools
schooldirectory <- read.csv("~/public-schools/schools/schooldirectory.csv", header = TRUE)

#Load file containing school quality report
schooltarget <- read_excel("~/public-schools/schools/schoolratings.xlsx", col_names = TRUE, skip = 1)

#Renaming column to have same name
colnames(schooldirectory)[1] <- "DBN"

#Merge left
schooldata <- merge(x = schooltarget, y = schooldirectory, by = "DBN", all.x = TRUE)

#Getting the number of school types
table(schooldata$`School Type`)

#Plotting school types, boring black and white
barplot(table(schooldata$`School Type`), ylim = c(0, 700), main = "Number of Schools by Type", ylab = "Number of Schools", xlab = "Types of schools")

#Beautiful bar with colors by the rainbow, must use, this looks nice and not boring
qplot(factor(`School Type`), data=schooldata, geom="bar", fill=factor(`School Type`), xlab = "School Type", ylab = "Number of Schools", main = "Number of Schools by Type") + theme(legend.position='none')

#################################
#PLOTTING
#################################

#Getting the number of schools by achievement rating
table(schooldata$`Achievement Rating`)

#Boring plot
barplot(table(schooldata$`Achievement Rating`), ylim = c(0, 700), main = "Number of Schools by Achievement Rating", ylab = "Number of Schools", xlab = "Target")

#Plot with beautiful colors ~ achievement
qplot(factor(`Achievement Rating`), data=schooldata, geom="bar", fill=factor(`Achievement Rating`), xlab = "Target", ylab = "Number of Schools", main = "Number of Schools by Achievement Rating") + theme(legend.position='none')
########################
########################

#Getting the number of schools by environment
table(schooldata$`Environment Rating`)

#Boring plot of environemnt 
barplot(table(schooldata$`Environment Rating`), ylim = c(0, 700), main = "Number of Schools by Environment Rating", ylab = "Number of Schools", xlab = "Target")

#Plot with beautiful colors environment
qplot(factor(`Environment Rating`), data=schooldata, geom="bar", fill=factor(`Environment Rating`), xlab = "Target", ylab = "Number of Schools", main = "Number of Schools by Environment Rating") + theme(legend.position='none')

#Plotting the different environment by zip code. 
qplot(factor(`Environment Rating`), data=schooldata, geom="bar") + facet_wrap(~ `Zip`)

