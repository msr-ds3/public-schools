#Load libraries needed
library(readxl)
library(ggplot2)
library(dplyr)
#################

#################################################################################
################↓↓↓↓↓↓YOU MUST DO THIS STEP MANUALLY↓↓↓↓↓↓###############
################CHANGE SCHOOL DIRECTORY TO A CSV FILE############################
#################################################################################


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

##########################
#TO DO 
##########################

#Make scatter plot of environment and achievement rating
plot(schooldata$`Environment Rating`, schooldata$`Achievement Rating`)


#Achievement by school type



#Park Slope Comparison

parkslope <- filter(schooldata, Zip == c(11217, 11215))
qplot(factor(`Environment Rating`), data=parkslope, fill=factor(`Zip`))


Cardinal_Environment <- as.factor(schooldata$`Environment Rating`)
levels(Cardinal_Environment) <- 1:length(levels(Cardinal_Environment))
Cardinal_Environment <- as.numeric(Cardinal_Environment)
Cardinal_Environment<- data_frame(Cardinal_Environment)

#x1 =unclass(factor(schooldata$`Environment Rating`))

attach_toSchool_data <- cbind(schooldata, Cardinal_Environment)

Cardinal_Achievement <- as.factor(schooldata$`Achievement Rating`)
levels(Cardinal_Achievement ) <- 1:length(levels(Cardinal_Achievement ))
Cardinal_Achievement <- as.numeric(Cardinal_Achievement)
Cardinal_Achievement <- data_frame(Cardinal_Achievement)

#x1 =unclass(factor(schooldata$`Environment Rating`))

attach_toSchool_data1 <- cbind(attach_toSchool_data, Cardinal_Achievement)
View