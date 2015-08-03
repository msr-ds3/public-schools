#Load libraries needed
library(ggplot2)
library(dplyr)
#################

# load data from load_school_ratings.R
load('schools.RData')

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

#Park Slope Comparison
# Amit: %in%  is a better operator to use here. Compares for each column.
parkslope <- filter(schooldata, Zip %in% c(11217, 11215))
qplot(factor(`Environment Rating`), data=parkslope, fill=factor(`Zip`))

#Achievement by school type
#converting environment and achievement to numerics (1,2,3,4,5)
l=unique(c(as.character(schooldata$`Environment Rating`), as.character(schooldata$`Achievement Rating`)))

environments <- data.frame(Cardinal_Environment=as.numeric(factor(schooldata$`Environment Rating`, levels=l)), 
                           Cardinal_Achievement=as.numeric(factor(schooldata$`Achievement Rating`, levels=l)))

#attach environment and achievement back to schooldata
attach_toSchool_data <- cbind(schooldata, environments)

#group by geographical zip code to find the mean of environment and achievement
environment_achievement<-aggregate(attach_toSchool_data[, c("Cardinal_Environment", "Cardinal_Achievement")], 
                                    list(attach_toSchool_data$Geographical.District.Code), mean) # TODO district.code does not exist

#plotting Achievement vs Environment
ggplot(environment_achievement, aes(x=Cardinal_Environment, y=Cardinal_Achievement, 
                                    color = factor(Group.1)), size = Cardinal_Achievement) + 
  geom_point() + 
  geom_abline() +
  xlab("Environment Rating") +
  ylab("Achievement Rating") +
  ggtitle("Achievement vs Environment rating by geo zip")

