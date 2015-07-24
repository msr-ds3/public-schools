library(ggplot2)
library(dplyr)
library(locfit)
library(RColorBrewer)
theme_set(theme_bw())

#Deleting all the missing values
schooldata <- schooldata[!(is.na(schooldata$`% Poverty`) | is.na(schooldata$District) | is.na(schooldata$`Environment Rating`) | is.na(schooldata$ `Mean Scale Score Math`)), ]
#Setting seed to have same result
set.seed(808)
#Using only 75% of our data to train model
num_train <- round(nrow(schooldata) * 0.75)
ndx <- sample(nrow(schooldata), num_train)
#Add data to train
train <- schooldata[ndx, ]
#Place rest of data in test
test <- schooldata[-ndx, ]
#Remove residual data frames and values
rm(ndx, num_train)

# select all numeric variables
schooldata1 <- schooldata %>% select(`Total Enrollment`, `% Female`, as.numeric(`Mean Scale Score Math`), as.numeric(`% English Language Learners`),
                                     `% Male`,  `% Asian`, `% Black`, `% Hispanic`, `% White`, `% Poverty`, `Mean Scale Score Math`, `% English Language Learners`, 
                                     `Mean Scale Score English`)
#dummy values for mean in `% Female column`
mean<-sapply(schooldata1, mean)
#dummy values for median in `% Female column`
dummy_mean<-as.data.frame(t(mean))
#creating dum variable to create additional row
#dum_median <- data.frame( `Total Enrollment`= "662.8921" ,`% Female` ="0.4902032",
#                          `% English Language Learners` = "0.1381914", `% Male` = "0.5097968",
#                         `% Asian` = "0.1367071", `% Black` = "0.2754821", `% Hispanic` = "0.4060882", `% White` = "0.1616373",
#                        `% Poverty` = "0.1616373", check.names = FALSE) 

#filled2 <- rbind(dummy_median, dum_median)
dummy_mean<-dummy_mean[rep(seq_len(nrow(dummy_mean)), each=20),]
filled1 = dummy_mean
filled1$`% Female` <- seq(0.45, by = 0.01, length.out =nrow(filled1))

modelenglish.lm = lm(`Mean Scale Score English` ~ `% Poverty` + `% White`  + `% English Language Learners` + `% Female`, data=train) 
#converting filled to numeric since all variables are char
filled1 <- as.data.frame(sapply(filled2, function(col) { as.numeric(col) }))
#Test model and storing predictions in the same data gram
filled1$predictioneng <- predict(modelenglish.lm, filled1)
#See corelation
#cor(filled2$predictioneng, filled2$`Mean Scale Score Math`)^2
#Plot the prediction vs actual
qplot(data=filled1, x=`% Female`, y=predictioneng)

# median **********************************
median1<-sapply(schooldata1, median)
#dummy values for median in `% Female column`
dummy_median<-as.data.frame(t(median1))
#creating dum variable to create additional row
#dum_median <- data.frame( `Total Enrollment`= "662.8921" ,`% Female` ="0.4902032",
#                          `% English Language Learners` = "0.1381914", `% Male` = "0.5097968",
 #                         `% Asian` = "0.1367071", `% Black` = "0.2754821", `% Hispanic` = "0.4060882", `% White` = "0.1616373",
  #                        `% Poverty` = "0.1616373", check.names = FALSE) 

#filled2 <- rbind(dummy_median, dum_median)
dummy_median<-dummy_median[rep(seq_len(nrow(dummy_median)), each=20),]
filled2 = dummy_median
filled2$`% Female` <- seq(0.45, by = 0.01, length.out =nrow(filled2))

modelenglish.lm = lm(`Mean Scale Score English` ~ `% Poverty` + `% White`  + `% English Language Learners` + `% Female`, data=train) 
#converting filled to numeric since all variables are char
filled2 <- as.data.frame(sapply(filled2, function(col) { as.numeric(col) }))
#Test model and storing predictions in the same data gram
filled2$predictioneng <- predict(modelenglish.lm, filled2)
#See corelation
#cor(filled2$predictioneng, filled2$`Mean Scale Score Math`)^2
#Plot the prediction vs actual
qplot(data=filled2, x=`% Female`, y=predictioneng)

