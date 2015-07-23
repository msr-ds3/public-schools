library(ggplot2)
library(dplyr)
library(locfit)

#get rid of NA
schooldata <- schooldata[!(is.na(schooldata$`% Poverty`) | is.na(schooldata$District) | 
                             is.na(schooldata$`Environment Rating`) | is.na(schooldata$ `Mean Scale Score Math`) |
                             is.na(schooldata$`Total Enrollment`)), ]
# randomly select 80% of the data to train the model on
# and 20% to test on
set.seed(808)
num_train <- round(nrow(schooldata) * 0.75)
ndx <- sample(nrow(schooldata), num_train)
train <- schooldata[ndx, ]
test <- schooldata[-ndx, ]

#group_by
minority_group <-schooldata %>% group_by(`% Poverty`, `Total Enrollment`,`% Asian`,
                                         `% White`, `% Black`, `% Hispanic`,
                                         `Mean Scale Score Math`, `Mean Scale Score English`) 
#model for math 
lmmin.lm = lm(`Mean Scale Score Math` ~ `% Poverty` + `Environment Rating` + District, data=train)
ggplot(train, aes(x = `% Poverty` , y  = `Mean Scale Score Math`)) + 
  geom_point(aes(colour = `% Poverty`))+ geom_smooth(method="lm",se=FALSE) +
  ggtitle("Poverty vs Math Score") 

predict(lmmin.lm, newdata=train)
summary(lmmin.lm)$r.squared 
summary(lmmin.lm)


#model with poly prediction
k<- c(1:15)
train_fit <- c(numeric(length=15))
test_fit <- c(numeric(length=15))


for (n in k){
  #fit1 <- lm(num_trips~poly(tmin,n,raw=TRUE) + poly(tmax,n,raw=TRUE)+poly(prcp,n,raw=TRUE)+poly(snwd,n,raw=TRUE)+poly(snow,n,raw=TRUE), data=merged_train)
  
  fit1<-lm(`Mean Scale Score Math` & `Mean Scale Score English`~poly(`% Poverty`,n,raw=TRUE)  +
             poly(`% Asian`,1,raw=TRUE) + poly(`% White`,1,raw=TRUE) + poly(`% Black`,1,raw=TRUE) + 
             poly(`% Hispanic`,1,raw=TRUE) + 
             poly(`Total Enrollment`,n,raw=TRUE) ,data=train)
  
  predictions_test<- predict(fit1, test)
  predictions_test<- data.frame(predictions_test)
  predictions_train <- predict(fit1,train)
  predictions_train<- data.frame(predictions_train)
  
  train_fit[n] <- cor(train$`% Poverty`,predictions_train)
  test_fit[n] <- cor(test$`% Poverty`,predictions_test)
}

df1<- data.frame(k,train_fit,train_or_test="train")
colnames(df1)[2] <- "fit"
df2<- data.frame(k,test_fit,train_or_test="test")
colnames(df2)[2] <- "fit"
df3<-rbind(df1,df2)
df3
summary(df3)
summary(lm(df3))$r.squared
ggplot(data=df3, aes(x=k,y=fit,color=as.factor(train_or_test))) + geom_line(method=locfit, formula=y ~ lp(x,nn=0.5, deg=2))
ggplot(data=df3, aes(x=k, y=fit,color=as.factor(train_or_test))) + geom_point()+ geom_smooth(method=locfit, formula=y ~ lp(x,nn=0.5, deg=2))
#regular lm model
math_test.lm = lm(`Mean Scale Score Math` ~ `% Poverty` + `Total Enrollment` + `% Asian` +
                    `% White` + `% Black` + `% Hispanic`, data=train)
#prediction for math by Poverty * Total enrollment
math_test.lm <-lm(`Mean Scale Score Math` ~ `% Poverty`*`Environment Rating`, data = train) 
summary(math_test.lm)$r.squared
test$predict_test <- predict(math_test.lm, test)
#ggplot with point and line
ggplot(test, aes(x=`Mean Scale Score Math`,y=predict_test)) +  xlab("Actual Math Test") +
  ylab("Predicted Math Test") + ggtitle("Actual vs Predicted Test Scores") + 
  geom_point(aes(color = `Achievement Rating`))
ggplot(test, aes(x=`Mean Scale Score Math`,y=predict_test)) + geom_smooth()
