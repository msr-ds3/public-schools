library(ggplot2)
library(dplyr)
library(locfit)
# randomly select 80% of the data to train the model on
# and 20% to test on
schooldata <- schooldata[!(is.na(schooldata$`% Poverty`) | is.na(schooldata$District) | is.na(schooldata$`Environment Rating`) | is.na(schooldata$ `Mean Scale Score Math`)), ]
set.seed(808)
num_train <- round(nrow(schooldata) * 0.75)
ndx <- sample(nrow(schooldata), num_train)
train <- schooldata[ndx, ]
test <- schooldata[-ndx, ]


minority_group <-schooldata %>% group_by(`% Poverty`, `Total Enrollment`,`% Asian`,
                                         `% White`, `% Black`, `% Hispanic`,
                                         `Mean Scale Score Math`, `Mean Scale Score English`) 

asian = minority_group$`% Asian`   # the eruption durations 
math = minority_group$`Mean Scale Score Math`  
asian_poverty = minority_group$`% Poverty`
asian_math<-cor(asian, math) 

asian_math<-aggregate( `Mean Scale Score Math`~`% Asian`, minority_group, mean )
asian_eng<-aggregate( `Mean Scale Score English`~`% Asian`, minority_group, mean )
asian_math_median<-aggregate( `Mean Scale Score Math`~`% Asian`, minority_group, median )
asian_eng_median<-aggregate( `Mean Scale Score English`~`% Asian`, minority_group, median )

ggplot(data=asian_math, aes(x = `% Asian`, y = `Mean Scale Score Math`)) + geom_point() + 
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("Mean:Asian vs Math Score")

ggplot(data=asian_eng, aes(x = `% Asian`, y = `Mean Scale Score English`)) + geom_point() + 
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("Mean:Asian vs English Score")

ggplot(data=asian_math_median, aes(x = `% Asian`, y = `Mean Scale Score Math`)) + geom_point() + 
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("Median:Asian vs Math Score")

ggplot(data=asian_eng_median, aes(x = `% Asian`, y = `Mean Scale Score English`)) + geom_point() + 
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("Median:Asian vs English Score")
#summaryBy(`Mean Scale Score Math`~`% Asian`, data=minority_group, FUN=c(mean, sd))


lmmin.lm = lm(`Mean Scale Score Math` ~ `% Poverty` + `Environment Rating` + District, data=train)
plot(train$`% Poverty`, train$`Mean Scale Score Math`, 
     main="Relationship between Math Test Score and Poverty",
     xlab="Mean Scale Score Math", ylab="Poverty") 
ggplot(train, aes(x = `% Poverty` , y  = `Mean Scale Score Math`)) + 
  geom_point(aes(colour = `% Poverty`))+ geom_smooth(method="lm",se=FALSE) +
  ggtitle("Poverty vs Math Score") 

predict(lmmin.lm, newdata=train)
summary(lmmin.lm)$r.squared 
summary(lmmin.lm)


k<- c(1:15)
train_fit <- c(numeric(length=15))
test_fit <- c(numeric(length=15))

for (n in k){
  #fit1 <- lm(num_trips~poly(tmin,n,raw=TRUE) + poly(tmax,n,raw=TRUE)+poly(prcp,n,raw=TRUE)+poly(snwd,n,raw=TRUE)+poly(snow,n,raw=TRUE), data=merged_train)
  
  fit1<-locfit(`Mean Scale Score Math`~ lp(`% Poverty`,`Environment Rating`,
                                           `% Asian`, `% Hispanic`, `% Black`, `% White`,
                                           `Total Enrollment`, n, deg=2), data=train)

  predictions_test<- predict(fit1, test)
  predictions_test<- data.frame(predictions_test)
  predictions_train <- predict(fit1,train)
  predictions_train<- data.frame(predictions_train)
  
  train_fit[n] <- cor(train$`% Poverty`,predictions_train)
  test_fit[n] <- cor(test$`% Poverty`,predictions_test)
}

for (n in k){
  #fit1 <- lm(num_trips~poly(tmin,n,raw=TRUE) + poly(tmax,n,raw=TRUE)+poly(prcp,n,raw=TRUE)+poly(snwd,n,raw=TRUE)+poly(snow,n,raw=TRUE), data=merged_train)
  
  fit1<-lm(`Mean Scale Score Math` & `Mean Scale Score English`~poly(`% Poverty`,n,raw=TRUE)  +
             poly(`% Asian`,n,raw=TRUE) + poly(`% White`,n,raw=TRUE) + poly(`% Black`,n,raw=TRUE) + 
             poly(`% Hispanic`,n,raw=TRUE) + 
             poly(`Total Enrollment`,n,raw=TRUE) + poly(as.numeric(District,n,raw=TRUE)),data=train)
  
  predictions_test<- predict(fit1, test)
  predictions_test<- data.frame(predictions_test)
  predictions_train <- predict(fit1,train)
  predictions_train<- data.frame(predictions_train)
  
  train_fit[n] <- cor(train$`% Poverty`,predictions_train)
  test_fit[n] <- cor(test$`% Poverty`,predictions_test)
}

df1<- data.frame(k,train_fit,train_or_test="train")
df1<- rename(df1, fit= train_fit)
df2<- data.frame(k,test_fit,train_or_test="test")
df2<-rename(df2,fit =test_fit)
df3<-rbind(df1,df2)
df3
summary(df3)
summary(lm(df3))$r.squared
ggplot(data=df3, aes(x=k,y=fit,color=as.factor(train_or_test))) + geom_line()
ggplot(data=df3, aes(x=k, y=fit,color=as.factor(train_or_test))) + geom_point()+ geom_smooth(method=locfit, formula=y ~ lp(x,nn=0.5, deg=2))







print(fit)`Mean Scale Score Math`
fit <- rpart(`Mean Scale Score Math` ~ `% Poverty` + `Environment Rating` + District,
             method="anova", data=train)
printcp(fit)
plotcp(fit)
summary(fit)
par(mfrow=c(1,2))
rsq.rpart(fit)
plot(fit, uniform=TRUE,
     main="Regression Tree for Train Data ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


