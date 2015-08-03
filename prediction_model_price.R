library(ggplot2)
library(dplyr)
library(glmnet)

load('../public-schools/compl_school.RData')

schools_zone_sales <- schools_zone_sales %>% mutate(DBN = as.factor(DBN))
schools_zone_sales$mean=rowMeans(schools_zone_sales[,c("Mean Scale Score Math", "Mean Scale Score English")], na.rm=TRUE)
# randomly select 80% of the data to train the model on
# and 20% to test on
set.seed(22)
num_train <- round(nrow(schools_zone_sales) * 0.75)
ndx <- sample(nrow(schools_zone_sales), num_train)
train <- schools_zone_sales[ndx, ]
test <- schools_zone_sales[-ndx, ]
test <- test[test$DBN %in% train$DBN, ]
test <- test[test$neighborhood %in% train$neighborhood, ]
train <- filter(train, price_per_sqft <= 6000)
rm(ndx, num_train, complete_listings, schooldata, sold_listings)

school.lm <-lm(price_per_sqft ~ bedrooms + baths + mean + DBN, data = train)
summary(school.lm)
test$predict_test <- predict(school.lm, test)
#ggplot with point and line
ggplot(test, aes(x=price_per_sqft,y=predict_test)) +  xlab("Actual ") +
  ylab("Predicted") + ggtitle("Actual vs Predicted Test Scores") + geom_abline()+
  geom_point(aes(color = `Environment Rating`))


x = model.matrix(I(price_per_sqft ) ~ baths + mean  + `% Poverty` + `% White` + `% Hispanic` 
                 + `% Asian` + (bedrooms*DBN) +num_unique_lines+num_closeby_stations + dist.1, data = train)
y = train$price_per_sqft 
cvfit = cv.glmnet (x, y)

xt = model.matrix(I(price_per_sqft ) ~ baths + mean  + `% Poverty` + `% White` + `% Hispanic` 
                  + `% Asian` + (bedrooms*DBN) +num_unique_lines+num_closeby_stations +dist.1, data = test)
yt = test$price_per_sqft 
cvpred <- predict(cvfit, newx = xt, s = "lambda.min")

## Change prediction to easy view
cvpred <- data.frame(cvpred)
## Get correlation between the predictions and the truth
cor(cvpred, yt)^2
## Plot the cvfit to see the the lambda  and mean square error
plot(cvfit)
coef(cvfit, s = "lambda.min")

sqrt(mean((cvpred$X1 - yt)^2))
median(abs(cvpred$X1 - yt))

ggplot(cvpred, aes(y=X1, x=yt)) +  xlab("Actual ") +
  ylab("Predicted") + ggtitle("Actual vs Predicted Test Scores") + geom_abline()+
  geom_point()
