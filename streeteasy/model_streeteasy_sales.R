#By: Glenda and Ryley                                                              July, 24, 2015              

## Downloading the requered libraries
library(ggplot2)
library(dplyr)
library(locfit)
library(glmnet)

## Load the data for modeling
load("streeteasy_sales.RData")

################################################################################################
##                           Plotting The NYC Sold Listing Data                              ##
################################################################################################

##1) Use a temporary df to store only complete listings with a DBN
df <- complete_listings[!is.na(complete_listings$DBN), ]

##2) Group by borough and DBN and get an average per square feet
plot_data <- df %>%
  group_by(DBN, borough) %>% 
  summarize(avg_price = mean(price/sqft))

##3) Plot the average price per school zone faceted
ggplot(data = filter(plot_data, avg_price < 5000), aes(x = DBN, y = avg_price)) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  facet_wrap(~ borough, scale="free", ncol=1) +
  ggtitle('Average Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##4) Plot the square feet average price per school zone, colored by each borough
ggplot(data = filter(plot_data, avg_price < 5000), aes(x = DBN, y = avg_price, color = as.factor(borough))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##5) Find the average price per bedrooms, bathrooms, borough and school dristrict
plot_data <- df %>%
  group_by(DBN, bedrooms, baths, BORO_NUM, sqft) %>% summarize(avg_price = mean(price/sqft))

  ## Order the data
plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, avg_price))

  ##plot the average price less than 5000, sqft less than 5000 for beds 0, 1, 2, and 3
ggplot(data = filter(plot_data, avg_price < 5000, sqft < 5000, (bedrooms == 0 |bedrooms == 1 | bedrooms == 2 | bedrooms ==3)), 
       aes(x = avg_price, y = sqft, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Avg Price') + 
  ylab('SQFT') +
  ggtitle('Average Price Per Square Foot') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))



##6) By district, check the avg price per square foot by bedroom
ggplot(data = filter(plot_data, avg_price < 5000, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = avg_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School District By # of Bedrooms') +
  facet_wrap( ~ BORO_NUM) +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

  ## Separate each borough into its own mapping for better looks
ggplot(data = filter(plot_data, avg_price < 5000, BORO_NUM == 3, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = avg_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone By Bedroom Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


##7) Make a new column "total rooms"
plot_data$total_rooms = plot_data$bedrooms + plot_data$baths

plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, avg_price))

  ## Plot based on total rooms
ggplot(data = filter(plot_data, avg_price < 5000, BORO_NUM == 3, (total_rooms == 1 | total_rooms == 2 | total_rooms == 3 | total_rooms == 4 | total_rooms == 5 | total_rooms == 6)), aes(x = DBN, y = avg_price, color = as.factor(total_rooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone By Total Room Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))
###
#Median price Sold ;)
###

##8) Group by borough and DBN and get median average price
plot_data <- df %>%
  group_by(DBN, borough) %>% 
  summarize(med_price = median(price/sqft))

##9) Plot the average price per school zone faceted
ggplot(data = plot_data, aes(x = DBN, y = med_price)) + 
  geom_point()+
  xlab('List of Schools') + 
  ylab('Median Price') +
  facet_wrap(~ borough, scale="free", ncol=1) +
  ggtitle('Median Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##10) Plot the median average price per school zone, colored each borough
ggplot(data = plot_data, aes(x = DBN, y = med_price, color = as.factor(borough))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##11) Group by bedrooms, bathrooms, borough and school
plot_data <- df %>%
  group_by(DBN, bedrooms, baths, borough) %>% 
  summarize(med_price = median(price/sqft))

  ## Order the data
plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, med_price))

##12) By borough, check the median average price per square feet by bedroom
ggplot(data = filter(plot_data, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = med_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone By Bedroom Numbers') +
  facet_wrap( ~ borough) +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

  ## Separate each borough into its own mapping for better looks
ggplot(data = filter(plot_data, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = med_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone By Bedroom Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##13) Make a new column addint the total number of rooms
plot_data$total_rooms = plot_data$bedrooms + plot_data$baths

  ##Order the data
plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, med_price))

  ## Plot based on total rooms
ggplot(data = filter(plot_data, (total_rooms == 1 | total_rooms == 2 | total_rooms == 3 | total_rooms == 4 | total_rooms == 5 | total_rooms == 6)), aes(x = DBN, y = med_price, color = as.factor(total_rooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone By Total Room Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

####################
##14) Looking for all the names for each neighborhood and thier total sum

df3 <- complete_listings %>% group_by(neighborhood) %>% summarize(num = n())
View(df3)

###############################################################################################
##                            Modeling The NYC Sold Listing Sales Data                      ##
###############################################################################################

set.seed(808)

## Removing the nas for beds, baths, DBN, and borough is the complete listing dataset
df <- complete_listings[!is.na(complete_listings$bedrooms) & 
                          !is.na(complete_listings$baths) & 
                          !is.na(complete_listings$DBN) & 
                          !is.na(complete_listings$borough) & 
                         # !is.na(complete_listings$maintenance_fee) 
                         complete_listings$bedrooms < 4, ]
## Filter the data-frame so we can obtain only the the Upper West Side and Park Slope neighborhoods
df2 <- filter(df, price <= 2.5e6, neighborhood == "Upper West Side" | 
                neighborhood == "Park Slope"| 
                neighborhood == "Williamsburg" | 
                neighborhood == "Upper East Side" | 
                neighborhood == "Sunset Park"|
                neighborhood == "Greenpoint" |
                neighborhood == "East Williamsburg"|
                neighborhood == "Bushwick"|
                neighborhood == "Battery Park City"|
                neighborhood == "Bay Ridge"|
                neighborhood == "Bedford Park")

df2 <- filter(df, price <= 2.5e6, neighborhood == "Upper West Side" | neighborhood == "Park Slope")
##Experimenting to see what happen with outliers for each 
# df2 <- filter(df, price <= 2.5e6, borough == "Manhattan")
# df2 <- filter(df, price <= 2.5e6, borough == "Manhattan" & (DBN == "05M030" | DBN == "05M129" | DBN == "04M096" | DBN == "04M102"))

## We drop unused levels from a factor in a data frame 
df2 <- mutate(df2, DBN = droplevels(DBN))
## We split the the dataframe f into a test and train data
num_train <- round(nrow(df2) * 0.75)
ndx <- sample(nrow(df2), num_train)
train <- df2[ndx, ]
test <- df2[-ndx, ]

# ########################             # GLMNET() #             ######################### #

##  Prep the training data for input into glmnet
x = model.matrix(I(price/sqft) ~ DBN + bedrooms + baths + 
                  borough, data = train)
y = train$price/train$sqft

## Prep the testing data for input into glmnet
xT = model.matrix(I(price/sqft) ~ DBN +bedrooms + baths + 
                    borough, data = test)
yT = test$price/test$sqft

## Unique combinations
#length(unique(train$DBN))

## Run the actual glmnet function to get a model
cvfit = cv.glmnet (x, y)# Grab the coefficients of each variable in the equation
coef(cvfit, s="lambda.min")

## Predict on the testing data with the lambda minimized
cvpred <- predict(cvfit, newx = x, s = "lambda.min")

## Change prediction to easy view
cvpred <- data.frame(cvpred)

# Set the predictions and true values next to each other for the training set
pred <- cbind(cvpred, y)
View(pred)

## Get correlation between the predictions and the truth
cor(cvpred, y)

# Graph the true vals vs predictions for training set
ggplot(data = filter(pred, y <= 5000), aes(x = X1, y = y, color= y)) + 
  geom_point()+
  geom_abline(data = pred,  stat = "abline", position = "identity", show_guide = FALSE)+
  xlab('Predicted ppsqft') + 
  ylab('True Ppsqft') +
  ggtitle('True Ppsqft vs Predicted ppsqft For Training Data') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


## Predict on the testing data with the lambda minimized
cvpred <- predict(cvfit, newx = xT, s = "lambda.min")

## Change prediction to easy view
cvpred <- data.frame(cvpred)

## Get correlation between the predictions and the truth
cor(cvpred, yT)

# Set the predictions and true values next to each other for the test set
pred2 <- cbind(cvpred, yT)
View(pred2)

# Plot the true Price Per psqft vs Predicted ppsqft For Test Data
ggplot(data = filter(pred2, yT <= 5000), aes(x = X1, y = yT, color = yT)) + 
  geom_point()+
  geom_abline(data = pred,  stat = "abline", position = "identity", show_guide = FALSE)+
  xlab('Predicted ppsqft') + 
  ylab('True Ppsqft') +
  ggtitle('True Price Price per sqft vs Predicted ppsqft For Test Data') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

## Plot the cvfit to see the the lambda  and mean square error
plot(cvfit)

## The quantile function gives us 100 equal groups into which a population can be divided 
## according to the distribution of values of a particular variable
quantile(df$price, seq(0,1,by=0.01))

## Get the actual average mean squared error
mean((cvpred - yT)^2) 


###############################################################################################
###                                         Making a Linear Model                         ###
###############################################################################################
## Make a linear model
models.lm = lm(price ~ DBN*price + bedrooms + baths + borough, data = train)

modelVal <- predict(models.lm, newdata = train)
modelFrame <- data.frame(modelVal)
summary(models.lm)$r.squared 
summary(models.lm)
View(modelFrame)

## Make polynomial models
models.lm <-lm(price ~ poly(bedrooms, 2, raw=TRUE) + 
           DBN*poly(baths, 1, raw=TRUE) + DBN, data = train)

modelVal <- predict(models.lm, newdata = train)
modelFrame <- data.frame(modelVal)

## Obtaining a simple statistical analyses 
summary(models.lm)$r.squared 
summary(models.lm)
View(modelFrame)

###############################################################################################
############################################ Also Ignore ######################################
###############################################################################################

k <- c(1:15)
train_fit <- c(numeric(length=15))
test_fit <- c(numeric(length=15))

for (n in k) {
  fit1<-lm(price ~ poly(bedrooms, n, raw=TRUE) + poly(baths, 2, raw=TRUE) + BORO + poly(as.numeric(DBN), 1, raw=TRUE), data = train)
  
  predictions_test<- predict(fit1, test)
  predictions_test<- data.frame(predictions_test)
  predictions_train <- predict(fit1,train)
  predictions_train<- data.frame(predictions_train)
  
  train_fit[n] <- cor(train$price, predictions_train)
  test_fit[n] <- cor(test$price,predictions_test)
}

df1<- data.frame(k,train_fit,train_or_test="train")
df1<- rename(df1, fit= train_fit)
df2<- data.frame(k,test_fit,train_or_test="test")
df2<-rename(df2,fit =test_fit)

df3<-rbind(df1,df2)
View(df3)

ggplot(data=df3, aes(x=k,y=fit,color=as.factor(train_or_test))) + geom_line()
ggplot(data=df3, aes(x=k, y=fit,color=as.factor(train_or_test))) + geom_point()+ geom_smooth(method=locfit, formula=y ~ lp(x,nn=0.5, deg=2))

