########################################################################################
##                      Modeling The NYC Sold Listing Sales Data                      ##
########################################################################################
## By: Riley and Glenda                                                         

## Get the required libraries
library(ggplot2)
library(dplyr)
library(locfit)
library(glmnet)

## Load the data for modeling
load('../compl_school.RData')

schools_zone_sales <-filter(schools_zone_sales, sqft > 100)
schools_zone_sales$meanScores = rowMeans(schools_zone_sales[,c("Mean Scale Score Math", "Mean Scale Score English")], na.rm=TRUE)

## Rename the original data for ease in modeling
df <- schools_zone_sales

## Declare DBNs as factors and drop any unused factors
df$DBN <- as.factor(df$DBN)
df <- mutate(df, DBN = droplevels(DBN))

## Set random seed for splitting into training and test data
set.seed(808)

## We split the the dataframe f into a test and train data
num_train <- round(nrow(df) * 0.75)
ndx <- sample(nrow(df), num_train)
train <- df[ndx, ]
test <- df[-ndx, ]

########################################################
#            Original Plotting with pure data          #
########################################################


##########################
#### ON TRAINING DATA ####
##########################
## Make a matrix with training data for use in GLMNET()
x = model.matrix(I(price/sqft) ~ baths + mean  + `% Poverty` + `% White` + `% Hispanic` 
                 + `% Asian` + neighborhood + (bedrooms*DBN), data = train)
y = train$price/train$sqft

## Make a matrix with testing data for use in GLMNET()
xT =  model.matrix(I(price/sqft) ~ baths + mean + `% Poverty` + `% White`  
                   + `% Hispanic` + `% Asian` + neighborhood + (bedrooms*DBN), data = test)
yT = test$price/test$sqft

## Run the glmnet function to get a model for the training data
  # This is running Ridge Regression and such will toss out variables
cvfit = cv.glmnet (x, y)

## We want lambda minimized, so grab the coefficients there for a general idea
coef(cvfit, s = "lambda.min")

## Predict on the training data with the lambda minimized
cvpred <- predict(cvfit, newx = x, s = "lambda.min")
## Change prediction to easy view
cvpred <- data.frame(cvpred)
## Set the predictions and actual values next to each other for the training set
pred <- cbind(cvpred, y)

## Get correlation between the predictions and the actuals for the training set
cor(cvpred, y)

## Get the sqrt(Mean Squared Error) for training data
sqrt(mean((cvpred$X1 - y)^2))

## Get the median of the absolute error for training data
median(abs(cvpred$X1 - y))


## Graph the actual vals vs predictions for training set
ggplot(data = pred, aes(x = X1, y = y)) + 
  geom_point()+
  geom_abline(data = pred,  stat = "abline", position = "identity", show_guide = FALSE)+
  xlab('Predicted Price Per Sq Ft') + 
  ylab('True Price Per Sq Ft') +
  ggtitle('Actual Price Per Sq Ft vs Predicted Price Per Sq Ft For Training Data') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


######################
#### ON TEST DATA ####
######################

## Predict on the testing data with the lambda minimized
cvpred <- predict(cvfit, newx = xT, s = "lambda.min")
## Change predictions to easy view
cvpred <- data.frame(cvpred)
## Get correlation between the predictions and the actual values for test set
cor(cvpred, yT)
## Set the predictions and actuals values next to each other for the test set
pred <- cbind(cvpred, yT)

## Get the sqrt(Mean Squared Error) for testing data
sqrt(mean((cvpred$X1 - yT)^2))

## Get the median of the absolute error for testing data
median(abs(cvpred$X1 - yT))

## Plot Actual Price vs Predicted Price for test data
ggplot(data = pred, aes(x = X1, y = yT)) + 
  geom_point()+
  geom_abline(data = pred,  stat = "abline", position = "identity", show_guide = FALSE)+
  xlab('Predicted Price Per Sq Ft') + 
  ylab('True Price Per Sq Ft') +
  ggtitle('Actual Price Per Sq Ft vs Predicted Price Per Sq Ft For Test Data') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


#################################################################
# Plotting the stratification of predictions on real test data  #
#################################################################

# Make a data frame with only relevant columns
testWPred <- cbind(test, pred)
testWPred <- testWPred[ , c("DBN", "bedrooms", "price_per_sqft", "X1")]

# Order the data for readable plotting
plot_data <- testWPred %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, X1))

# Plot the stratification of bedrooms by DBN for real data
ggplot(data = filter(plot_data, bedrooms >= 0), aes(x = DBN, y = X1, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('DBN') + 
  ylab('Prediction') + 
  ggtitle('Stratification of Bedrooms by DBN') +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


###########################################
# Modeling on Fake Data With School Stuff #
###########################################

#### To avoid that occur in expand.grid, we'll instead prep fake data differently ####
# Essentially, when using expand.grid, there's no way to tell which of the original
# data points were in which borough, as it's declared in order rather than
# by the actual borough. This skewed the data depending on chance rather than reality.
# By prepping the data this way, we make sure that the borough for a given row
# is the same as the original borough so that the fake test data reflects
# the borough properly.

## Grab necessary fields and summarize on a dummy variable, then get rid of dummy
fakeTest <- df %>% group_by(DBN, borough, meanScores, `% Poverty`, `% White`, `% Hispanic`,`% Asian`, neighborhood) %>% summarize(bathrooms = mean(baths))
fakeTest$bathrooms = NULL

## Create a new vector to vary over bedroom count
beds <- data.frame(bedrooms = c(0, 1, 2, 3))
baths <- data.frame(baths = c(1, 1.5, 2))

## Add dummy vectors for price and sqft, as glmnet forces us to make the fields
## the same in the model and the data to predict on to run properly
price <- data.frame(price = 1)
sqft <- data.frame(sqft = 1)

## Merge all vectors to create the final fake data frame
fakeTest <- merge(fakeTest, beds, all = T)
fakeTest <- merge(fakeTest, baths, all = T)
fakeTest <- merge(fakeTest, price, all = T)
fakeTest <- merge(fakeTest, sqft, all = T)
fakeTest <- merge(fakeTest, baths, all = T)

## Filter out bad configurations of beds and baths.
  # If chosen we can do this in the plot itself and calculate.
fakeTest <- filter(fakeTest, 
              (bedrooms == 0 & baths == 1) | 
              (bedrooms == 1 & baths == 1) | 
              (bedrooms == 2 & baths == 1) |  
              (bedrooms == 3 & baths == 1) |
              (bedrooms == 2 & baths == 1.5) |
              (bedrooms == 3 & baths == 1.5) |
              (bedrooms == 2 & baths == 2) |
              (bedrooms == 3 & baths == 2))


##  Prep the training data for input into glmnet
x = model.matrix(I(price/sqft) ~ baths + meanScores  + `% Poverty` + `% White` + `% Hispanic` 
                 + `% Asian` + neighborhood + (bedrooms*DBN), data = df)
y = df$price/df$sqft


## Run the actual glmnet function to get a model
cvfit = cv.glmnet (x, y)

## Make a matrix of the data to predict on
xF = model.matrix(I(price/sqft) ~ baths + meanScores  + `% Poverty` + `% White` + `% Hispanic` 
                  + `% Asian` + neighborhood + (bedrooms*DBN), data = fakeTest)

## Predict on the fake data with the lambda minimized
fakeDataPred <- predict(cvfit, newx = xF, s = "lambda.min")

## Change predictions to easy view
fakeDataPred <- data.frame(fakeDataPred)

## Bind the fake testing data to the predictions for comparison for plotting
testWPred <- cbind(fakeTest, fakeDataPred)

## Order the data for readable plotting
plot_data <- testWPred %>%
  mutate(DBN = reorder(DBN, X1))

## Plot the stratification of bedrooms by separated by borough
ggplot(data = plot_data, aes(x = DBN, y = X1, color = as.factor(bedrooms + baths))) + 
  geom_point()+xlab('DBN') + 
  ylab('Prediction') + 
  ggtitle('Stratification of bedrooms by DBN') +
  facet_grid(. ~ borough) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 80, hjust = 1),
        legend.background = element_rect(fill = "transparent"))

# Plot stratification by DBN separated by bathroom amount
ggplot(data = plot_data, aes(x = DBN, y = X1, color = as.factor(bedrooms))) + 
  geom_point()+xlab('DBN') + 
  ylab('Prediction') + 
  ggtitle('Stratification of bedrooms by DBN') +
  facet_grid(. ~ baths) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 80, hjust = 1),
        legend.background = element_rect(fill = "transparent"))
