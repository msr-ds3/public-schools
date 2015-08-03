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

## Rename the original data for ease in modeling
df <- schools_zone_sales

## Declare DBNs as factors and drop any unused factors
df$DBN <- as.factor(df$DBN)
df <- mutate(df, DBN = droplevels(DBN))

# Remove NAs in maintenance fee
df <- df[!is.na(df$maintenance_fee), ]

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


##  Prep the training data for input into glmnet
x = model.matrix(I(price/sqft) ~ DBN + bedrooms + baths + 
                  borough + maintenance_fee, data = train)
y = train$price/train$sqft

## Prep the testing data for input into glmnet
xT = model.matrix(I(price/sqft) ~ DBN + bedrooms + baths + borough + maintenance_fee, data = test)
yT = test$price/test$sqft

## Run the actual glmnet function to get a model
cvfit = cv.glmnet (x, y)

## Grab the coefficients of each variable in the equation
coef(cvfit, s = "lambda.min")

## Predict on the testing data with the lambda minimized
cvpred <- predict(cvfit, newx = x, s = "lambda.min")

## Change prediction to easy view
cvpred <- data.frame(cvpred)

## Set the predictions and actual values next to each other for the training set
pred <- cbind(cvpred, y)

## Get correlation between the predictions and the actuals for the training set
cor(cvpred, y)

# Graph the actual vals vs predictions for training set
ggplot(data = pred, aes(x = X1, y = y)) + 
  geom_point()+
  geom_abline(data = pred,  stat = "abline", position = "identity", show_guide = FALSE)+
  xlab('Predicted Price Per Sq Ft') + 
  ylab('True Price Per Sq Ft') +
  ggtitle('Actual Price Per Sq Ft vs Predicted Price Per Sq Ft For Training Data') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

## Predict on the testing data with the lambda minimized
cvpred <- predict(cvfit, newx = xT, s = "lambda.min")

## Change predictions to easy view
cvpred <- data.frame(cvpred)

## Get correlation between the predictions and the actual values for test set
cor(cvpred, yT)

## Set the predictions and actuals values next to each other for the test set
pred <- cbind(cvpred, yT)


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

## The quantile function gives us 100 equal groups into which a population can be divided 
## according to the distribution of values of a particular variable
quantile(df$price, seq(0,1,by=0.01))

## Get the avg mean squared error over all observations
mean((cvpred - yT)^2)/nrow(test)

## Get the actual average mean squared error
sqrt(mean((cvpred - yT)^2))


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
       

################################################################
# Fake data to test change in price by bedroom count, BK only  #
################################################################

## Filter any data outside of Brooklyn
fakeTrainTest <- filter(df, borough == "Brooklyn")

##  Prep the training data for input into glmnet, but only for Brooklyn for some testing
x = model.matrix(I(price/sqft) ~ DBN + bedrooms + baths, data = fakeTrainTest)
y = fakeTrainTest$price/fakeTrainTest$sqft

## Run the glmnet function to get a model
cvfit = cv.glmnet (x, y)

## Create fake data w/ expand.grid, based on real data
fakeTest <- expand.grid(DBN = unique(fakeTrainTest$DBN), bedrooms = c(0, 1, 2, 3), baths = mean(fakeTrainTest$baths), price = 1, sqft = 1)

## Make a matrix of the fake test data
xF = model.matrix(I(price/sqft) ~ DBN + bedrooms + baths, data = fakeTest)

## Predict on the fake testing data with the lambda minimized
fakeDataPred <- predict(cvfit, newx = xF, s = "lambda.min")

## Change prediction to easy view
fakeDataPred <- data.frame(fakeDataPred)

## Make a data frame with real values versus predictions
testWPred <- cbind(fakeTest, fakeDataPred)

## Order the data for easy plotting
plot_data <- testWPred %>%
  mutate(DBN=reorder(DBN, X1))

## Plot the stratification of bedrooms by DBN, just for BK
ggplot(data = filter(plot_data, bedrooms >= 0), aes(x = DBN, y = X1, color = as.factor(bedrooms))) + 
  geom_point()+xlab('DBN') + 
  ylab('Prediction') + 
  ggtitle('Stratification of Bedrooms by DBN in Brooklyn') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


###########################################################################
# Fake data to test change in price by bedrooms for all boroughs at once  #
###########################################################################

#### To avoid that occur in expand.grid, we'll instead prep fake data differently ####
  # Essentially, when using expand.grid, there's no way to tell which of the original
  # data points were in which borough, as it's declared in order rather than
  # by the actual borough. This skewed the data depending on chance rather than reality.
  # By prepping the data this way, we make sure that the borough for a given row
  # is the same as the original borough so that the fake test data reflects
  # the borough properly.

## Grab necessary fields and summarize on a dummy variable
fakeTest <- df %>% group_by(DBN, borough) %>% summarize(baths = mean(baths))

## Declare dummy variable equal to the real value we want, which is
# the avg number of bathrooms over all data
fakeTest$baths = mean(df$baths)
fakeTest$baths = NULL

## Create a new vector to vary over bedroom count
beds <- data.frame(bedrooms = c(0, 1, 2, 3))
bath <- data.frame(baths = c(1,2))
## Add dummy vectors for price and sqft, as glmnet forces us to make the fields
  ## the same in the model and the data to predict on to run properly
price <- data.frame(price = 1)
sqft <- data.frame(sqft = 1)
schoolFeatures <- unique(df[ , c(df$`% Poverty`, df$`% Asian`, df$`% Black`, df$`% White`, df$`% Hispanic`)])

## Merge all vectors to create the final fake data frame
fakeTest <- merge(fakeTest, beds, all = T)
fakeTest <- merge(fakeTest, bath, all = T)
fakeTest <- merge(fakeTest, price, all = T)
fakeTest <- merge(fakeTest, sqft, all = T)
fakeTest <- merge(fakeTest, schoolFeatures, all = T)


##  Prep the training data for input into glmnet, but only for Manhattan for some testing
x = model.matrix(I(price/sqft) ~ DBN + bedrooms + baths + borough + `% Poverty` + `% Asian` + `% Black` + `% White` + `% Hispanic`, data = df)
y = df$price/df$sqft

## Run the actual glmnet function to get a model
cvfit = cv.glmnet (x, y)

## Make a matrix of the data to predict on
xF = model.matrix(I(price/sqft) ~ DBN + bedrooms + baths + borough, data = fakeTest)

## Predict on the fake data with the lambda minimized
fakeDataPred <- predict(cvfit, newx = xF, s = "lambda.min")

## Change predictions to easy view
fakeDataPred <- data.frame(fakeDataPred)

## Bind the fake testing data to the predictions for comparison for plotting
testWPred <- cbind(fakeTest, fakeDataPred)

## Order the data for readable plotting
plot_data <- testWPred %>%
  mutate(DBN = reorder(DBN, X1))

## Plot the stratification of bedrooms by DBN
ggplot(data = filter(plot_data, bedrooms >= 0), aes(x = DBN, y = X1, color = as.factor(bedrooms))) + 
  geom_point()+xlab('DBN') + 
  ylab('Prediction') + 
  ggtitle('Stratification of bedrooms by DBN') +
  facet_grid(. ~ borough) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 80, hjust = 1),
        legend.background = element_rect(fill = "transparent"))
