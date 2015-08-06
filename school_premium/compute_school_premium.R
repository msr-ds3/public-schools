## Get the required libraries
library(ggplot2)
library(dplyr)
library(locfit)
library(glmnet)
library(broom)
library(reshape2)
## Load the data for modeling
load('compl  .RData')

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

## Make a matrix with training data that gets used as a feature selection step
x = model.matrix(I(price/sqft) ~ baths + `% Poverty` + `% White` + `% Hispanic` 
                 + `% Asian` + neighborhood + (bedrooms*DBN), data = train)
y = train$price/train$sqft

cvfit = cv.glmnet (x, y)

coef = coef(cvfit, s = "lambda.min")
coef_df <- subset(tidy(cvfit$glmnet.fit), abs(lambda-cvfit$lambda.min) < 0.01)
selected_features <- filter(coef_df, estimate>0)

# Now fit a model without schools with selected features
## (Ideally, would use linear regression here. We can constrain the lambda to be close to zero in glmnet)
x1 = model.matrix(I(price/sqft) ~ baths + neighborhood + bedrooms, data = train)
x1df <- as.data.frame(x1)
x1df <- x1df[,which(names(x1df) %in%selected_features$term)]
x1_filtered = as.matrix(x1df)
y1 = train$price/train$sqft
cvfit1 = cv.glmnet (x1_filtered, y1)

# Now fit a model with schools, with selected features
x2 = model.matrix(I(price/sqft) ~ baths + `% Poverty` + `% White` + `% Hispanic` 
                 + `% Asian` + neighborhood + (bedrooms*DBN), data = train)
x2df <- as.data.frame(x2)
x2df <- x2df[,which(names(x2df) %in%selected_features$term)]
x2_filtered = as.matrix(x2df)
y2 = train$price/train$sqft
cvfit2 = cv.glmnet (x2_filtered, y2)

# Let's check the coefficients. All feature coefficients should be non-zero.
coef_mat1 <- coef(cvfit1, s = "lambda.min")
coef_mat2 <- coef(cvfit2, s = "lambda.min")

#Now let us make predictions on train data for convenience
## (Ideally, want to do this on test data)
cvpred1 <- predict(cvfit1, newx =x1_filtered, s = "lambda.min")
cvpred2 <- predict(cvfit2, newx = x2_filtered, s = "lambda.min")

# Let's plot the premium for DBNs
plot_df <- cbind(train, data.frame(pred_without_schools=cvpred1[,1]), data.frame(pred=cvpred2[,1]))
# grouping by DBN
premium_by_DBN <- plot_df %>% group_by(DBN) %>% summarise(mean_pred_without_schools=mean(pred_without_schools), mean_pred=mean(pred)) %>%
  arrange(mean_pred_without_schools)
premium_by_DBN$DBN <- factor(premium_by_DBN$DBN, levels=premium_by_DBN$DBN)
melted_premium <- melt(premium_by_DBN, id.vars=c("DBN"))
ggplot(melted_premium, aes(x=DBN, y=value, group=variable, color=variable)) + geom_line()
