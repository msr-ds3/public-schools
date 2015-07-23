library(ggplot2)
library(dplyr)
library(locfit)

# Load the data for modeling
load("streeteasy_sales.RData")


##################################################
##             Plotting Sales Data              ##
##################################################

# Use a temporary df to store only complete listings with a DBN
df <- complete_listings[!is.na(complete_listings$DBN), ]
# Group by borough and DBN and get an average
plot_data <- df %>%
  group_by(DBN, BORO_NUM) %>% summarize(avg_price = mean(price/sqft))
View(df)

# Chart the average price per school zone faceted
ggplot(data = filter(plot_data, avg_price < 5000), aes(x = DBN, y = avg_price)) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  facet_wrap(~ BORO_NUM, scale="free", ncol=1) +
  ggtitle('Average Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

# Chart avg price per school zone, colored
ggplot(data = filter(plot_data, avg_price < 5000), aes(x = DBN, y = avg_price, color = as.factor(BORO_NUM))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))



# Group by bedrooms, bathrooms, borough and school
plot_data <- df %>%
  group_by(DBN, bedrooms, baths, BORO_NUM, sqft) %>% summarize(avg_price = mean(price/sqft))

# Order the data
plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, avg_price))



ggplot(data = filter(plot_data, avg_price < 5000, sqft < 5000, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = avg_price, y = sqft, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Avg Price') + 
  ylab('SQFT') +
  ggtitle('Average Price Per Square Foot') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))



# By borough, check the avg price per square foot by bedroom
ggplot(data = filter(plot_data, avg_price < 5000, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = avg_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone By Bedroom Numbers') +
  facet_wrap( ~ BORO_NUM) +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

# Separate each borough into its own mapping for better looks
ggplot(data = filter(plot_data, avg_price < 5000, BORO_NUM == 3, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = avg_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone By Bedroom Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


# Make a new column "total rooms"
plot_data$total_rooms = plot_data$bedrooms + plot_data$baths

plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, avg_price))

# Plot based on total rooms
ggplot(data = filter(plot_data, avg_price < 5000, BORO_NUM == 3, (total_rooms == 1 | total_rooms == 2 | total_rooms == 3 | total_rooms == 4 | total_rooms == 5 | total_rooms == 6)), aes(x = DBN, y = avg_price, color = as.factor(total_rooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Average Price') +
  ggtitle('Average Price Per School Zone By Total Room Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


# Group by borough and DBN and get an average
plot_data <- df %>%
  group_by(DBN, BORO_NUM) %>% summarize(med_price = median(price/sqft))
View(df)

# Chart the average price per school zone faceted
ggplot(data = plot_data, aes(x = DBN, y = med_price)) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  facet_wrap(~ BORO_NUM, scale="free", ncol=1) +
  ggtitle('Median Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

# Chart avg price per school zone, colored
ggplot(data = plot_data, aes(x = DBN, y = med_price, color = as.factor(BORO_NUM))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

# Group by bedrooms, bathrooms, borough and school
plot_data <- df %>%
  group_by(DBN, bedrooms, baths, BORO_NUM) %>% summarize(med_price = median(price/sqft))

# Order the data
plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, med_price))

# By borough, check the avg price per square foot by bedroom
ggplot(data = filter(plot_data, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = med_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone By Bedroom Numbers') +
  facet_wrap( ~ BORO_NUM) +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

# Separate each borough into its own mapping for better looks
ggplot(data = filter(plot_data, BORO_NUM == 3, (bedrooms == 0 | bedrooms == 1 | bedrooms == 2 | bedrooms == 3)), aes(x = DBN, y = med_price, color = as.factor(bedrooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone By Bedroom Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))


# Make a new column "total rooms"
plot_data$total_rooms = plot_data$bedrooms + plot_data$baths

plot_data <- plot_data %>%
  ungroup() %>%
  mutate(DBN=reorder(DBN, med_price))

# Plot based on total rooms
ggplot(data = filter(plot_data, BORO_NUM == 3, (total_rooms == 1 | total_rooms == 2 | total_rooms == 3 | total_rooms == 4 | total_rooms == 5 | total_rooms == 6)), aes(x = DBN, y = med_price, color = as.factor(total_rooms))) + 
  geom_point()+
  xlab('Schools') + 
  ylab('Median Price') +
  ggtitle('Median Price Per School Zone By Total Room Numbers') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##################################################
##             Modeling Sales Data              ##
##################################################

set.seed(808)

df <- complete_listings[!is.na(complete_listings$bedrooms) & !is.na(complete_listings$baths) & !is.na(complete_listings$DBN) & !is.na(complete_listings$BORO), ]
num_train <- round(nrow(df) * 0.75)
ndx <- sample(nrow(df), num_train)
train <- df[ndx, ]
test <- df[-ndx, ]

# Make a linear model
models.lm = lm(price ~ DBN + bedrooms + baths + BORO, data = train)

modelVal <- predict(models.lm, newdata = train)
modelFrame <- data.frame(modelVal)
summary(models.lm)$r.squared 
summary(models.lm)
View(modelFrame)

# Make poly models
models.lm <-lm(price ~ poly(bedrooms, 2, raw=TRUE) + 
           DBN*poly(baths, 1, raw=TRUE) + DBN, data = train)

modelVal <- predict(models.lm, newdata = train)
modelFrame <- data.frame(modelVal)
summary(models.lm)$r.squared 
summary(models.lm)
View(modelFrame)


###############
# Also Ignore #
###############

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


######################
# Ignore
#####################

#print(fit)`Mean Scale Score Math`
#fit <- rpart(`Mean Scale Score Math` ~ `% Poverty` + `Environment Rating` + District,
#             method="anova", data=train)
#printcp(fit)
#plotcp(fit)
#summary(fit)
#par(mfrow=c(1,2))
#rsq.rpart(fit)
#plot(fit, uniform=TRUE,
#     main="Regression Tree for Train Data ")
#text(fit, use.n=TRUE, all=TRUE, cex=.8)


