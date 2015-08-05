#Load Libraries
library(plyr)
library(dplyr)
library(scales)
library(grid)
library(ggplot2)
library(easyGgplot2)
library(readxl)

#Load Necessary Data
load("complete_data.RData")
testscores = read_excel("schools/testscores.xls")

#Remove House Data that We don't need
rm(complete_listings, sold_listings)

#Keep only columns that we need
testscores <- testscores %>% select(BEDSCODE, Expr1, ITEM_DESC, `2013-14_Mean`, COUNTY_NAME)

#Renaming Richmond to Staten Island
testscores <- testscores %>% ungroup() %>% 
  mutate(COUNTY_NAME =  plyr::mapvalues(COUNTY_NAME, from = c("RICHMOND"), to = c("STATEN ISLAND")))

#Selecting NYC data
nycscores <- testscores %>% filter(COUNTY_NAME == "BRONX" | COUNTY_NAME == "MANHATTAN" | 
                                     COUNTY_NAME == "BROOKLYN" | COUNTY_NAME == "QUEENS" |
                                     COUNTY_NAME == "STATEN ISLAND" 
)

#Selecting Upstate New York data
statescore <- testscores %>% filter(!((COUNTY_NAME == "BRONX" |
                                         COUNTY_NAME == "MANHATTAN") | 
                                        COUNTY_NAME == "BROOKLYN" | COUNTY_NAME == "QUEENS" |
                                        COUNTY_NAME == "STATEN ISLAND"))

#Separating between English and Math
englishnyc <- nycscores %>% filter(Expr1 == " ELA")
mathnyc <- nycscores %>% filter(Expr1 == "Math")

englishstate <- statescore %>% filter(Expr1 == " ELA")
mathstate <- statescore %>% filter(Expr1 == "Math")

#Remove Intermediate Data Frames
rm(nycscores, statescore, testscores)

#Adding New Column with the average English scores 
englishnyc <- englishnyc %>% group_by(BEDSCODE) %>% mutate(avg_english = mean(`2013-14_Mean`))
mathnyc <- mathnyc %>% group_by(BEDSCODE) %>% mutate(avg_math = mean(`2013-14_Mean`))

englishnyc <- englishnyc %>%  select(BEDSCODE, COUNTY_NAME,  avg_english)
mathnyc <- mathnyc %>%  select(BEDSCODE, COUNTY_NAME,  avg_math)

#Joined both English and Math into one Data frame
nycscores <- inner_join(englishnyc, mathnyc)

#Remove Intermediate Data Frames
rm(englishnyc, mathnyc)

#Delete repeating values
nycscores <- unique(nycscores)

#Adding New Column with the average English scores 
englishstate <- englishstate %>% group_by(BEDSCODE) %>% mutate(avg_english = mean(`2013-14_Mean`))
mathstate <- mathstate %>% group_by(BEDSCODE) %>% mutate(avg_math = mean(`2013-14_Mean`))

#Adding New Column with the average Math scores 
englishstate <- englishstate %>%  select(BEDSCODE, COUNTY_NAME,  avg_english)
mathstate <- mathstate %>%  select(BEDSCODE, COUNTY_NAME,  avg_math)

#Joined both English and Math into one Data frame
statescores <- inner_join(englishstate, mathstate)

#Delete repeating values
statescores <- unique(statescores)

#Remove Intermediate Data Frames
rm(englishstate, mathstate)

#Adding a new column that adds labels either the city or upstate
nycscores$part <- 'nyc'
statescores$part <- 'upstate'

#Binding nyc and state scores into one column
scores <- rbind(nycscores, statescores)

#Getting the Percentile of both Math and English
scores$percentile_all <- rank(scores$avg_math + scores$avg_english)/length(scores$avg_english)
#nycschools$percentile_all <- rank(nycschools$avg_math + nycschools$avg_english)/length(nycschools$avg_english)
nycscores <- left_join(nycscores, scores)


#############################
#PLOTTING
#############################

#Plotting NYC scores against the scores of upstate NY

ggplot(scores, aes(avg_english, fill = part, color = part)) + geom_density(alpha = 0.4) + theme_bw()
ggplot(scores, aes(avg_math, fill = part, color = part)) + geom_density(alpha = 0.4) + theme_bw()

#Plotting School Performance (Statewide Percentile)
nycscores <- nycscores %>% mutate(COUNTY_NAME = factor(COUNTY_NAME, c("BRONX", "STATEN ISLAND", "QUEENS", "BROOKLYN", "MANHATTAN")))

plotpercentile <- ggplot(nycscores, aes(percentile_all)) + geom_histogram(binwidth = 0.1) + 
  theme_bw() + facet_wrap(~ COUNTY_NAME, nrow = 1) + 
  scale_x_continuous("\nSchool Performance (Statewide Percentile)", label = percent, limits = c(0,1)) + 
  scale_y_continuous("Number of Schools\n") + theme(panel.margin = unit(1, "lines"))
ggsave(plot = plotpercentile, file = "figures/plotpercentile.pdf", width = 10, height = 4)
ggsave(plot = plotpercentile, file = "figures/plotpercentile.png", width = 10, height = 4)

############

#Creating Data Frame to Plot Percentile vs Avg Price per Square Foot Per School
plotting <- schools_zone_sales %>% group_by(DBN) %>% summarize(avg_sqft = mean(price_per_sqft))
schooldata$percentile <- rank(schooldata$`Mean Scale Score Math`)/length(schooldata$`Mean Scale Score Math`)
schooldatareduce <- schooldata %>% select(DBN, percentile, City)
plotting <- merge(plotting, schooldatareduce, by = "DBN")
plotting <- plotting %>% mutate(City = factor(City, c("BRONX", "STATEN ISLAND","QUEENS", "BROOKLYN", "MANHATTAN")))
rm(schooldatareduce)

#Plotting Percentile vs Avg Price per Square Foot Per School
percentile <- ggplot(plotting, aes(x = percentile, y = avg_sqft)) + 
  geom_point() +  geom_smooth(method=loess, color = "red")  + facet_wrap(~ City, nrow = 1) + theme_bw() + 
  scale_x_continuous("\nSchool Performance (Statewide Percentile)", label = percent, limits = c(0,1)) + 
  scale_y_continuous("Average Price Per Square Foot\n", label = dollar) + theme(panel.margin = unit(1, "lines"))
ggsave(plot = percentile, file = "figures/plotpercentilesqft.pdf", width = 10, height = 4)
ggsave(plot = percentile, file = "figures/plotpercentilesqft.png", width = 10, height = 4)