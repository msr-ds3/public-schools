#Load Libraries
library(devtools)
library(easyGgplot2)


#Scatterplot Race vs Poverty - White
White <- ggplot(schooldata, aes(x=`% Poverty`, y=`% White`)) +
  geom_point(shape=1) + ggtitle("Percent White") +
  geom_smooth(method=lm, se=FALSE)

#Scatterplot Race vs Poverty - Hispanic
Hispanic <- ggplot(schooldata, aes(x=`% Poverty`, y=`% Hispanic`)) +
  geom_point(shape=1) +   ggtitle("Percent Hispanic") +
  geom_smooth(method=lm, se=FALSE)

#Scatterplot Race vs Poverty - Black
Black <- ggplot(schooldata, aes(x=`% Poverty`, y=`% Black`)) +
  geom_point(shape=1) +    ggtitle("Percent Black") +
  geom_smooth(method=lm, se=FALSE)

#Scatterplot Race vs Poverty - Asian
Asian <- ggplot(schooldata, aes(x=`% Poverty`, y=`% Asian`)) +
  geom_point(shape=1) +    ggtitle("Percent Asian") +
  geom_smooth(method=lm, se=FALSE)

ggplot2.multiplot(White, Hispanic, Black, Asian, cols = 2)
