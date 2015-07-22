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


#Scatterplot Race vs poverty by Math mean scale score
math_asian <- ggplot(schooldata, aes(x=`% Poverty`, y=`% Asian`, color = `Mean Scale Score Math`)) +
  geom_point(shape=1) +    ggtitle("Percent Asian") + ylim(0.0, 1.0) +
  geom_smooth(method=lm, se=FALSE)

math_black <-math_asian <- ggplot(schooldata, aes(x=`% Poverty`, y=`% Black`, color = `Mean Scale Score Math`)) +
  geom_point(shape=1) +    ggtitle("Percent Black") + ylim(0.0, 1.0) +
  geom_smooth(method=lm, se=FALSE)

math_white <-math_asian <- ggplot(schooldata, aes(x=`% Poverty`, y=`% White`, color = `Mean Scale Score Math`)) +
  geom_point(shape=1) +    ggtitle("Percent White") + ylim(0.0, 1.0) +
  geom_smooth(method=lm, se=FALSE)

math_hispanic <-math_asian <- ggplot(schooldata, aes(x=`% Poverty`, y=`% Hispanic`, color = `Mean Scale Score Math`)) +
  geom_point(shape=1) +    ggtitle("Percent Hispanic") + ylim(0.0, 1.0) +
  geom_smooth(method=lm, se=FALSE)

#Scatterplot Race vs poverty by Math mean scale score
eng_asian <- ggplot(schooldata, aes(x=`% Poverty`, y=`% Asian`, color = `Mean Scale Score English`)) +
  geom_point(shape=1) +    ggtitle("Percent Asian") + ylim(0.0, 1.0) +
  geom_smooth(method=lm, se=FALSE)

eng_black <-math_asian <- ggplot(schooldata, aes(x=`% Poverty`, y=`% Black`, color = `Mean Scale Score English`)) +
  geom_point(shape=1) +    ggtitle("Percent Black") + ylim(0.0, 1.0) +
  geom_smooth(method=lm, se=FALSE)

eng_white <-math_asian <- ggplot(schooldata, aes(x=`% Poverty`, y=`% White`, color = `Mean Scale Score English`)) +
  geom_point(shape=1) +    ggtitle("Percent White") + ylim(0.0, 1.0) +
  geom_smooth(method=lm, se=FALSE)

eng_hispanic <-math_asian <- ggplot(schooldata, aes(x=`% Poverty`, y=`% Hispanic`, color = `Mean Scale Score English`)) +
  geom_point(shape=1) +    ggtitle("Percent Hispanic") + ylim(0.0, 1.0) +
  geom_smooth(method=lm, se=FALSE)

ggplot2.multiplot(math_asian, math_black, math_white, math_hispanic, eng_asian, eng_black, eng_white, eng_hispanic, col = 4)
