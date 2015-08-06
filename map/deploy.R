
setwd("~/public-schools/map")
library(devtools)
library(shinyapps)
#devtools::install_github('rstudio/shinyapps')
#shinyapps::setAccountInfo(name='nycschoolcost',
#                          token='0B16193697E62F0C4328AD0D3C58887A',
#                          secret='uE9gJY9k52J/ZDyVftdQdPwEWcye1Ry1wINPt0/T')
shinyapps::setAccountInfo(name='nycschoolcost',
                          token='D1C1723B1E807551EA84FEBE1C0A09A3',
                          secret='bYSNTfO6mu5lnR+CST3VEx2g0ziNiE6xPiBIE7Hs')
#shinyapps::removeAccount("nycschoolpremium")
shinyapps::deployApp(appName="map")

