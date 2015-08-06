
## By:Glenda A
##################################################################################################
            ## A ui.R ui ==> controls the layout, apperiance, and widgets 33 ##
##################################################################################################

### Loading the required libraries
library(shiny)
library(leaflet)
library(ggmap) 
library(dplyr)
library(reshape2) 

### Define UI for the shiny application here  
shinyUI(fluidPage(
  ## titlePanel creates the title for the name of the application
  titlePanel(h3('Prices For The High-Performing Public Schools in NYC', align = "center")),
  
  ## sidebarLayout creates a layout with a sidebar and main area
  sidebarLayout(
    sidebarPanel(
              textInput("address", "Enter your your address", "New York City"),
              radioButtons("bedrooms", "Select Number of Beds", list( "0","1", "2", "3"), "1"), 
              radioButtons("baths", "Select Number of Baths", list("1", "2"), "1")),
              
    mainPanel(
      tabsetPanel(
        tabPanel("Price Average For ES", leafletOutput("mymap")), 
        tabPanel("Median Price For ES", leafletOutput("median_map")),
        tabPanel("Premium Price For ES", leafletOutput("premium_map"))
      )
    )
  )
  )
)
