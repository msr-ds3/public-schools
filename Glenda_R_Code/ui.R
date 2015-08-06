

##################################################################################################
##    Creates a user interface in ui.R ui ==> controls the layout, apperiance, and widgets 33 ##
########### An open-source JavaScript library for mobile-friendly interactive maps ###############
##################################################################################################

## Setting the working directory
setwd("~/public-schools/Glenda_R_Code")

### Load the packages and data
library(shiny)
library(plotGoogleMaps)
library(leaflet)
library(ggmap) #geocoding
library(dplyr)

### Define UI for the shiny application here  
shinyUI(fluidPage(
  ## titlePanel creates the title for the name of the application
  titlePanel(h3('Prices For The High-Performing Public Schools in NYC', align = "center")),

#     ## Making diverse menus
#   selectInput("Prices For Different District", 
#               "Prices For Different District", 
#               c("Average Price For Each School District"  = "avg_price_menu",
#                 "Mediam Price For Each School District"  = "med_price_menu",
#                 "Premium Price For each School District"  = "pre_price_menu")
#               ),
  
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
