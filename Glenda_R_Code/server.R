
##############################################################################################
#######   Create the server.R for computation purposes: It uses the input provided by  #######
####### the user process them and produces the required output displayed by ui.r script  #####
##############################################################################################

### Install required libraries
library(shiny)
library(leaflet)
library(ggmap)
library(dplyr)
library(raster)

#HW:##################!! Remaind Jake and Amit to host the app!! ##############
#HW2: Make stable NYC map for the price averages the price search location 

### Loading the required dataframes for the shiny application
load("../Glenda_R_Code/shiny_app_df.RData")
load("../Glenda_R_Code/fakeDataWPremiums.RData")

### Create the server.R for computation purposes
shinyServer(
  function(input, output, session) {
    
    ##outputs on the main panel what the user inputs ==> I took it out :D
    output$mylongitude <- reactive({as.numeric(geocode(input$address)[,1])})
    output$mylatitude <- reactive({as.numeric(geocode(input$address)[,2])})
    
    ## Goes through the geocoding folder and find uses the folders we want to use
    source("../geocoding/find_school_district_by_address.R")
    filepath <- "../geocoding/2013_2014_School_Zones_8May2013"
    shapefile <- "ES_Zones_2013-2014"
    
    # Get school boundaries from NYC opendata shapefiles
    school_zone_boundaries <- create_school_mapdata(filepath, shapefile)
    school_zone_df <-fortify(school_zone_boundaries)
    myZone <- school_zone_boundaries$DBN
    #school_zone_boundaries$price <- school_zone_boundaries@data$avg_price
  
    
    sum_ga <- ga %>% 
      group_by(DBN) %>% 
      summarise(mean_price = mean(avg_price), 
                neighborhood_str = paste0(paste(paste(bedrooms, baths, sep=""), avg_price, sep=":"), 
                                          collapse = "", sep = "<br>"))
    
    ## create a school_ga variable to joins the school zone boundaries with the ga file by the DBN   
    schools_ga<- left_join(school_zone_boundaries@data, sum_ga, by="DBN")
    myprice <- schools_ga$price
    
       
#########################################################################################
                            ####  Creating different tabs ####
#########################################################################################
## first tab: Tab for finding the average price    
    
    ## Create a continuous palette function that changes the average price map color
    pal1 <- colorNumeric(  ## 1 = yellow, 2 = dark green, 3=pink,  4 = blue
      palette = c("#ffeb3b","#33e77f" , "#f66196", "#3d5afe"),
      domain = myprice
    )
    
    ## Plot the average price per school boundary
    output$mymap <- renderLeaflet({
      leaflet(school_zone_boundaries) %>% 
        setView(lng = geocode(input$address)[,1],
                lat = as.numeric(geocode(input$address)[,2]), 
                zoom = 10)  %>%
        addTiles() %>%
        addMarkers(
          as.numeric(geocode(input$address)[,1]), 
          as.numeric(geocode(input$address)[,2]), 
          popup="Your Average Price"
        ) %>%
        addPolygons(
          data= school_zone_boundaries, 
          weight = 2, 
          smoothFactor = 0.2, fillOpacity = .5,
          fillColor="grey",
      
      #    color = ~pal1(schools_ga[schools_ga$bedrooms == input$bedrooms & 
      #                              schools_ga$baths == input$baths,]$avg_price), 
          popup=paste0("<strong>Name:</strong>", school_zone_boundaries$DBN)#, "<strong> \nPrice:$</strong>", 
      #                 schools_ga[schools_ga$bedrooms == input$bedrooms & 
      #                              schools_ga$baths == input$baths,]$avg_price
      #               )
      )
    })
    
### Second tab
    
    #school_zone_boundaries$med_price <- school_zone_boundaries@data$med_price
    #my_med_price <- school_zone_boundaries$med_price
    
    ## Create a continuous palette function that changes the average price map color
    pal2 <- colorNumeric(
      palette = c("#ffeb3b","#33e77f" , "#f66196", "#3d5afe"),
      domain = schools_ga$med_price
    )
    
    ## Plot the average price per school boundary
    output$median_map <- renderLeaflet({
      leaflet() %>% 
        setView(lng = geocode(input$address)[,1],
                lat = as.numeric(geocode(input$address)[,2]), 
                zoom = 10)  %>%
        addTiles() %>%
        addMarkers(
          as.numeric(geocode(input$address)[,1]), 
          as.numeric(geocode(input$address)[,2]), 
          popup="Your Med Avg Price"
        ) %>%
        addPolygons(
          data= school_zone_boundaries, 
          weight = 2, 
          smoothFactor = 0.2, fillOpacity = .5,
          color = ~pal2(schools_ga[schools_ga$bedrooms == input$bedrooms & 
                                    schools_ga$baths == input$baths,]$med_price), 
          popup=paste0("<strong>Name:</strong>", myZone, "<strong> \nPrice:$</strong>", 
                       format(round(schools_ga[schools_ga$bedrooms == input$bedrooms & 
                                                 schools_ga$baths == input$baths,]$avg_price, 2), nsmall = 2)
          )
        )
    })
    
## Third Tab  
    x <- fakeDataWPremiums %>% 
      group_by(DBN, bedrooms, baths) %>% 
      summarise(premium_avg = mean(premium), 
                negborhood_str = paste0(paste(neighborhood,  format(round(premium, 2), nsmall = 2), sep = ": "), 
                                        collapse = "", sep = "<br>"))
    ## Join the school zone boundaries@data with the premium average price by DBN
    schools_fake <- left_join(school_zone_boundaries@data, x, by="DBN")

    ## Define the pap2
    pal3 <- colorNumeric(
      palette = c("#ffeb3b","#33e77f" , "#f66196", "#3d5afe"),
      domain = schools_fake$premium
    )
    
    output$premium_map <- renderLeaflet({
      leaflet() %>% 
        setView(lng=-74.0059, 
                lat=40.7127,
                zoom = 10) %>%
        addTiles() %>%
        addMarkers(lng=-74.0059, lat=40.7127, 
                   #when the users hovers throuh every school district shows the av_price 
                   popup="Your Premium Price"
        ) %>%
        addPolygons(
          data=school_zone_boundaries, 
          weight = 2, 
          smoothFactor = 0.2, fillOpacity = .5,
          color = ~pal3(schools_fake[schools_fake$bedrooms == input$bedrooms & 
                                    schools_fake$baths == input$baths,]$premium), 
          popup=paste0("<strong>Name:</strong>", myZone, "<br /><strong> \nPrice:$</strong>", 
                       format(round(schools_fake[schools_fake$bedrooms == input$bedrooms & 
                                                   schools_fake$baths == input$baths,]$premium, 2), 
                              nsmall = 2), schools_fake$negborhood_str
          )
        )
      
    })

    
  }

)