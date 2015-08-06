
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
library(stringr)
library(reshape2) 

### Loading the required dataframes for the shiny application
load("../Glenda_R_Code/shiny_app_df.RData")
load("../Glenda_R_Code/fakeDataWPremiums.RData")

### Create the server.R for computation purposes
shinyServer(
  function(input, output, session) {
    
    ## Outputs on the main panel what the user inputs ==> I took it out :D
    output$mylongitude <- reactive({as.numeric(geocode(input$address)[,1])})
    output$mylatitude <- reactive({as.numeric(geocode(input$address)[,2])})
    
    ## Goes through the geocoding folder and find uses the folders we want to use
    source("../geocoding/find_school_district_by_address.R")
    filepath <- "../geocoding/2013_2014_School_Zones_8May2013"
    shapefile <- "ES_Zones_2013-2014"
    
    # Get school boundaries from NYC opendata shapefiles
    school_zone_boundaries <- create_school_mapdata(filepath, shapefile)
    myZone <- school_zone_boundaries$DBN
  
    
    ga$bedbath = paste(ga$bedrooms, ga$baths, sep="")
    
       
#########################################################################################
                            ####  Creating different tabs ####
#########################################################################################
## first tab: Tab for finding the average price    
    casted_ga <- dcast(ga, DBN~bedbath, value.var="avg_price")
    ## create a school_ga variable to joins the school zone boundaries with the ga file by the DBN   
    schools_ga<- left_join(school_zone_boundaries@data, casted_ga, by="DBN")
    
    ## Create a continuous palette function that changes the average price map color
    pal1 <- colorNumeric(  ## 1 = yellow, 2 = dark green, 3=pink,  4 = blue
      palette = c("#ffeb3b","#33e77f" , "#f66196", "#3d5afe"),
      domain = ga$avg_price
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
          weight = 1, 
          smoothFactor = 0.2, fillOpacity = .7,
          fillColor = ~pal1(schools_ga[,paste(input$bedrooms, input$baths, sep="")]), 
          popup=paste0("<strong>Name: </strong>", school_zone_boundaries$DBN, "<strong> \nPrice: </strong>$", 
                      format(round(schools_ga[,paste(input$bedrooms, input$baths, sep="")],2),nsmall=2), sep=""
                     )
      )
    })
    
### Second tab
    casted_ga2 <- dcast(ga, DBN~bedbath, value.var="med_price")
    ## create a school_ga variable to joins the school zone boundaries with the ga file by the DBN   
    schools_ga2<- left_join(school_zone_boundaries@data, casted_ga2, by="DBN")
    
    #school_zone_boundaries$med_price <- school_zone_boundaries@data$med_price
    #my_med_price <- school_zone_boundaries$med_price
    
    ## Create a continuous palette function that changes the average price map color
    pal2 <- colorNumeric(
      palette = c("#ffeb3b","#33e77f" , "#f66196", "#3d5afe"),
      domain = ga$med_price
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
          weight = 1, 
          smoothFactor = 0.2, fillOpacity = .7,
          fillColor = ~pal2(schools_ga2[,paste(input$bedrooms, input$baths, sep="")]), 
          popup=paste0("<strong>Name: </strong>", school_zone_boundaries$DBN, "<strong> \nPrice: </strong>$", 
                       format(round(schools_ga2[,paste(input$bedrooms, input$baths, sep="")],2),nsmall=2)
          
          )
        )
    })
    
## Third Tab  
    grouped_fake <- fakeDataWPremiums %>% 
      group_by(DBN, bedrooms, baths) %>% 
      summarise(premium_avg = mean(premium), 
                negborhood_str = paste0(paste(neighborhood,  format(round(premium, 2), nsmall = 2), sep = ": "), 
                                        collapse = "", sep = "<br>"))
    ## Join the school zone boundaries@data with the premium average price by DBN
    grouped_fake$bedbath = paste(grouped_fake$bedrooms, grouped_fake$baths, sep="")
    casted_fake <- dcast(grouped_fake, DBN~bedbath, value.var="premium_avg" )
    schools_fake <- left_join(school_zone_boundaries@data, casted_fake, by="DBN")

    ## Define the pal3
    pal3 <- colorNumeric(
      palette = c("#ffeb3b","#33e77f" , "#f66196", "#3d5afe"),
      domain = grouped_fake$premium_avg
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
          weight = 1, 
          smoothFactor = 0.2, fillOpacity = .7,
          fillColor = ~pal3(schools_fake[,paste(input$bedrooms, input$baths, sep="")]), 
          popup=paste0("<strong>Name: </strong>", school_zone_boundaries$DBN, "<strong> \nPremium: </strong>$", 
                       format(round(schools_fake[,paste(input$bedrooms, input$baths, sep="")],2),nsmall=2)
                       
          )
        )
      
    })

    
  }

)