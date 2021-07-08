## This code is to create a shiny APP for the platypus month data 

## It does the following

#1 load libraries
#2 Read in the platypus month data 
#3 set up a ui for the shiny app





#1 load libraries 

install.packages("dplyr")

install.packages("tidyr")

install.packages("lubridate")

install.packages("ggplot2")

install.packages("shiny")

install.packages("leaflet")

install.packages("plotly")


library(dplyr)

library(tidyr)

library(lubridate)

library(ggplot2)

library(shiny)

library(leaflet)

library(plotly)

#2 Read in platypus month data 

p <- read.csv("app_data.csv")

str(p)

p <- p %>% mutate(
  longitude = as.numeric(longitude),
  date = as.Date(date, format = "%d/%m/%Y")
)


#3 set up a ui for the shiny app

ui <- fluidPage(titlePanel("Platypus observations - ACT waterwatch"),
        sidebarLayout(
          sidebarPanel(
            
            #select species 
            selectInput(inputId = "species", label = "Species",
                        choices = unique(p$species),
                        selected = "platypus"),
            
            
            
            #select data type
            selectInput(inputId = "type", label = strong("Type of data"),
                        choices = unique(p$type),
                        selected = "survey"),
            
            # select site
            selectInput(inputId = "site", label = "Site",
                        choices = unique(p$site)),
            selected = "Cooma_Ck"),
          
          # select the time period for the output
          dateRangeInput(inputId = "date", 
                        label = "Date range",
                        format = "dd/mm/yyyy",
                        separator = "to"),
          
          # Add leaflet map
          leafletOutput("my_map")),
          
          #Output figure and table
        
        mainPanel
        (plotlyOutput(outputId = "barplot", height = "400px"),
          ))





#4 Define server function 
          
 server <-  function (input, output, session) {
   
   #subset data by species
   selected_species = reactive ({
      p %>%
     filter(species == input$species )
   })
   
   # select the type of data to be used
   selected_data = reactive ({
     p %>%
       filter(Type == input$type )
   })
   
   # select the site 
   selected_site  = reactive ({
     p %>%
       filter(Type == input$site_id )
   })
   
   # select the dat range for the output
   
   slected_date_range <- reactive ({req(input$daterangeInput)})
   
   # add map
   output$my_map <- renderleaflet({
     leaflet () %>%
       addTiles() %>%
       addMarkers(data = p, lng = `longitude, lat = ~latitude, layerID = ~`)
   })
   
   
 }        
          
          
str(p)          
          
          
 Canb <- leaflet () %>%
   addTiles() %>%
   fitBounds(lng1 = 149.0, lat1 = -35.0, lng2 = 150.0, lat2 = -36.0)%>%
   addMarkers(data = p, ~longitude, ~latitude)

Canb
















