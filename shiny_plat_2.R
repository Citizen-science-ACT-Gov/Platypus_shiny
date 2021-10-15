## This code is to create a shiny APP for the platypus month data 

## It does the following

#1 load libraries
#2 Read in the platypus month data 
#3 set up a ui for the shiny app





#1 load libraries 

#install.packages("dplyr")

#install.packages("tidyr")

#install.packages("lubridate")

#install.packages("ggplot2")

#install.packages("shiny")

#install.packages("leaflet")

#install.packages("plotly")
# install.packages("DT")


library(dplyr)

library(tidyr)

library(lubridate)

library(ggplot2)

library(shiny)

library(leaflet)

library(plotly)

library(DT)

#2 Read and tidy platypus month data 

platy <- read.csv("survey_data.csv")


platy <- platy%>% mutate(
  date = as.Date(date, format = "%d/%m/%Y"),
  species = trimws(species),
  site_id = as.factor(site_id)
)

reaches <- platy %>% select("site_id", "latitude", "longitude") %>%
  unique()


cleanup <- theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(color = "black"),
                 panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = 0.1),
                 axis.text.x = element_text(color = "black"),
                 axis.text.y = element_text(color = "black"))

platyIcons <- iconList(
  "survey" = makeIcon("platy_image.png", 40, 40),
  "adhoc" = makeIcon("platy_image_2.png", 40, 40)
)




#3 set up a ui for the shiny app

ui <- fluidPage(tags$head(
  # Note the wrapping of the string in HTML()
  tags$style(HTML("
      body {
        background-color: powderblue;
        color: midnightblue;
      }
      "))
  ),
  # App title 
  fluidRow(h1("Platypus and Rakali observations - ACT Waterwatch", align = "center", style = "color:midnightblue;")),
  fluidRow(
    column(3,
           br(),
           br(),
           br(),
           br(),
           br(),
           
           #select species 
      wellPanel(selectInput(inputId = "species", label = "Species",
                  choices = unique(platy$species),
                  selected = "platypus"),
      
         # select site
      selectInput(inputId = "site", label = "Site",
                  choices = unique(platy$site_id),
                  selected = "Cooma_Ck"),
      
      # select the time period for the output
      dateRangeInput(inputId = "date", 
                     label = "Date range",
                     format = "dd/mm/yyyy",
                     start = "2014-01-01",
                     separator = "to"))),
      column(9, 
             h2(textOutput("figurehead")),
             plotlyOutput(outputId = "col_plot", height = "400px"),
    "Figure 1. Number of sightings. Red dot indicates that a survey was conducted and bars indicate the 
    number of individuals observed")
    ),
  
  fluidRow(
      
     
      # Add leaflet map
      column(3,
             h2("Site map"),
             "Click on Platypus icon to show site name and select site.",
             
             leafletOutput("mymap")),
    
      # Add summary data table
      column(2,
             h2("Data summary table"),
             DT::dataTableOutput(outputId = "survey_table")),
      
      column(5, 
             
             h2("Details of the surveys"),
             tags$br(),
             "Platypus month is conducted during August. 
             The sites currently being surveyed are Cooma Creek, Jerrabomberra Creek, below Coppins Crossing, Mittagang Crossing, Point Hut Crossing, Scottsdale, Queanbeyan and Tidbinbilla. 
             Data from some sites that have been previously included in the survey are also available. 
             Currently, each year four surveys are conducted at each site to keep survey effort consistent across the sites. 
             In previous years a different number of surveys were conducted at the different sites. At each site, 8 to 10 points are surveyed along a section of 800 m to 1 km. 
             At each site surveys are conducted at dawn and dusk when Platypus are most active.", 
            ),
     
     column(2, tags$img(height = 100, width = 250,
               src = "https://f079a2602f82498e927c63b0219b50f9.app.rstudio.cloud/file_show?path=%2Fcloud%2Fproject%2FWaterwatch_logo_Upper_Murrumbidgee.png"))
     ))
    




#4 Define server function 

html_legend <- "<img src='https://github.com/Citizen-science-ACT-Gov/platy_images/blob/main/platy_image.png'>survey<br/>
<img src='https://github.com/Citizen-science-ACT-Gov/platy_images/blob/main/platy_image_2.png'>Adhoc"






server <- function (input, output, session) {
  output$mymap <- renderLeaflet({
    
    leaflet(reaches) %>%
      addTiles() %>%
      fitBounds(lng1 = 148.8, lat1 = -35.1, lng2 = 149.35, lat2 = -36.3)%>%
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(data = reaches, ~longitude, ~latitude, layerId = ~site_id, 
                  popup = ~site_id, icon = platyIcons[p$Type],
                 labelOptions = labelOptions(noHide = T, direction = "bottom", 
                                             style = list("color" = "black",
                                                          "font-family" = "serif",
                                                          "font-size" = "12px")))
                 })
  leafletOutput('mymap', height = 600)
  
  # subset data for plot 
  
  plot_data <- reactive({
    p %>%
      filter(
        species == input$species &
          site_id == input$site &
          date >=  input$date[1] &
          date <= input$date[2])
  })
  
 
  # update selected site based on map click
  
  observeEvent(input$mymap_marker_click, {
    p <- input$mymap_marker_click
    
    #updateSelectInput(session, "site", selected = p$Siteid) 
    updateSelectInput(session, "site", "Update my site", selected = p$id)
  })
  
  
  

  
  # Create a reactive plot based on the inputs listed
  output$figurehead <- renderText({paste0(input$species, " surveys at ", input$site)})
  
  output$col_plot <- renderPlotly({
    ggplotly(
      ggplot(plot_data(), )+
      geom_col(aes(date, number, colour = "Animals observed"))+
      geom_point(aes(date, Survey, colour = "Survey conducted"))+
      ggtitle("Number of animals observed")+
      scale_x_date(name = "Date",  date_breaks = "6 months", date_labels = "%m/%y", limits = c(input$date[1], input$date[2])) +
      ylab("Number of individuals")+
      scale_colour_manual(values = c("black", "Red"))+
      labs(colour = "Legend")+
      cleanup +
      theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "blue4", hjust = 0.5),
            axis.title = element_text(size = (15), colour = "blue4", face = "bold")))
    
  })
  
 
  
  
  # subset data for table 
  table_data <- reactive({
    platy %>%
      filter(
        species == input$species &
        site_id == input$site) %>%
      mutate(year = substr(date, 1,4))%>%
      select("year", "number")%>%
      group_by(year)%>%
      summarise("Max count" = max(number), "Surveys conducted" = n())
  })
  
  
  
  
  ## Create reactive table for data 
  
  output$survey_table <- DT::renderDataTable({DT::datatable(table_data (), options=list(iDisplayLength=5,                    # initial number of records
                                                                                                       aLengthMenu=c(5,10),                  # records/page options
                                                                                                       bLengthChange=0,                       # show/hide records per page dropdown
                                                                                                       bFilter=0,                                    # global search box on/off
                                                                                                       bInfo=0)) })
  
  
  
}


# 5 run the shiny APP   

shinyApp(ui = ui, server = server)





# establish and connect to shinyapps.io 

install.packages('rsconnect')

rsconnect::setAccountInfo(name='citizen-science-act-gov', token='6BAD85804410F294674F83FE6AD75B0C', secret='LWBMqwGfkVcSqOQvw1rSNLne6k3mH2dsQjrYMGpF')











