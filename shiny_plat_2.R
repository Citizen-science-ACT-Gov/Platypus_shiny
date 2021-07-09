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


library(dplyr)

library(tidyr)

library(lubridate)

library(ggplot2)

library(shiny)

library(leaflet)

library(plotly)

#2 Read and tidy platypus month data 

p <- read.csv("survey_data.csv")


p <- p %>% mutate(
  date = as.Date(date, format = "%d/%m/%Y"),
  species = trimws(species)
)


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
    column(3, #select species 
      wellPanel(selectInput(inputId = "species", label = "Species",
                  choices = unique(p$species),
                  selected = "platypus"),
      
         # select site
      selectInput(inputId = "site", label = "Site",
                  choices = unique(p$site_id),
                  selected = "Cooma_Ck"),
      
      # select the time period for the output
      dateRangeInput(inputId = "date", 
                     label = "Date range",
                     format = "dd/mm/yyyy",
                     start = "2014-01-01",
                     separator = "to"))),
      column(9, plotOutput(outputId = "col_plot", height = "400px"))),
  
  fluidRow(
      
     
      # Add leaflet map
      column(3,
             h2("Site map"),
             "Click on Platypus icon to show site name",
             
             leafletOutput("my_map")),
    
    
      column(3,
             h2("Data summary table"),
             dataTableOutput(outputId = "survey_table")),
      
      column(4, 
             tags$br(),
             tags$br(),
             h2("Details of the surveys"),
             tags$br(),
             "The data presented is collected by volunteers during August of each year as part of the Platypus month program. 
             During the surveys data are collected at 10 sites along a reach of the waterway. 
             Reach length is generally ranges for 800 m to 1 km.  ", 
             tags$br(),
             tags$br(),
             tags$br(), 
             tags$br(),
             tags$br()),
      column(2, 
      # add the Waterwatch logo to the bottom right hand corner of the map
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(), 
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(), 
      tags$br(),
      tags$br(),
      tags$br(), 
      tags$br(),
      tags$br(),
      tags$img(height = 100, width = 250,
               src = "https://f079a2602f82498e927c63b0219b50f9.app.rstudio.cloud/file_show?path=%2Fcloud%2Fproject%2FWaterwatch_logo_Upper_Murrumbidgee.png"),
      
      )
    
  ))



#4 Define server function 

html_legend <- "<img src='https://github.com/Citizen-science-ACT-Gov/platy_images/blob/main/platy_image.png'>survey<br/>
<img src='https://github.com/Citizen-science-ACT-Gov/platy_images/blob/main/platy_image_2.png'>Adhoc"






server <- function (input, output) {
  output$my_map <- renderLeaflet({
    
    leaflet(p) %>%
      addTiles() %>%
      fitBounds(lng1 = 148.8, lat1 = -35.1, lng2 = 149.35, lat2 = -36.3)%>%
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(~longitude, ~latitude, icon = platyIcons[p$Type], popup = (p$site_id), labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                                                                                    style = list(
                                                                                                                      "color" = "black",
                                                                                                                      "font-family" = "serif",
                                                                                                                                                                                                                                        "font-size" = "12px"
                                                                                                                    )))
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
  
  # Create a reactive plot based on the inputs listed
  
  output$col_plot <- renderPlot({
    ggplot(plot_data(), )+
      geom_col(aes(date, number, colour = "Animals observed"))+
      geom_point(aes(date, Survey, colour = "Survey conducted"))+
      ggtitle("Number of animals observed")+
      scale_x_date(name = "Date",  date_breaks = "2 months", date_labels = "%m/%y", limits = c(input$date[1], input$date[2])) +
      ylab("Number of individuals")+
      scale_colour_manual(values = c("black", "Red"))+
      labs(colour = "Legend")+
      cleanup +
      theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "blue4", hjust = 0.5),
            axis.title = element_text(size = (15), colour = "blue4", face = "bold"))
    
  })
  
 
  
  
  # subset data for table 
  table_data <- reactive({
    p %>%
      filter(
        species == input$species &
        site_id == input$site) %>%
      mutate(year = substr(date, 1,4))%>%
      select("year", "number")%>%
      group_by(year)%>%
      summarise("Maximum number of animals detected" = max(number), "Average animals per survey" = mean(number), "Surveys conducted" = n())
  })
  
  
  
  
  ## Create reactive table for data 
  
  output$survey_table <- renderDataTable({table_data()})
  
  
  
}


# 5 run the shiny APP   



shinyApp(ui = ui, server = server)


# establish and connect to shinyapps.io 

install.packages('rsconnect')

rsconnect::setAccountInfo(name='citizen-science-act-gov', token='6BAD85804410F294674F83FE6AD75B0C', secret='LWBMqwGfkVcSqOQvw1rSNLne6k3mH2dsQjrYMGpF')



p %>%  filter(
  site_id == "Cooma_Ck") %>%
  mutate(year = substr(date, 1,4))%>%
  group_by(year)%>%
  summarise(max(number))



ggplot(p,  )+
  geom_col(aes(date, number, colour = "Green"))+
  geom_point(aes(date, Survey, colour = "Red"))+
  ggtitle("Number of animals observed")+
  scale_x_date(name = "Date",  date_breaks = "2 months", date_labels = "%m/%y") +
  ylab("Number of individuals")+
  labs(colour = "Legend")+
  scale_colour_manual(values = c("black", "Red"),
                      labels = c("Animals observed", "Survey conducted"))+
  cleanup +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "blue4", hjust = 0.5),
        axis.title = element_text(size = (15), colour = "blue4", face = "bold"))



p %>% filter(date >= "2019-01-01") %>%
  distinct(site_id)



