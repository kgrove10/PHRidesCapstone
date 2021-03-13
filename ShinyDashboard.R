library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(tidyverse)
library(rio)
library(shinythemes)
library(shinyjs) 
library(bslib)
library(sp)
library(maptools)
library(shinyWidgets)
library(leaflet.extras)
library(shinycssloaders)

#read in data created in DataPrep.R
outofhub <- readRDS(file = "./ShinyData/outofhub.rds")

repaircoord <- readRDS(file = "./ShinyData/repaircoord.rds")

destroycoord <- readRDS(file = "./ShinyData/destroycoord.rds")

stationdescriptions <- readRDS("./ShinyData/stationdescript.rds")

noendstation <- readRDS(file = "./ShinyData/noendstation.rds")

bikelines <- readRDS(file = "./ShinyData/bikelines.rds")

descriptive <- readRDS(file = "./ShinyData/fordescriptives.rds")

bikestats <- readRDS(file = "./ShinyData/bikestats.rds")

#create icons for use in markers
icons <- awesomeIcons(
  icon = 'fa-bicycle',
  iconColor = '#FFFFFF',
  library = 'fa',
)

s_icon <- awesomeIcons(  
  icon = 'fa-bicycle',
  iconColor = "#FFFFFF",
  markerColor = "black",
  library = "fa"
)

n_icon <- awesomeIcons(
  icon = 'fa-bicycle',
  iconColor = "#000000",
  markerColor = "lightgray",
  library = "fa"
)

# customize waiting spinner
options(spinner.color = "#99b8d4", spinner.color.background = "#E9F4FF", spinner.size = 2)

#shiny App:
ui <- fluidPage(navbarPage(title = "PeaceHealth Rides Visualizations", 
                          theme = bs_theme(bg = "#E9F4FF", fg = "#000000", primary = "#27598e", 
                                           secondary = "#99b8d4", base_font = font_google("Signika"), 
                                           `enable-gradients` = FALSE, `enable-shadows` = FALSE, bootswatch = "slate"),
                          
                          tabPanel("Out-of-Station Bikes",
                                   sidebarLayout(fluid = TRUE,
                                                 sidebarPanel(
                                                   tags$head(
                                                     tags$style(type="text/css", 
                                                                "label.control-label, .selectize-control.single {display: table-cell; text-align: left; vertical-align: middle;} 
                                                                label.control-label {padding-right: 10px; width: 50px; text-align: right;}
                                                                .form-group {display: table-row;}
                                                                .selectize-control.single div.item {padding-right: 15px; width: 160px;}")),
                                                   div(style = "text-align:center", 
                                                       "This map displays the distribution of out-of-station PeaceHealth Rides bikes.
                                                       Each circle indicates the number of bikes in that area. Hover over the circle to see which area it encompasses, 
                                                       or click to zoom in."),
                                                   div(style = "text-align:center",
                                                       "Single instances are represented by a single black dot.
                                                        PeaceHealth Rides Stations are indicated with blue markers."),
                                                   br(),
                                                   div(style = "text-align:center",
                                                        "Use the dropdown boxes below to filter for specific years, months, or days of the week."),
                                                   br(),
                                                     selectInput("year", label = "Year:",
                                                               choices = c("All", "2018", "2019", "2020", "2021"),
                                                               width = "500px"),
                                                   selectInput("month",label = "Month:",
                                                               choices = c("All", "Jan", "Feb", "Mar", "Apr",
                                                                           "May", "Jun", "Jul", "Aug", "Sep",
                                                                           "Oct", "Nov", "Dec")),
                                                   selectInput("day",label = "Weekday:",
                                                               choices = c("All", "Mon", "Tue", "Wed",
                                                                           "Thu", "Fri", "Sat", "Sun")),
                                                   br(),
                                                   h6(style = "text-align:center", "Number of Out-of-Station Bikes:"),
                                                   textOutput("summarystats1"),
                                                   tags$style(type="text/css", "#summarystats1 {text-align:center;}"),
                                                   br(),
                                                   width = 3
                                                 ),
                                                 mainPanel(tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                                           withSpinner(leafletOutput(outputId = "map"), type = 2),
                                                           width = 9
                                                 )
                                   )
                          ),
                          tabPanel("System Repair & Destruction",
                                   sidebarLayout(fluid = TRUE,
                                                 sidebarPanel(
                                                   div(style = "text-align:center;",
                                                   "Selecting a station and direction below displays a heatmap of the end point of destructive trips originating from this station (\"Leave Station\"), or
                                                   the starting points of repair trips ending at this station (\"Return to Station\")."),
                                                   div(style = "text-align:center; font-size:9px; color:#d2dce6", "---"),
                                                   div(style = "text-align:center; font-size:13px;",
                                                       "Destructive trips are those where bikes are taken from a station but left out-of-station, 
                                                   and repair trips are those in which an out-of-station bike is returned to a station."),
                                                   br(),
                                                   selectInput("station", label = "Station:",
                                                               choices = str_sort(stationdescriptions$start_hub)),
                                                   br(),
                                                   prettyRadioButtons("triptype",
                                                                      label = "Direction:",
                                                                      choices = list( 
                                                                                     "Leave Station" = 2,
                                                                                     "Return to Station" = 1), 
                                                                      selected = 2,
                                                                      status = "primary",
                                                                      shape = "round",
                                                                      fill = TRUE,
                                                                      animation = "jelly",
                                                                      plain = FALSE,
                                                                      thick = FALSE,
                                                                      bigger = FALSE,
                                                                      inline = TRUE
                                                   ),
                                                   br(),
                                                   selectInput("year2", label = "Year:",
                                                               choices = c("All", "2018", "2019", "2020", "2021"),
                                                               selected = "All"),
                                                   selectInput("month2",label = "Month:",
                                                               choices = c("All", "Jan", "Feb", "Mar", "Apr",
                                                                           "May", "Jun", "Jul", "Aug", "Sep",
                                                                           "Oct", "Nov", "Dec"),
                                                               selected = "All"),
                                                   selectInput("day2",label = "Weekday:",
                                                               choices = c("All", "Mon", "Tue", "Wed",
                                                                           "Thu", "Fri", "Sat", "Sun"),
                                                               selected = "All"),
                                                   h6("Summary Statistics:", style = "text-align:center"),
                                                   fluidRow(column(7,
                                                                   div("Number of Trips:", style = 'text-align:right')
                                                                   ), 
                                                            column(5,
                                                                   div(textOutput("summarystats2_1"), style = 'text-align:left'),
                                                                   style = "padding:0px;")),
                                                   fluidRow(column(7,
                                                                   div("Average Trip Duration:", style = 'text-align:right')), 
                                                            column(5,
                                                                   div(textOutput("summarystats2_2"), style = 'text-align:left'),
                                                                   style = "padding:0px;")),
                                                   fluidRow(column(7,
                                                                   div("Average Trip Distance:", style = 'text-align:right')), 
                                                            column(5,
                                                                   div(textOutput("summarystats2_3"), style = 'text-align:left'),
                                                                   style = "padding:0px;")),
                                                   div(style = "text-align:center", " "),
                                                   width = 3
                                                 ),
                                                 mainPanel(tags$style(type = "text/css", "#map2 {height: calc(100vh - 80px) !important;}"),
                                                           withSpinner(leafletOutput(outputId = "map2"), type = 2),
                                                           width = 9
                                                 )
                                   )
                          ),tabPanel("Bike Usage Statistics",
                                     sidebarLayout(fluid = TRUE,
                                                   sidebarPanel(
                                                     div(style = "text-align:center",
                                                         "These plots display the average ride start time, duration, and distance for the time period selected below."),
                                                     br(),
                                                     selectInput("year3", label = "Year:",
                                                                 choices = c("All", "2018", "2019", "2020", "2021")),
                                                     selectInput("month3",label = "Month:",
                                                                 choices = c("All", "Jan", "Feb", "Mar", "Apr",
                                                                             "May", "Jun", "Jul", "Aug", "Sep",
                                                                             "Oct", "Nov", "Dec")),
                                                     selectInput("day3",label = "Weekday:",
                                                                 choices = c("All", "Mon", "Tue", "Wed",
                                                                             "Thu", "Fri", "Sat", "Sun")),
                                                     br(),
                                                     h6("Summary Statistics:", style = "text-align:center"),
                                                     fluidRow(column(7, div("Number of Trips:", style = 'text-align:right')),
                                                              column(5, div(textOutput("summarystats3_1"), style = 'text-align:left'), style = "padding:0px;")),
                                                     fluidRow(column(7, div("Number of Users:", style = 'text-align:right')),
                                                              column(5, div(textOutput("summarystats3_2"), style = 'text-align:left'), style = "padding:0px;")),
                                                     fluidRow(column(7, div("Median Trip Duration:", style = 'text-align:right')),
                                                              column(5, div(textOutput("summarystats3_3"), style = 'text-align:left'), style = "padding:0px;")),
                                                     fluidRow(column(7, div("Median Trip Distance:", style = 'text-align:right')),
                                                              column(5, div(textOutput("summarystats3_4"), style = 'text-align:left'), style = "padding:0px;")),
                                                     br(),
                                                     h6("Bike Access Method:", style = "text-align:center"),
                                                     fluidRow(column(7, div("Keypad:", style = 'text-align:right')),
                                                              column(5, div(textOutput("summarystats3_5"), style = 'text-align:left'), style = "padding:0px;")),
                                                     fluidRow(column(7, div("Keypad Phone Number", style = 'text-align:right')),
                                                              column(5, div(textOutput("summarystats3_6"), style = 'text-align:left'), style = "padding:0px;")),
                                                     fluidRow(column(7, div("Keypad RFID Card:", style = 'text-align:right')),
                                                              column(5, div(textOutput("summarystats3_7"), style = 'text-align:left'), style = "padding:0px;")),
                                                     fluidRow(column(7, div("Web:", style = 'text-align:right')),
                                                              column(5, div(textOutput("summarystats3_8"), style = 'text-align:left'), style = "padding:0px;")),
                                                     fluidRow(column(7, div("Admin:", style = 'text-align:right')),
                                                              column(5, div(textOutput("summarystats3_9"), style = 'text-align:left'), style = "padding:0px;")),
                                                     width = 3
                                                   ),
                                                   mainPanel(
                                                             fluidRow(column(width = 12, plotOutput("plot3", height = "15vw", width = "100%"), )),
                                                             fluidRow(column(width = 12, withSpinner(plotOutput("plot1", height = "15vw", width = "100%"), type = 2), )),
                                                             fluidRow(column(width = 12, plotOutput("plot2", height = "15vw", width = "100%"), )),
                                                             width = 9
                                                   )
                                     )
                          ),
                          tabPanel("One Bike's Journey",
                                   sidebarLayout(fluid = TRUE,
                                                 sidebarPanel(align = "center",
                                                   textOutput("descript1"),
                                                   br(),
                                                   textOutput("descript2"),
                                                   width = 3
                                                 ),
                                                 mainPanel(tags$style(type = "text/css", "#map4 {height: calc(100vh - 80px) !important;}"),
                                                           withSpinner(leafletOutput(outputId = "map4"), type = 2),
                                                           width = 9
                                                 )
                                   )
                          )
                          

),
hr(),
div("Created by Kivalina E. Grove, Winter 2021 Educational Data Science Specialization Capstone Project", style = "text-align:center"),
div("Data Provided by the City of Eugene and PeaceHealth Rides", style = "text-align:center"),
div("Special Thanks to Shane Rhodes & Daniel Anderson", style = "text-align:center"),
hr()
)




server <- function(input, output, session) {
  
  filteredData <- reactive({
    if (input$year == "All" & input$month == "All" & input$day == "All" ) {
      noendstation
    } else if (input$year != "All" & input$month == "All" & input$day == "All") {
      filter(noendstation, year == input$year)
    } else if (input$year == "All" & input$month != "All" & input$day == "All") {
      filter(noendstation, month == input$month)
    } else if (input$year == "All" & input$month == "All" & input$day != "All") {
      filter(noendstation, day == input$day)
    } else if (input$year != "All" & input$month != "All" & input$day == "All") {
      filter(noendstation, year == input$year & month == input$month)
    } else if (input$year != "All" & input$month == "All" & input$day != "All") {
      filter(noendstation, year == input$year & day == input$day)
    } else if (input$year == "All" & input$month != "All" & input$day != "All") {
      filter(noendstation, month == input$month & day == input$day)
    } else{
      filter(noendstation, year == input$year & month == input$month & day == input$day)
    }
  })
  
  
  filteredData2 <- reactive({
    if (input$year2 == "All" & input$month2 == "All" & input$day2 == "All" & input$triptype == "1") {
      filter(repaircoord, end_hub == input$station)
    } else if(input$year2 == "All" & input$month2 == "All" & input$day2 == "All" & input$triptype == "2"){
      filter(destroycoord, start_hub == input$station)
      
    } else if (input$year2 != "All" & input$month2 == "All" & input$day2 == "All" & input$triptype == "1") {
      filter(repaircoord, end_hub == input$station & year == input$year2)
    } else if (input$year2 != "All" & input$month2 == "All" & input$day2 == "All" & input$triptype == "2") {
      filter(destroycoord, start_hub == input$station & year == input$year2)
      
    } else if (input$year2 == "All" & input$month2 != "All" & input$day2 == "All" & input$triptype == "1") {
      filter(repaircoord, end_hub == input$station & month == input$month2)
    } else if (input$year2 == "All" & input$month2 != "All" & input$day2 == "All" & input$triptype == "2") {
      filter(destroycoord, start_hub == input$station & month == input$month2)
      
    } else if (input$year2 == "All" & input$month2 == "All" & input$day2 != "All" & input$triptype == "1") {
      filter(repaircoord, end_hub == input$station & day == input$day2)
    } else if (input$year2 == "All" & input$month2 == "All" & input$day2 != "All" & input$triptype == "2") {
      filter(destroycoord, start_hub == input$station & day == input$day2)
      
    } else if (input$year2 != "All" & input$month2 != "All" & input$day2 == "All" & input$triptype == "1") {
      filter(repaircoord, end_hub == input$station & year == input$year2 & month == input$month2)
    } else if (input$year2 != "All" & input$month2 != "All" & input$day2 == "All" & input$triptype == "2") {
      filter(destroycoord, start_hub == input$station & year == input$year2 & month == input$month2)
      
    } else if (input$year2 != "All" & input$month2 == "All" & input$day2 != "All" & input$triptype == "1") {
      filter(repaircoord, end_hub == input$station & year == input$year2 & day == input$day2)
    } else if (input$year2 != "All" & input$month2 == "All" & input$day2 != "All" & input$triptype == "2") {
      filter(destroycoord, start_hub == input$station & year == input$year2 & day == input$day2)
      
    } else if (input$year2 == "All" & input$month2 != "All" & input$day2 != "All" & input$triptype == "1") {
      filter(repaircoord, end_hub == input$station & month == input$month2 & day == input$day2)
    } else if (input$year2 == "All" & input$month2 != "All" & input$day2 != "All" & input$triptype == "2") {
      filter(destroycoord, start_hub == input$station & month == input$month2 & day == input$day2)
      
    } else if (input$year2 != "All" & input$month2 != "All" & input$day2 != "All" & input$triptype == "1") {
      filter(repaircoord, end_hub == input$station & year == input$year2 & month == input$month2 & day == input$day2)
    } else{
      filter(destroycoord, start_hub == input$station & year == input$year2 & month == input$month2 & day == input$day2)
    }
  })
  
  filteredData3 <- reactive({
    if (input$year3 == "All" & input$month3 == "All" & input$day3 == "All" ) {
      descriptive
    } else if (input$year3 != "All" & input$month3 == "All" & input$day3 == "All") {
      filter(descriptive, year == input$year3)
    } else if (input$year3 == "All" & input$month3 != "All" & input$day3 == "All") {
      filter(descriptive, month == input$month3)
    } else if (input$year3 == "All" & input$month3 == "All" & input$day3 != "All") {
      filter(descriptive, day == input$day3)
    } else if (input$year3 != "All" & input$month3 != "All" & input$day3 == "All") {
      filter(descriptive, year == input$year3 & month == input$month3)
    } else if (input$year3 != "All" & input$month3 == "All" & input$day3 != "All") {
      filter(descriptive, year == input$year3 & day == input$day3)
    } else if (input$year3 == "All" & input$month3 != "All" & input$day3 != "All") {
      filter(descriptive, month == input$month3 & day == input$day3)
    } else{
      filter(descriptive, year == input$year3 & month == input$month3 & day == input$day3)
    }
  })
  
  output$map<- renderLeaflet({
    stationdescriptions %>%
      leaflet() %>%
      addTiles() %>%
      fitBounds(~min(lon), 
                ~min(lat),
                ~max(lon), 
                ~max(lat)) %>%
      addAwesomeMarkers(lng = ~lon,
                        lat = ~lat,
                        icon = icons,
                        popup = ~paste0(start_hub, "</br>", "Number of Racks: ", as.character(racks)),
                        label = ~start_hub,
                        options = markerOptions(opacity = .95)) %>%
      addResetMapButton()
    
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      addCircleMarkers(lng = ~end_longitude,
                       lat = ~end_latitude,
                       color = "black",
                       radius = 2,
                       clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE))
  })
  
  output$summarystats1 <- renderText({
    nrow(filteredData())
  })
  
  output$map2 <- renderLeaflet({
    stationdescriptions %>%
      leaflet() %>%
      addTiles() %>%
      fitBounds(~min(lon), 
                ~min(lat),
                ~max(lon), 
                ~max(lat)) %>%
      addAwesomeMarkers(data = filter(stationdescriptions, start_hub == "1600 Millrace Drive"),
                        lng = ~lon,
                        lat = ~lat,
                        icon = s_icon,
                        popup = ~paste0(start_hub, "</br>", "Number of Racks: ", as.character(racks)),
                        label = ~start_hub,
                        options = markerOptions(opacity = .85)) %>%
      addAwesomeMarkers(data = filter(stationdescriptions, start_hub != "1600 Millrace Drive"),
                        lng = ~lon,
                        lat = ~lat,
                        icon = n_icon,
                        popup = ~paste0(start_hub, "</br>", "Number of Racks: ", as.character(racks)),
                        label = ~start_hub,
                        options = markerOptions(opacity = .85)) %>%
      addHeatmap(data = filter(destroycoord, start_hub == "1600 Millrace Drive"),
                 lng = ~longitude,
                 lat = ~latitude,
                 radius = 8) %>%
      addResetMapButton()
  })
  
  observe({
    leafletProxy("map2", data = filteredData2()) %>%
      clearHeatmap() %>%
      addHeatmap(lng = ~longitude,
                 lat = ~latitude,
                 radius = 8) %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = filter(stationdescriptions, start_hub != input$station),
                        lng = ~lon,
                        lat = ~lat,
                        icon = n_icon,
                        popup = ~paste0(start_hub, "</br>", "Number of Racks: ", as.character(racks)),
                        label = ~start_hub,
                        options = markerOptions(opacity = .85)) %>%
      addAwesomeMarkers(data = filter(stationdescriptions, start_hub == input$station),
                        lng = ~lon,
                        lat = ~lat,
                        icon = s_icon,
                        popup = ~paste0(start_hub, "</br>", "Number of Racks: ", as.character(racks)),
                        label = ~start_hub,
                        options = markerOptions(opacity = .85))
  })
  
  output$summarystats2_1 <- renderText({
    paste(nrow(filteredData2()))
  })
  
  output$summarystats2_2 <- renderText({
    paste(format(round(mean(filteredData2()$duration, na.rm = TRUE), 2), nsmall = 2), "min")
  })
  
  output$summarystats2_3 <- renderText({
    paste(format(round(mean(filteredData2()$distance_miles, na.rm = TRUE), 2), nsmall = 2), "mi")   
  })
  
  output$plot1 <- renderPlot({
        filteredData3() %>%
          ggplot(aes(x = duration)) +
          geom_histogram(binwidth = .5, color = "#303336") +
      scale_x_continuous(breaks = seq(0, 75, 5), limits = c(0, 77), expand = c(0, 0)) +
      theme_minimal() +
      labs(title = "Ride Duration",
           x = "Ride Duration (minutes)  \n       ",
           y = "Count") +
      theme(plot.background = element_rect(fill = "#ededed", color = "white"), 
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white"),
            plot.title = element_text(hjust = 0.5))
  })
  
  output$plot2 <- renderPlot({
        filteredData3() %>%
          ggplot(aes(x = distance_miles)) +
          geom_histogram(binwidth = .1, color = "#193c61", fill = "#27598e") +
      scale_x_continuous(breaks = seq(0, 10, .5), limits = c(0, 10.2), expand = c(0, 0)) +
      theme_minimal() +
      labs(title = "Ride Distance",
           x = "Ride Distance (miles)",
           y = "Count") +
      theme(plot.title = element_text(hjust = 0.5))
  })
      
  output$plot3 <- renderPlot({
        filteredData3() %>%
          ggplot(aes(x = time_start)) +
          geom_histogram(binwidth = 0.25, color = "#193c61", fill = "#27598e") +
          scale_x_continuous(breaks = seq(0, 24, 1), expand = c(0, 0), limits = c(0, 24.2)) +
          theme_minimal() +
      labs (title = "Ride Start Time",
            x = "Ride Start Hour (24-hour clock)  \n       ",
            y = "Count") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  
  output$summarystats3_1 <- renderText({
    paste(round(nrow(filteredData3()), 0), sep = "")
  })
  
  output$summarystats3_2 <- renderText({
    paste(round(length(unique(filteredData3()$user_id)), 0), sep = "")
  })
  
  output$summarystats3_3 <- renderText({
    paste(format(round(median(filteredData3()$duration, na.rm = TRUE), 2), nsmall = 2), " min", sep = "")
  })

  output$summarystats3_4 <- renderText({
    paste(format(round(median(filteredData3()$distance_miles, na.rm = TRUE), 2), nsmall = 2), " mi", sep = "")
  })
  
  output$summarystats3_5 <- renderText({
    paste(format(round((sum(filteredData3()$rental_access_path == "keypad") / length(filteredData3()$rental_access_path)) * 100, 2), nsmall = 2), "%", sep = "")
  })
  
  output$summarystats3_6 <- renderText({
    paste(format(round((sum(filteredData3()$rental_access_path == "keypad_phone_number") / length(filteredData3()$rental_access_path)) * 100, 2), nsmall = 2), "%", sep = "")
  })
  
  output$summarystats3_7 <- renderText({
    paste(format(round((sum(filteredData3()$rental_access_path == "keypad_rfid_card") / length(filteredData3()$rental_access_path)) * 100, 2), nsmall = 2), "%", sep = "")
  })
  
  output$summarystats3_8 <- renderText({
    paste(format(round((sum(filteredData3()$rental_access_path == "web") / length(filteredData3()$rental_access_path)) * 100, 2), nsmall = 2), "%", sep = "")
  })
  
  output$summarystats3_9 <- renderText({
    paste(format(round((sum(filteredData3()$rental_access_path == "admin") / length(filteredData3()$rental_access_path)) * 100, 2), nsmall = 2), "%", sep = "")
  })
  
  output$descript1 <- renderText({
    paste("This map shows the trips taken by one bike, #", bikestats[[1]], ", between ", 
          bikestats[[2]], " ", bikestats[[3]], ", ", bikestats[[4]], " and ",
          bikestats[[5]], " ", bikestats[[6]], ", ", bikestats[[7]], ". ",
          "  In total, this bike made ", bikestats[[8]], " trips with ",
          bikestats[[9]], " different users, traveling ", bikestats[[10]], 
          " miles in ", bikestats[[11]], " hours, or approximately ", bikestats [[12]], " days."
          , sep = "")
  })
  
  output$descript2 <- renderText({
    paste("In this same period, the ", bikestats[[13]], 
          " bikes in the PeaceHealth Rides System together made ",
          bikestats[[14]], " trips with ", bikestats[[15]], 
          " different users, traveling ", bikestats[[16]], " miles in ",
          bikestats[[17]], " hours, or approximately ", bikestats[[18]], 
          " years of bike travel time.", sep = "")
  })

  output$map4 <- renderLeaflet({
    stationdescriptions %>%
      leaflet() %>%
      addTiles() %>%
      fitBounds(~min(lon), 
                ~min(lat),
                ~max(lon), 
                ~max(lat)) %>%
      addAwesomeMarkers(lng = ~lon,
                        lat = ~lat,
                        icon = icons,
                        popup = ~paste0(start_hub, "</br>", "Number of Racks: ", as.character(racks)),
                        label = ~start_hub,
                        options = markerOptions(opacity = .85)) %>%
      addPolylines(data = bikelines,
                   opacity = 0.2, weight = 3, color = "black") %>%
      addResetMapButton()
      
  })
  
}

shinyApp(ui = ui, server = server)

#use renv to take a snapshot of the packages used when creating this app
# renv::init()
# renv::snapshot()