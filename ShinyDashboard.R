#library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(tidyverse)
library(rio)
library(janitor)
library(shinythemes)
library(shinyjs) 
library(bslib)

alldatalist <- import_list(dir("./data", pattern = ".csv", full.names = TRUE))

# testdata <- import("trips_peace_health_rides_11_01_2020-01_07_2021.csv") %>%
#     clean_names()

station_bikes <- import("./data/created/stationracks.csv") %>%
  clean_names() %>%
  select(c("start_hub", "racks")) %>%
  na.omit()

alldata <- data.table::rbindlist(alldatalist) %>%
  clean_names() %>%
  mutate(start_latitude = as.numeric(start_latitude),
         start_longitude = as.numeric(start_longitude),
         end_latitude = as.numeric(end_latitude),
         end_longitude = as.numeric(end_longitude))

#list of bikes for reference
#bikes_unique <- alldata %>%
  #distinct(bike_id)

#bike_list <- dplyr::pull(bikes_unique, bike_id) 

#sum(duplicated(alldata$route_id)) # check that route_id is a distinct id variable identifying each trip

#prepare data for visual #2: 
# loopbike <- alldata %>%
#   filter(start_hub != "" | end_hub != "") %>% #remove trips with no start or end hub
#   arrange(bike_id, start_time) #sort list by time within bike ids


# goback <- NULL

#loop through data, pulling only those trips where bikes are left out of hubs, or returned to hubs
#by different people
# for(i in 2:nrow(loopbike)) {
#   if(loopbike$end_hub[i-1] == "" & 
#      loopbike$start_hub[i] == "" &
#      loopbike$user_id[i] != loopbike$user_id[i-1])
#   {
#     tmp1 <- loopbike[i-1]
#     tmp2 <- loopbike[i]
#     goback <- rbind(goback, tmp1, tmp2)
#   } 
# }

#sum(duplicated(goback)) #check to make sure I didn't accidentally introduce any duplicates!

#write goback to a .csv so we don't have to run this loop every time! 
#write.csv(goback, "./data/created/outofhub.csv")

outofhub <- import("./data/created/outofhub.csv") %>%
  select(-V1)
  

# inefficient double loop - implemented better single loop above
# for(k in 1:length(bike_list)) {
#   for(i in 2:nrow(hubtrip)) {
#     number <- bike_list[k]
#     onebike <- filter(hubtrip, bike_id == number)
#     arranged <- arrange(onebike, start_time)
#     if(arranged$end_hub[i-1] == "" &
#        is.na(arranged$end_hub[i-1]) != TRUE &
#        arranged$start_hub[i] == "" &
#        is.na(arranged$start_hub[i]) != TRUE &
#        arranged$user_id[i] != arranged$user_id[i-1])
#     {
#       tmp1 <- arranged[i-1]
#       tmp2 <- arranged[i]
#       goback1 <- rbind(goback3, tmp1, tmp2)
#     }
#   }
# }

stations <- alldata %>%
  filter(start_hub != "",  
         start_latitude != "-",
         start_longitude != "-") %>%
  group_by(start_hub) %>%
  mutate(lat = median(start_latitude),
         lon = median(start_longitude))
# lonmax = max(start_longitude),
# lonmin = min(start_longitude))

stationlist <- distinct(stations, start_hub, .keep_all = TRUE) %>%
  subset(select = c(start_hub, lat, lon))

stationdescriptions <- full_join(stationlist, station_bikes)


noendstation <- alldata %>%
  filter(end_hub == "") %>%
  mutate(month = lubridate::month(start_time, label = TRUE)) %>%
  mutate(day = lubridate::wday(start_time, label = TRUE)) %>%
  mutate(year = as.numeric(lubridate::year(start_time)))


icons <- awesomeIcons(
  icon = 'fa-bicycle',
  iconColor = '#FFFFFF',
  library = 'fa',
)

#code to customize theme
# material <- bs_theme(
#   bg = "#202123",
#   fg = "#B8BCC2",
#   primary = "#EA80FC",
#   secondary = "#00DAC6",
#   success = "#4F9B29",
#   info = "#28B3ED",
#   warning = "#FD7424",
#   danger = "#F7367E",
#   base_font = font_google("Open Sans"),
#   heading_font = font_google("Proza Libre"),
#   code_font = font_google("Fira Code")
# )
# bs_theme_preview(material, with_themer = TRUE)

ui<- fluidPage(navbarPage(title = "PeaceHealth Rides Visualizations", 
                          theme = bs_theme(bg = "#E9F4FF", fg = "#000000", primary = "#27598e", 
                                           secondary = "#99b8d4", base_font = font_google("Signika"), 
                                           `enable-gradients` = FALSE, `enable-shadows` = FALSE, bootswatch = "slate"),
                tabPanel("Out-of-Station Bikes",
                         sidebarLayout(fluid = TRUE,
                           sidebarPanel(
                             selectInput("year", label = "Year",
                                         choices = c("All", "2018", "2019", "2020", "2021")),
                             selectInput("month",label = "Month",
                                         choices = c("All", "Jan", "Feb", "Mar", "Apr",
                                                     "May", "Jun", "Jul", "Aug", "Sep",
                                                     "Oct", "Nov", "Dec")),
                             selectInput("day",label = "Day of Week",
                                         choices = c("All", "Mon", "Tue", "Wed",
                                                     "Thu", "Fri", "Sat", "Sun")),
                             div(style = "text-align:center", "This application built with the support of the City of Eugene and PeaceHealth Rides"),
                             width = 2
                           ),
                         mainPanel(tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                   leafletOutput(outputId = "map"),
                                   width = 10
                         )
                         )
                ),
                tabPanel("Bike Bounty Hunting"
                         )
                )
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
                        options = markerOptions(opacity = .85))
    # output$map<- renderLeaflet({
    # noendstation %>%
    #     leaflet() %>%
    #     addTiles() %>%
    #     fitBounds(~min(end_longitude), 
    #               ~min(end_latitude), 
    #               ~max(end_longitude), 
    #               ~max(end_latitude))
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
}

shinyApp(ui = ui, server = server)

#previous app code

# bikeIcon <- makeIcon(
#   iconUrl = "https://image.flaticon.com/icons/png/512/130/130066.png",
#   iconWidth = 25, iconHeight = 25,
#   iconAnchorX = 10, iconAnchorY = 10
# )


# ui<- dashboardPage(
#   skin = "blue",
#   dashboardHeader(title = "PeaceHealth Rides Visualizations"),
#   dashboardSidebar(
#     selectInput("year", label = "Year",
#                 choices = c("All", "2018", "2019", "2020", "2021")),
#     selectInput("month",label = "Month",
#                 choices = c("All", "Jan", "Feb", "Mar", "Apr",
#                             "May", "Jun", "Jul", "Aug", "Sep",
#                             "Oct", "Nov", "Dec")),
#     selectInput("day",label = "Day of Week",
#                 choices = c("All", "Mon", "Tue", "Wed",
#                             "Thu", "Fri", "Sat", "Sun"))
#   ),
#   dashboardBody(
#     tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
#     leafletOutput(outputId = "map")
#   ))
# 
# server <- function(input, output, session) {
#   
#   filteredData <- reactive({
#     if (input$year == "All" & input$month == "All" & input$day == "All" ) {
#       noendstation
#       } else if (input$year != "All" & input$month == "All" & input$day == "All") {
#         filter(noendstation, year == input$year)
#       } else if (input$year == "All" & input$month != "All" & input$day == "All") {
#         filter(noendstation, month == input$month)
#       } else if (input$year == "All" & input$month == "All" & input$day != "All") {
#         filter(noendstation, day == input$day)
#       } else if (input$year != "All" & input$month != "All" & input$day == "All") {
#         filter(noendstation, year == input$year & month == input$month)
#       } else if (input$year != "All" & input$month == "All" & input$day != "All") {
#         filter(noendstation, year == input$year & day == input$day)
#       } else if (input$year == "All" & input$month != "All" & input$day != "All") {
#         filter(noendstation, month == input$month & day == input$day)
#       } else{
#         filter(noendstation, year == input$year & month == input$month & day == input$day)
#       }
#   })
#   
#   output$map<- renderLeaflet({
#     stationdescriptions %>%
#       leaflet() %>%
#       addTiles() %>%
#       fitBounds(~min(lon), 
#                 ~min(lat),
#                 ~max(lon), 
#                 ~max(lat)) %>%
#       addAwesomeMarkers(lng = ~lon,
#                         lat = ~lat,
#                         icon = icons,
#                         popup = ~paste0(start_hub, "</br>", "Number of Racks: ", as.character(racks)),
#                         label = ~start_hub,
#                         options = markerOptions(opacity = .85))
#     # output$map<- renderLeaflet({
#     # noendstation %>%
#     #     leaflet() %>%
#     #     addTiles() %>%
#     #     fitBounds(~min(end_longitude), 
#     #               ~min(end_latitude), 
#     #               ~max(end_longitude), 
#     #               ~max(end_latitude))
#   })
#   
#   observe({
#     leafletProxy("map", data = filteredData()) %>%
#       clearMarkerClusters() %>%
#       addCircleMarkers(lng = ~end_longitude,
#                        lat = ~end_latitude,
#                        color = "black",
#                        radius = 2,
#                        clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE))
#   })
# }
# 
# shinyApp(ui = ui, server = server)


#to do: install and use package "Renv" to take a snapshot of packageg used! (see notes at top of class in notebook)
# see: https://kevinushey-2020-rstudio-conf.netlify.app/slides.html#1