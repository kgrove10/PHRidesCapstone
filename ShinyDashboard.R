#library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(tidyverse)
library(rio)
library(janitor)

alldatalist <- import_list(dir("./data", pattern = ".csv", full.names = TRUE))

station_bikes <- import("stationracks.csv") %>%
  clean_names() %>%
  select(c("start_hub", "racks")) %>%
  na.omit()

alldata <- data.table::rbindlist(alldatalist) %>%
  clean_names() %>%
  mutate(start_latitude = as.numeric(start_latitude),
         start_longitude = as.numeric(start_longitude),
         end_latitude = as.numeric(end_latitude),
         end_longitude = as.numeric(end_longitude))

# testdata <- import("trips_peace_health_rides_11_01_2020-01_07_2021.csv") %>%
#     clean_names()

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
  mutate(month = lubridate::month(start_time, label = TRUE))

icons <- awesomeIcons(
  icon = 'fa-bicycle',
  iconColor = 'black',
  library = 'fa',
)


ui<- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "PeaceHealth Rides Visualizations"),
  dashboardSidebar(
    selectInput("month",label = "Month",
                choices = unique(noendstation$month))
  ),
  dashboardBody(
    fluidRow(box(width = 12,leafletOutput(outputId = "map")))
  ))

server <- function(input, output, session) {
  
  filteredData <- reactive({
    filter(noendstation, month == input$month)
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
                        label = ~start_hub)
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
      addCircleMarkers(lng = ~end_longitude,
                       lat = ~end_latitude,
                       color = "black",
                       radius = 2,
                       clusterOptions = markerClusterOptions())
  })
}

shinyApp(ui = ui, server = server)


#Still to do:
# add additional filters (e.g. year, season, day of week?)
# make station markers less in the way, or add some way to toggle them on / off? see reference here: https://rstudio.github.io/leaflet/markers.html
# figure out how to host in a way that keeps data private! 