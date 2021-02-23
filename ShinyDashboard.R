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
library(sp)
library(maptools)
library(shinyWidgets)
library(leaflet.extras)


alldatalist <- import_list(dir("./data", pattern = ".csv", full.names = TRUE))

# testdata <- import("trips_peace_health_rides_11_01_2020-01_07_2021.csv") %>%
#     clean_names()

station_bikes <- import("./data/created/stationracks.csv") %>%
  clean_names() %>%
  select(c("start_hub", "racks")) %>%
  na.omit()

alldata <- data.table::rbindlist(alldatalist) %>%
  clean_names() %>%
  filter(start_latitude != "" &
           start_longitude != "" &
           end_latitude != "" &
           end_longitude != "") %>%
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

destroy <- outofhub %>%
  filter(start_hub != "") %>%
  filter(is.na(end_longitude) == FALSE)

destroycoord <- destroy %>%
  rename(latitude = end_latitude, longitude = end_longitude)

repair <- outofhub %>%
  filter(start_hub == "")

repaircoord <- repair %>%
  rename(latitude = start_latitude, longitude = start_longitude)

# 
# points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
#   coordinates(data) <- c(long, lat)
#   if (!is.null(sort_field)) {
#     if (!is.null(id_field)) {
#       data <- data[order(data[[id_field]], data[[sort_field]]), ]
#     } else {
#       data <- data[order(data[[sort_field]]), ]
#     }
#   }
#   if (is.null(id_field)) {
#     lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
#     return(lines)
#   } else if (!is.null(id_field)) {  
#     paths <- sp::split(data, data[[id_field]])
#     sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
#     for (p in 2:length(paths)) {
#       id <- paste0("line", as.character(p))
#       l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
#       sp_lines <- spRbind(sp_lines, l)
#     }
#     return(sp_lines)
#   }
# }

# repairlinestestlong <- repair %>%   
#   filter(start_latitude != "" &
#            start_longitude != "" & 
#            end_latitude != "" &
#            end_longitude != "") %>%
#   mutate(month = lubridate::month(start_time, label = TRUE)) %>%
#   filter(month == "Mar" &
#            distance_miles > 1) %>%
#   select(user_id, route_id, bike_id, 
#          start_hub, start_latitude, start_longitude, start_time, 
#          end_hub, end_latitude, end_longitude, end_time) %>%
#   gather(key = group, value = location, -ends_with("id"), 
#          -ends_with("time"), -ends_with("hub")) %>%
#   separate(group, c("group","where")) %>%
#   spread(where, location)
# 
# repairlinestestmed <- repair %>%   
#   filter(start_latitude != "" &
#            start_longitude != "" & 
#            end_latitude != "" &
#            end_longitude != "") %>%
#   mutate(month = lubridate::month(start_time, label = TRUE)) %>%
#   filter(month == "Mar" &
#            distance_miles <= 1 &
#            distance_miles >= .5) %>%
#   select(user_id, route_id, bike_id, 
#          start_hub, start_latitude, start_longitude, start_time, 
#          end_hub, end_latitude, end_longitude, end_time) %>%
#   gather(key = group, value = location, -ends_with("id"), 
#          -ends_with("time"), -ends_with("hub")) %>%
#   separate(group, c("group","where")) %>%
#   spread(where, location)
# 
# repairlinestestshort <- repair %>%   
#   filter(start_latitude != "" &
#            start_longitude != "" & 
#            end_latitude != "" &
#            end_longitude != "") %>%
#   mutate(month = lubridate::month(start_time, label = TRUE)) %>%
#   filter(month == "Mar" &
#            distance_miles < .5) %>%
#   select(user_id, route_id, bike_id, 
#          start_hub, start_latitude, start_longitude, start_time, 
#          end_hub, end_latitude, end_longitude, end_time) %>%
#   gather(key = group, value = location, -ends_with("id"), 
#          -ends_with("time"), -ends_with("hub")) %>%
#   separate(group, c("group","where")) %>%
#   spread(where, location)


# rl1 <- points_to_line(repairlinestestlong, "longitude", "latitude", "route_id")
# rl2 <- points_to_line(repairlinestestmed, "longitude", "latitude", "route_id")
# rl3 <- points_to_line(repairlinestestshort, "longitude", "latitude", "route_id")


# destroylines <- destroy %>%
#   select(user_id, route_id, bike_id, 
#          start_hub, start_latitude, start_longitude, start_time, 
#          end_hub, end_latitude, end_longitude, end_time) %>%
#   gather(key = group, value = location, -ends_with("id"), 
#          -ends_with("time"), -ends_with("hub")) %>%
#   separate(group, c("group","where")) %>%
#   spread(where, location)
# 
# dl <- points_to_line(destroylines, "longitude", "latitude", "route_id")



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

bikeIcon <- makeIcon(
  iconUrl = "https://image.flaticon.com/icons/png/512/130/130066.png",
  iconWidth = 25, iconHeight = 25,
  iconAnchorX = 10, iconAnchorY = 10
)

# code to customize theme
# material <- bs_theme(
#   bg = "#E9F4FF",
#   fg = "#000000",
#   primary = "#27598e",
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
                # tabPanel("Bike Bounty Hunting",
                #          sidebarLayout(fluid = TRUE,
                #                        sidebarPanel(
                #                          selectInput("triptype", label = "Trip Type",
                #                                      choices = c("Short", "Medium", "Long")),
                #                          div(style = "text-align:center", "Short < 0.5 mile, Long > 1 mile"),
                #                          width = 2
                #                        ),
                #                        mainPanel(tags$style(type = "text/css", "#vis2 {height: calc(100vh - 80px) !important;}"),
                #                                  leafletOutput(outputId = "vis2"),
                #                                  width = 10)
                #          )
                # ),
                tabPanel("Distributed System Repair",
                         sidebarLayout(fluid = TRUE,
                                       sidebarPanel(
                                         selectInput("station", label = "Station",
                                                     choices = str_sort(stationlist$start_hub)),
                                         prettyRadioButtons("triptype", 
                                                            label = "Type of Trip",
                                                            choices = list("Return to Station" = 1, 
                                                                           "Leave from Station" = 2), 
                                                            selected = 1,
                                                            status = "primary",
                                                            shape = "round",
                                                            fill = TRUE,
                                                            animation = "jelly",
                                                            plain = FALSE,
                                                            thick = FALSE,
                                                            bigger = TRUE
                                                            ),
                                         
                                         div(style = "text-align:center", " "),
                                         width = 3
                                       ),
                                       mainPanel(tags$style(type = "text/css", "#map2 {height: calc(100vh - 80px) !important;}"),
                                                 leafletOutput(outputId = "map2"),
                                                 width = 9
                                       )
                         )
                )
  ))




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
  
  # filteredlinedata <- reactive({
  #   if (input$triptype == "Short") {
  #     rl3
  #   } else if (input$triptype == "Medium") {
  #     rl2
  #   } else{
  #     rl1
  #   }
  # })
  
  filteredData2 <- reactive({
    if (input$triptype == "1") {
      filter(repaircoord, end_hub == input$station)
    } else if (input$triptype == "2") {
      filter(destroycoord, start_hub == input$station)
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
      addHeatmap(data = filter(repaircoord, end_hub == "1600 Millrace Drive"),
                 lng = ~longitude,
                 lat = ~latitude,
                 radius = 8)
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
    
# output$vis2 <- renderLeaflet({
#   stationdescriptions %>%
#     leaflet() %>%
#     addTiles() %>%
#     fitBounds(~min(lon), 
#               ~min(lat),
#               ~max(lon), 
#               ~max(lat)) %>%
#     addAwesomeMarkers(lng = ~lon,
#                       lat = ~lat,
#                       icon = icons,
#                       popup = ~paste0(start_hub, "</br>", "Number of Racks: ", as.character(racks)),
#                       label = ~start_hub,
#                       options = markerOptions(opacity = .85)) %>%
#     #addPolylines(data = repairlinestestlong, lng = ~longitude, lat = ~latitude, group = ~group)
#     addPolylines(data = filteredlinedata(), opacity = 0.3, weight = 3, color = "black")
  # output$map<- renderLeaflet({
  # noendstation %>%
  #     leaflet() %>%
  #     addTiles() %>%
  #     fitBounds(~min(end_longitude), 
  #               ~min(end_latitude), 
  #               ~max(end_longitude), 
  #               ~max(end_latitude))
# })
# 
#  
# observeEvent(input$vis2_shape_click, {
#   p <- input$vis2_shape_click
#   print(p) 
# })
# 
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