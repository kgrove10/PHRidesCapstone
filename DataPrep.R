#Required Packages
library(shiny)
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
library(tidyr)
library(magrittr)

## GENERAL DATA preparation/cleaning:

# import all data stored in "Data" file ending wtih .csv
alldatalist <- import_list(dir("./data", pattern = ".csv", full.names = TRUE))

# bind and clean data together into one dataframe
alldata <- data.table::rbindlist(alldatalist) %>%
  clean_names() %>%
  filter(start_latitude != "" &
           start_longitude != "" &
           end_latitude != "" &
           end_longitude != "") %>%
  mutate(start_latitude = as.numeric(start_latitude),
         start_longitude = as.numeric(start_longitude),
         end_latitude = as.numeric(end_latitude),
         end_longitude = as.numeric(end_longitude)) %>%
  drop_na(start_latitude, start_longitude, end_latitude, end_longitude) %>%
  distinct(route_id, .keep_all = TRUE) %>% #remove any duplicated routes caused by data file overlap
  select(-starts_with("overtime"), -starts_with("out_of")) #remove columns without variance

#check there are no nas in latitude or longitude variables
#sum(is.na(alldata$end_latitude))

# import data on stations (name, # racks)
station_bikes <- import("./data/created/stationracks.csv") %>%
  clean_names() %>%
  select(c("start_hub", "racks")) %>%
  na.omit()

#create descriptions for stations, including name, locations and number of racks
stations <- alldata %>%
  filter(start_hub != "",  
         start_latitude != "-",
         start_longitude != "-") %>%
  group_by(start_hub) %>%
  mutate(lat = median(start_latitude),
         lon = median(start_longitude))

stationlist <- distinct(stations, start_hub, .keep_all = TRUE) %>%
  subset(select = c(start_hub, lat, lon))

stationdescriptions <- full_join(stationlist, station_bikes)

#save for use in shiny app
saveRDS(stationdescriptions, file = "./ShinyData/stationdescript.rds")


## VISUAL 1:

# create data for visual #1 (bikes that were left out of station)
noendstation <- alldata %>%
  filter(end_hub == "") %>%
  mutate(month = lubridate::month(start_time, label = TRUE)) %>%
  mutate(day = lubridate::wday(start_time, label = TRUE)) %>%
  mutate(year = as.numeric(lubridate::year(start_time)))

#save for use in shiny app
saveRDS(noendstation, file = "./ShinyData/noendstation.rds")


## VISUAL 2:

#to check that route_id is a distinct id variable identifying each trip (should have been removed earlier)
#sum(duplicated(alldata$route_id)) 

#prepare data for visual #2: THESE WILL TAKE A WHILE TO RUN, be prepared!
loopbike <- alldata %>%
  filter(start_hub != "" | end_hub != "") %>% #remove trips with no start or end hub
  arrange(bike_id, start_time) #sort list by time within bike ids

#initialize
goback <- NULL

#loop through data, pulling only those trips where bikes are left out of hubs, or returned to hubs
#by different people
for(i in 2:nrow(loopbike)) {
  if(loopbike$end_hub[i-1] == "" &
     loopbike$start_hub[i] == "" &
     loopbike$user_id[i] != loopbike$user_id[i-1])
  {
    tmp1 <- loopbike[i-1]
    tmp2 <- loopbike[i]
    goback <- rbind(goback, tmp1, tmp2)
  }
}

sum(duplicated(goback)) #check to make sure I didn't accidentally introduce any duplicates!

#write goback to a .rds so we don't have to run this loop every time! 
saveRDS(goback, "./data/ShinyData/outofhub.rds")

#create datasets for those trips that "destroy" the system: bikes are taken from a station and left out of station,
#and those bikes that "repair" the system: bikes are taken from out of station and returned to a station.
outofhub <- goback %>%
  separate(duration, c("min", "dis1", "sec", "dis2"), sep = " ") %>%
  select(-dis1, -dis2) %>%
  mutate(min = 60 * as.numeric(min)) %>%
  mutate(duration_sec = min + as.numeric(sec)) %>%
  mutate(duration = duration_sec / 60) #duration in minutes

destroy <- outofhub %>%
  filter(start_hub != "") %>%
  filter(is.na(end_longitude) == FALSE)

destroycoord <- destroy %>%
  mutate(duration = as.numeric(duration)) %>%
  rename(latitude = end_latitude, longitude = end_longitude) %>%
  mutate(month = lubridate::month(start_time, label = TRUE)) %>%
  mutate(day = lubridate::wday(start_time, label = TRUE)) %>%
  mutate(year = as.numeric(lubridate::year(start_time)))

saveRDS(destroycoord, file = "./ShinyData/destroycoord.rds")

repair <- outofhub %>%
  filter(start_hub == "") %>%
  mutate(duration = as.numeric(duration)) %>%
  mutate(month = lubridate::month(start_time, label = TRUE)) %>%
  mutate(day = lubridate::wday(start_time, label = TRUE)) %>%
  mutate(year = as.numeric(lubridate::year(start_time)))

repaircoord <- repair %>%
  rename(latitude = start_latitude, longitude = start_longitude)

saveRDS(repaircoord, file = "./ShinyData/repaircoord.rds")


## VISUAL 3:

#data for bike descriptive statistics
descriptivestats <- alldata %>%
  select(user_id, start_hub, start_time, end_hub, end_time, bike_id, distance_miles,
         duration, rental_access_path) %>%
  separate(duration, c("min", "dis1", "sec", "dis2"), sep = " ") %>%
  select(-dis1, -dis2) %>%
  mutate(min = 60 * as.numeric(min)) %>%
  mutate(duration_sec = min + as.numeric(sec)) %>%
  mutate(duration = duration_sec / 60) %>%
  select(-min, -sec, -duration_sec) %>%
  mutate(month = lubridate::month(start_time, label = TRUE)) %>%
  mutate(day = lubridate::wday(start_time, label = TRUE)) %>%
  mutate(year = as.numeric(lubridate::year(start_time))) %>%
  mutate(time_hour = as.numeric(lubridate::hour(start_time))) %>%
  mutate(time_minute = as.numeric(lubridate::minute(start_time))) %>%
  filter(duration <= 360 & duration >= 0) %>%
  mutate(time_parthour = time_minute / 60) %>%
  mutate(time_start = time_hour + time_parthour) %>%
  select(-time_hour, -time_minute, -time_parthour)

length(unique(descriptivestats$user_id))

saveRDS(descriptivestats, file = "./ShinyData/fordescriptives.rds")


##VISUAL 4:

#create data for visual #4, showing travels taken by one bike
biketrav <- alldata %>%
  select(start_latitude, start_longitude, start_time, 
         end_latitude, end_longitude, #end_time, 
         bike_id,
         distance_miles, duration) %>%
  separate(duration, c("min", "dis1", "sec", "dis2"), sep = " ") %>%
  select(-dis1, -dis2) %>%
  mutate(min = 60 * as.numeric(min)) %>% #convert minutes to seconds
  mutate(duration_sec = min + as.numeric(sec)) %>% #add minutes in seconds and seconds columns
  mutate(duration = duration_sec / 60) %>% #convert total time in seconds back to minutes
  select(-min, -sec, -duration_sec) %>% #remove other created columns
  mutate(year = as.numeric(lubridate::year(start_time))) %>%
  arrange(bike_id, start_time)

# count number of trips by bike - to select bike with greatest number of trips
bike_num <- biketrav %>%
  group_by(bike_id) %>%
  count() %>%
  arrange(desc(n)) %>%
  pull(bike_id) %>%
  head(1)
  

#select bike with greatest number of trips for visual #4
biketrips <- biketrav %>%
  filter(bike_id == bike_num) %>%
  mutate(id = seq.int(nrow(.))) %>%
  gather(key = group, value = location, -id, -bike_id, -duration, -distance_miles, -year, -start_time) %>%
  separate(group, c("group", "where"), sep = "_") %>%
  spread(where, location) %>%
  arrange(start_time)


#function to add date suffix for description (adapted from [shayaa on stack overflow]{https://stackoverflow.com/questions/40039903/r-add-th-rd-and-nd-to-dates})
append_date_suffix <- function(dates){
  suff <- case_when(dates %in% c(11,12,13) ~ "th",
                    dates %% 10 == 1 ~ 'st',
                    dates %% 10 == 2 ~ 'nd',
                    dates %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(dates, suff)
}

#create statistics used in description for visual #4
b_num <- bike_num
start_month <- lubridate::month(head(biketrav$start_time, 1), label = TRUE, abbr = FALSE)
start_day <- append_date_suffix(lubridate::day(head(biketrav$start_time, 1)))
start_year <- lubridate::year(head(biketrav$start_time, 1))
end_month <- lubridate::month(tail(biketrav$start_time, 1), label = TRUE, abbr = FALSE)
end_day <- append_date_suffix(lubridate::day(tail(biketrav$start_time, 1)))
end_year <- lubridate::year(tail(biketrav$start_time, 1))
b_num_trp <- nrow(filter(biketrav, bike_id == bike_num))
b_num_usr <- nrow(distinct(filter(alldata, bike_id == bike_num), user_id))
b_num_mi <- round(sum(filter(biketrav, bike_id == bike_num)$distance_miles), 2)
b_time_h <- round(sum(filter(biketrav, bike_id == bike_num)$duration / 60), 2)
b_time_d <- round(b_time_h / 24, 2)
t_bikes <- nrow(distinct(alldata, bike_id))
t_num_trp <- nrow(biketrav)
t_num_usr <- nrow(distinct(alldata, user_id))
t_num_mi <- round(sum(biketrav$distance_miles), 2)
t_time_h <- round(sum(biketrav$duration / 60), 2)
t_time_y <- round(t_time_h / 24 / 365, 2)

bikestats <- list(b_num, start_month, start_day, start_year, end_month, end_day, end_year,
                  b_num_trp, b_num_usr, b_num_mi, b_time_h, b_time_d, t_bikes, t_num_trp, t_num_usr,
                  t_num_mi, t_time_h, t_time_y)

saveRDS(bikestats, file = "./ShinyData/bikestats.rds")


#function to create map for visual 4 by creating a SpatialLines object
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
    coordinates(data) <- c(long, lat)
    if (!is.null(sort_field)) {
      if (!is.null(id_field)) {
        data <- data[order(data[[id_field]], data[[sort_field]]), ]
      } else {
        data <- data[order(data[[sort_field]]), ]
      }
    }
    if (is.null(id_field)) {
      lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
      return(lines)
    } else if (!is.null(id_field)) {
      paths <- sp::split(data, data[[id_field]])
      sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
      for (p in 2:length(paths)) {
        id <- paste0("line", as.character(p))
        l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
        sp_lines <- spRbind(sp_lines, l)
      }
      return(sp_lines)
    }
  }
  
# create object
bikelines <- points_to_line(biketrips, "longitude", "latitude", "id")

# save for use in shinydashboard.R
saveRDS(bikelines, file = "./ShinyData/bikelines.rds")



#to do: install and use package "Renv" to take a snapshot of packageg used! (see notes at top of class in notebook)
# see: https://kevinushey-2020-rstudio-conf.netlify.app/slides.html#1