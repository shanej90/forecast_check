#The purpose of this script is to scrape the Met Office forecast for Exeter, then compare it to observed data
#This will be done by scraping the five-day forecast daily
#Then at the end of each day scraping the forecast from the observations site
#Then saving the data (basic data so will use a csv) so it can be analysed for accuracy in the future
#Will be automated using GitHib actions

#########
#PACKAGES
##########
library(dplyr)
library(janitor)
library(purrr)
library(rvest)
library(stringr)
library(tidyr)


################
#PARAMETERS
#################

datetime <- Sys.time()

forecast_url <- "https://www.metoffice.gov.uk/weather/forecast/gcj8ds2s3"

observation_url <- "https://www.metoffice.gov.uk/weather/observations/gcj8ds2s3"

##################
#EXTRACT FORECAST
##################

#webpage
forecast_webpage <- read_html(forecast_url)

#extract 'forecast day' nodes - these include the forecast data
forecast_nodes <- html_nodes(forecast_webpage, "div.forecast-day") |> html_table()

#this will give you seven days of data.
#the first day can be ignored as you want to look ahead

#write a function to do this for each element of the list
extract_forecast <- function(n) {
  
  df <- forecast_nodes[[n]]
  
  #you want to transform the data for each df so it is long, focused on temp, and includes a timestamp
  df2 <- df |>
    pivot_longer(cols = -Time, names_to = "time", values_to = "forecast") |>
    filter(str_detect(Time, "Temperature\nTemperature")) |>
    #tidying
    transmute(
      extraction_timestamp = datetime,
      date = as.Date(datetime) + n - 1,
      time,
      temp = as.numeric(str_remove(forecast, "°"))
    )
  
  return(df2)
  
}

#extract and bind data
forecast_data <- map(c(2:7), extract_forecast) |>
  bind_rows() |>
  #precision
  mutate(
    precision = case_when(
      lead(as.numeric(substr(time, 1, 2))) == as.numeric(substr(time, 1, 2)) + 1 ~ "hourly",
      time == "23:00" ~ "hourly",
      TRUE ~ "three-hourly"
    )
  )

#################################
#EXTRACT OBSERVATION DATA
#################################

#webpage
observation_webpage <- read_html(observation_url)

#extract observatiosn table
observations <- html_nodes(observation_webpage, "section.observations-table")|> html_table()
obs_df <- observations[[1]] |> clean_names()

#format is a bit weird
#first day is in the initial set of rows, second day in next set
#a fair bit of wrangling needed

#column headings (times) for second day in row where first col blank
day2_head <- filter(obs_df, x == "")
day2_head <- unlist(day2_head[1, ])
day2_head <- day2_head[str_detect(day2_head, "0") & !is.na(day2_head)]

#extract temperatures
temps <- obs_df |> filter(x == "Temperature in degrees Celsius")

#sort out day 1
day1_df <- temps |>
  slice(1:1) |>
  pivot_longer(cols = -x, names_to = "time", values_to = "observation") |>
  transmute(
    extraction_timestamp = datetime,
    date = as.Date(datetime) - 1,
    time = str_remove(time, "x") |> str_replace("_", ":"),
    temp = as.numeric(str_remove(observation, "°"))
  ) |>
  filter(!is.na(temp))

#now day 2
day2_df <- temps |>
  slice(2:2) |>
  select_if(~ all(!is.na(.))) |>
  #need to correct the headers
  setNames(c("x", day2_head)) |>
  pivot_longer(cols = -x, names_to = "time", values_to = "observation") |>
  transmute(
    extraction_timestamp = datetime,
    date = as.Date(datetime),
    time = str_remove(time, "x") |> str_replace("_", ":"),
    temp = as.numeric(str_remove(observation, "°"))
  ) |>
  filter(!is.na(temp))

c#bind
observations_hourly <- bind_rows(day1_df, day2_df) |>
  mutate(precision = "hourly")

#create a three-hourly average for longer-term observations
observations_three_hourly <- observations_hourly |>
  #change times so they match three-hourly bands
  mutate(
    time = case_when(
      as.numeric(substr(time, 1, 2)) %in% c(1, 4, 7, 10, 13, 16, 19, 22) ~ time,
      TRUE ~ NA
    )
  ) |>
  fill(time) |>
  filter(!is.na(time)) |>
  #calculate averages
  summarise(temp = mean(temp), .by = c(extraction_timestamp, date, time)) |>
  mutate(precision = "three-hourly")

#bind
observations_all <- bind_rows(observations_hourly, observations_three_hourly)

#########################
#JOIN AND EXPORT
##########################

bound <- bind_rows(
  forecast_data |> mutate(type = "forecast"), 
  observations_all |> mutate(type = "observation")
  )

#export
write.table(bound, "data/forecast_check.csv", append = T, row.names = F, sep = ",", col.names = F)
  
