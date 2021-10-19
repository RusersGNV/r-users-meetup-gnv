library(tidyverse)
library(rvest)

sched_url <- "https://floridagators.com/sports/football/schedule/2021"

browseURL(sched_url)

raw_html <- 
  sched_url |> 
  read_html()

sched_rows <- 
  raw_html |> 
  html_nodes(".sidearm-schedule-game-row")

# Dates ----

dates_raw <- 
  sched_rows |> 
  map(\(x) x |> 
        html_nodes(".sidearm-schedule-game-opponent-date") |> 
        html_text()) 
  
dates <- 
  dates_raw |> 
  map_chr(\(x) gsub("Gators Sports Network|TBA|\\r\\n", "", x) |> 
        trimws())
      
dates <- 
  dates |> 
  map_chr(\(x) substring(x, 1, 6) |> 
        trimws())

# Home Game ? ----

home_raw <- 
  sched_rows |> 
  map(\(x) x |> 
        html_nodes(".sidearm-schedule-game-location") |> 
        html_text()) 

home <- 
  home_raw |> 
  map(\(x) gsub("\\r\\n", "", x) |> 
        trimws())

home <- 
  home |> 
  map_lgl(\(x) grepl("Ben Hill Griffin Stadium", x) |> 
        any())

# map2() to create data.frame ----

map2_dfr(dates,
     home,
     \(date, h) tibble(date = date,
                       home = h))


# keep() to show only home game days ----

keep(dates, home)

