library(googledrive)
library(googlesheets4)
library(tidyverse)
library(reactable)

# get activities table from Google Drive ------------------------------------------------------

# https://docs.google.com/spreadsheets/d/1q_upEllo0YniR2G_jEddRwPjP7WtgYBGqfOmSTAcC9g/edit#gid=1929631141

# run gs4 in a deathorized state for non-interactive pull
gs4_deauth()

id <- '1q_upEllo0YniR2G_jEddRwPjP7WtgYBGqfOmSTAcC9g'
sht <- read_sheet(id, sheet = 'All Activities')

activities <- sht |> 
  rename(
    Description = `...3`
  ) |> 
  fill(Action, .direction = "down") |> 
  select(-`Action Plan`, -Description) |> 
  separate(Activity, into = c('Activity', 'Description'), sep = '\\s', extra = 'merge') |> 
  split(.$Action) |> 
  map(~ .x |> select(-Action))

save(activities, file = 'data/activities.RData')

# tn loading data -----------------------------------------------------------------------------

download.file(url = 'https://github.com/tbep-tech/load-estimates/raw/main/data/tnanndat.RData', 
              destfile = 'data/tnanndat.RData')
