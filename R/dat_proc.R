library(googledrive)
library(googlesheets4)
library(tidyverse)
library(reactable)
library(haven)
library(here)
library(tbeptools)

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
  separate(Activity, into = c('Activity', 'Description'), sep = '\\s', extra = 'merge') 
activities <- split(activities, activities$Action) |> 
  map(~ .x |> select(-Action))

save(activities, file = here('data/activities.RData'))

# tn loading data -----------------------------------------------------------------------------

download.file(url = 'https://github.com/tbep-tech/load-estimates/raw/main/data/tnanndat.RData', 
              destfile = here('data/tnanndat.RData'))


# seagrass data intersected with bay segments -------------------------------------------------

maxyr <- 2022

segs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Manatee River', 'Terra Ceia Bay')

data('sgseg', package = 'tbeptools')

intseg <- sgseg |> 
  dplyr::filter(segment %in% segs) |>
  sf::st_make_valid()

sgurl <- paste0('https://github.com/tbep-tech/hmpu-workflow/raw/master/data/sgdat', maxyr, '.RData')
sgdatraw <- rdataload(sgurl)

sgdat <- sgdatraw |> 
  dplyr::filter(FLUCCSCODE %in% c(6540, 9113, 9116)) |> 
  dplyr::mutate(
    hab = dplyr::case_when(
      FLUCCSCODE == 6540 ~ 'Oyster',
      FLUCCSCODE == 9113 ~ 'Patchy seagrass',
      FLUCCSCODE == 9116 ~ 'Continuous seagrass'
    )
  ) |>
  sf::st_transform(sf::st_crs(intseg)) |> 
  sf::st_intersection(intseg)

sgdat <- sgdat |>
  dplyr::mutate(
    acres = sf::st_area(x = sgdat),
    acres = units::set_units(acres, acres)
  ) |> 
  sf::st_set_geometry(NULL) |> 
  dplyr::summarise(
    acres = sum(acres), 
    .by = c(segment, hab)
  )

save(sgdat, file = here::here('data/sgdat.RData'))

# nekton data ---------------------------------------------------------------------------------

data('fimdata', package = 'tbeptools')

tbniscr <- anlz_tbniscr(fimdata)

save(tbniscr, file = here::here('data/tbniscr.RData'))

# # FIM data from FTP ---------------------------------------------------------------------------
# 
# ftp_url <- "ftp://ftp.floridamarine.org/users/fim/DataMgt/Inshore_SAS_Data/FIM_inshore_SAS_database_library_20240222.zip"
# 
# temp_dir <- tempdir()
# 
# # Download the zip file from FTP to temporary directory
# temp_file <- paste0(temp_dir, "/", basename(ftp_url))
# download.file(url = ftp_url, destfile = temp_file, mode = "wb", quiet = FALSE)
# 
# # Unzip the file
# unzip(temp_file, exdir = temp_dir)
# 
# # extracted file path
# file_path <- paste0(temp_dir, "/", gsub('\\.zip$', '/', basename(ftp_url)))
# 
# # import original SAS data
# phyraw <- read_sas(paste0(file_path, 'tbm_physical.sas7bdat'))
# bioraw <- read_sas(paste0(file_path, 'tbm_biology_number.sas7bdat'))
# sppraw <- read_sas(paste0(file_path, 'species_codes.sas7bdat'))
# 
# unlink(temp_dir, recursive = TRUE)
# 
# # physical (site) data
# # filter zones A-E for TB proper
# # filter gear type 20 (21.3-m seine)
# # filter by stratum (if wanted) to return shoreline ('S') and/or offshore (>5 m from shore, 'A','B'), we can include both, but will need to add to methods section
# # filter reference with loc info
# # filter by project "AM" to return standard monitoring sites, others are special projects - may not change the output much
# phydat <- phyraw |> 
#   mutate(
#     date = ymd(date)
#   ) |> 
#   filter(Zone %in% c('A', 'B', 'C', 'D', 'E')) |> 
#   #filter(Project =='AM') 
#   filter(Gear == 20) |> 
#   #filter (Stratum %in% c('A','B')) |> 
#   select(Reference, date)
# 
# # species codes
# sppdat <- sppraw |> 
#   select(NODCCODE, Commonname) 
# 
# # species count data
# biodat <- bioraw |> 
#   select(Reference, Species_record_id, NODCCODE, Number) |> 
#   left_join(sppdat, by = 'NODCCODE')
# 
# # filter by species of interest
# sppdat <- biodat |> 
#   filter(Commonname %in% c('Pink Shrimp', 'Red Drum', 'Spotted Seatrout')) |> 
#   select(Reference, Number, Commonname)
# 
# # join all an create complete
# fimdat <- left_join(phydat, sppdat, by = 'Reference', relationship = 'one-to-many') |> 
#   complete(Commonname, nesting(Reference, date), fill = list(Number = 0)) |> 
#   na.omit()
# 
# save(fimdat, file = here('data/fimdat.RData'))

# epc ph data ---------------------------------------------------------------------------------

fl <- 'epcdatall.xlsx'
phdatraw <- read_importwq(fl, download_latest = T, all = T)

phdat <- phdatraw |> 
  select(epchc_station, SampleTime, yr, ph = `pH-M`) |> 
  summarise(
    ph = mean(ph, na.rm = T),
    .by = c(epchc_station, yr)
  ) |> 
  summarise(
    ph = mean(ph, na.rm = T),
    .by = c(yr)
  ) |> 
  arrange(yr)

save(phdat, file = here('data/phdat.RData'))

file.remove(fl)