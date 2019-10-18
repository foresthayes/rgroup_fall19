# Sandbox for R group data cleaning/manip
# Forest Hayes

################################################################################
# Packages
library(dplyr)

################################################################################
# Load data
dat <- readr::read_csv(
  "./data/collar_data.csv",
  col_types = "lccdddddddcccd"
)

dat_e <- readr::read_csv(
  "./data/collar_data_errors.csv",
  col_types = "lccdddddddcccd"
)

################################################################################
# Define functions
mrph_dt <- function(x, format = "%m/%d/%Y %H:%M", ...){
  as.POSIXct(strptime(x, format, ...))
}

mrph_lat <- function(lat, minc = 20, maxc = 60) {
      (lat * (lat > minc) * (lat < maxc)) %>%
      replace(lat == 0, NA_real_)
}

mrph_lon <- function(lon, minc = -115, maxc = -100) {
  (lon * (lon > minc) * (lon < maxc)) %>%
  replace(lon == 0, NA_real_)
}

mrph_bioyr <- function(x, yr_start = "October", ...){
  dts <- as.Date(x, ...)
  offset <- (12 - match(yr_start, month.name))
  dts - months(offset)
}

################################################################################
# Do work
dat %>%
  transmute(
    id          = ID,
    date_time   = mrph_dt(date_time),
    bio_yr      = mrph_bioyr(date_time),
    lat         = mrph_lat(lat),
    lon         = mrph_lon(lon),
    elev        = abs(elev),
    temperature = temperature
  )
