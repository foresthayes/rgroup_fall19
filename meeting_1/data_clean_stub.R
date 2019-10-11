# Sandbox for R group data cleaning/manip
# Josh Nowak

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
mrph_dt <- function(x, ...){
  x %>%
    mutate(
      date_time = as.POSIXct(
        strptime(date_time, "%m/%d/%Y %H:%M", ...)
      )
    )
}

mrph_lat <- function(x, minc = 20, maxc = 60) {
  x %>%
    mutate(
      lat = lat * (lat > minc),
      lat = lat * (lat < maxc),
      lat = replace(lat, lat == 0, NA_real_)
    )
}

mrph_lon <- function(x, minc = -115, maxc = -100) {
  x %>%
    mutate(
      lon = lon * (lon > minc),
      lon = lon * (lon < maxc),
      lon = replace(lon, lon == 0, NA_real_)
    )
}

mrph_elev <- function(x) {
  x %>%
    mutate(
      elev = abs(elev)
    )
}


################################################################################
# Do work
tmp <- dat %>%
  mrph_dt() %>%
  mrph_elev() %>%
  mrph_lat() %>%
  mrph_lon()

err <- dat_e %>%
  mrph_dt() %>%
  mrph_elev() %>%
  mrph_lat() %>%
  mrph_lon()
