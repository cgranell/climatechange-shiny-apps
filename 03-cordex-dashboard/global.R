library(readr)
library(dplyr)
library(lubridate)

# Define the list of options

indices <- structure(c("hwmid", "tn10p"), 
                     .Names = c("Heat Wave Magnitude Index-daily (HWMId)", "Cold nights (TN10P)"))


url_decade <- "https://raw.githubusercontent.com/cgranell/climatechange-shiny-apps/master/01-cordex-decades/data/dec_mean_allindices_allcities_ICHEC_EC_EARTH_rcp85_r1i1p1_KNMI_RACMO22E.csv"

tb_decade <- read_csv(url_decade)

years <- unique(tb_decade$year)
decades <- unique(tb_decade$decade)

