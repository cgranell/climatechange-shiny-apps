# library(readr)
# library(dplyr)
library(tidyverse)
library(lubridate)
library(sparkline)

# Define the list of options
# indices <- structure(c("hwmid", "tn10p"), 
#                      .Names = c("Heat Wave Magnitude Index-daily (HWMId)", "Cold nights (TN10P)"))

url_decade <- "https://raw.githubusercontent.com/cgranell/climatechange-viz/master/output/dec_mean_allindices_allcities_ICHEC_EC_EARTH_rcp85_r1i1p1_KNMI_RACMO22E.csv?token=AFqiAI6NIvTh0gVu35ljuKgABpGQ3OFpks5ap59fwA%3D%3D"
tb_decade <- read_csv(url_decade)

years <- unique(tb_decade$year)
decades <- unique(tb_decade$decade)


url_year <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/d9ae799c-9564-4047-8a44-f5bdd25ab2cb/download/yearly_ichec-ec-earth_rcp85_r12i1p1_smhi-rca4_8_indices_30_cities.csv"
tb_year <- read_csv(url_year)
years_list <- unique(tb_year$year)
cities_list <- unique(tb_year$city)

# prepare data for sparklines
tb_spark <- tb_decade %>%
  spread(key = "index", value = "value") %>%
  select(-tmstmp, -date, -year) %>%
  group_by(city) %>%
  summarise(
    hwmid = spk_chr(
      hwmid, 
      type="bar"
      #chartRangeMin=0, 
      #chartRangeMax=max(tb_decade$hwmid)
    ),
    tn10p = spk_chr(
      tn10p, 
      type="line"
    ),
    tn90p = spk_chr(
      tn90p, 
      type="line"
    ),
    tx10p = spk_chr(
      tx10p, 
      type="line",
      lineColor = 'black', 
      fillColor = '#ccc'
    ),
    tx90p = spk_chr(
      tx90p, 
      type="line",
      lineColor = 'black', 
      fillColor = '#ccc'
    )
    
  ) 

# Required Call back function to put sparklines into DT tables 
cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')



