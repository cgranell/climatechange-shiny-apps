# library(readr)
# library(dplyr)
library(tidyverse)
library(lubridate)
library(sparkline)

# Define the list of options
# indices <- structure(c("hwmid", "tn10p"), 
#                      .Names = c("Heat Wave Magnitude Index-daily (HWMId)", "Cold nights (TN10P)"))

url_decade <- "http://giv-oct2.uni-muenster.de:5000/dataset/4f4766b5-4f62-4f30-b72f-81e15c2976ce/resource/82ba7bbc-31d2-428a-80db-73ae759d6ac4/download/dec_mean_allindices_allcities_ichec_ec_earth_rcp85_r1i1p1_knmi_racmo22e.csv"
tb_decade <- read_csv(url_decade)

years <- unique(tb_decade$year)
decades <- unique(tb_decade$decade)


url_year1 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/d9ae799c-9564-4047-8a44-f5bdd25ab2cb/download/yearly_ichec-ec-earth_rcp85_r12i1p1_smhi-rca4_8_indices_30_cities.csv"
url_year2 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/c345af25-11c9-4686-bf8a-c32cfbb76fa7/download/yearly_ichec_ec_earth_rcp85_r1i1p1_knmi_racmo22e_8_indices_30_cities.csv"

# Load simulated data for fist model here to retrive the list of years and cities from the data. This variable (tb_year1) is no longer used
tb_year1 <- read_csv(url_year1)
tb_year2 <- read_csv(url_year2)

years_list <- unique(tb_year1$year)
cities_list <- unique(tb_year1$city)


# prepare (decadal) data for sparklines
tb_spark_decade <- tb_decade %>%
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

# prepare (yearly) data for sparklines
tb_spark_year <- tb_year1 %>%
  spread(key = "index", value = "value") %>%
  select(-tmstmp, -date) %>%
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



