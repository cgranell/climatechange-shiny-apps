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


url_year1 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/d9ae799c-9564-4047-8a44-f5bdd25ab2cb/download/yearly_ichec-ec-earth_rcp85_r12i1p1_smhi-rca4_8_indices_31_cities.csv"
url_year2 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/c345af25-11c9-4686-bf8a-c32cfbb76fa7/download/yearly_ichec_ec_earth_rcp85_r1i1p1_knmi_racmo22e_8_indices_31_cities.csv"
url_year3 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/1c9a046c-3deb-447d-8333-94486f919a81/download/yearly_ichec-ec-earth_rcp85_r3i1p1_dmi-hirham5_8_indices_31_cities.csv"
url_year4 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/9f4340d3-e9b3-4cff-ab3b-dc83f4facf8a/download/yearly_ipsl-ipsl-cm5a-mr_rcp85_r1i1p1_ipsl-ineris-wrf331f_8_indices_31_cities.csv"
url_year5 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/b249d65b-d0c1-4f77-a802-e924821dc825/download/yearly_ipsl-ipsl-cm5a-mr_rcp85_r1i1p1_smhi-rca4_8_indices_31_cities.csv"
url_year6 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/6a5fa594-d81b-47a1-a0bc-42f7c0e7878b/download/yearly_mohc-hadgem2-es_rcp85_r1i1p1_knmi-racmo22e_8_indices_31_cities.csv"
url_year7 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/f0166844-430d-4a82-8666-5e0e126b3261/download/yearly_mohc-hadgem2-es_rcp85_r1i1p1_smhi-rca4_8_indices_31_cities.csv"
url_year8 <- "http://giv-oct2.uni-muenster.de:5000/dataset/0c080c72-b89f-48b6-aff5-c18a70b0526d/resource/729d4e15-8095-4124-a020-cf2fc0a233d4/download/yearly_mpi-m-mpi-esm-lr_rcp85_r1i1p1_smhi-rca4_8_indices_31_cities.csv"

# Load simulated data for fist model here to retrive the list of years and cities from the data. This variable (tb_year1) is no longer used
tb_year1 <- read_csv(url_year1)
tb_year2 <- read_csv(url_year2)
tb_year3 <- read_csv(url_year3)
tb_year4 <- read_csv(url_year4)
tb_year5 <- read_csv(url_year5)
tb_year6 <- read_csv(url_year6)
tb_year7 <- read_csv(url_year7)
tb_year8 <- read_csv(url_year8)

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



