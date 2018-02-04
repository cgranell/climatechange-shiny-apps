library(readr)
library(dplyr)
library(lubridate)

# Define the list of options

indices <- structure(c("hwmid", "tn10p"), 
                     .Names = c("Heat Wave Magnitude Index-daily (HWMId)", "Cold nights (TN10P)"))

url_decade <- "https://raw.githubusercontent.com/cgranell/climatechange-viz/master/output/dec_mean_allindices_allcities_ICHEC_EC_EARTH_rcp85_r1i1p1_KNMI_RACMO22E.csv?token=AFqiACzgr_kV__sNgG9-xHmLv_Bdo3Ptks5agFxCwA%3D%3D"
tb_decade <- read_csv(url_decade)

years <- unique(tb_decade$year)
decades <- unique(tb_decade$decade)

