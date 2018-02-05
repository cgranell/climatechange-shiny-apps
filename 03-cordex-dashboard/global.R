# library(readr)
# library(dplyr)
library(tidyverse)
library(lubridate)
library(sparkline)

# Define the list of options

indices <- structure(c("hwmid", "tn10p"), 
                     .Names = c("Heat Wave Magnitude Index-daily (HWMId)", "Cold nights (TN10P)"))

url_decade <- "https://raw.githubusercontent.com/cgranell/climatechange-viz/master/output/dec_mean_allindices_allcities_ICHEC_EC_EARTH_rcp85_r1i1p1_KNMI_RACMO22E.csv?token=AFqiACzgr_kV__sNgG9-xHmLv_Bdo3Ptks5agFxCwA%3D%3D"
tb_decade <- read_csv(url_decade)

years <- unique(tb_decade$year)
decades <- unique(tb_decade$decade)


# create data with sparklines
spark_data <- data.frame(
  id = c('spark1', 'spark2'),
  spark = c(
    spk_chr(values = 1:3, elementId = 'spark1'),
    spk_chr(values = 3:1, elementId = 'spark2')
  )
)


tb_spark <- tb_decade %>%
  spread(key = "index", value = "value") %>%
  select(-tmstmp, -date, -year) %>%
  group_by(city) %>%
  summarise(
    sp_hwmid = spk_chr(
      hwmid, 
      type="bar"
      #chartRangeMin=0, 
      #chartRangeMax=max(tb_decade$hwmid)
    )
  ) 

# tb_spark <- tb_spark %>%
#   summarise(
#     sp_hwmid = spk_chr(
#       tb_spark$hwmid, 
#       type="line",
#       chartRangeMin=0, 
#       chartRangeMax=max(tb_spark$hwmid)
#     )
#   ) #%>% as.data.frame()
# 
#   # group_by(city)


###  adding this <------------
cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')

library(formattable)

x <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    hp = spk_chr(
      hp, type="box",
      chartRangeMin=0, chartRangeMax=max(mtcars$hp)
    ),
    mpg = spk_chr(
      mpg, type="box",
      chartRangeMin=0, chartRangeMax=max(mtcars$mpg)
    )
  ) %>%
  formattable() 

sparkline(x)

