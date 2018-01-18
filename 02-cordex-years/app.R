#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
library(here)
library(lubridate)
library(highcharter)

file_name <- "year_mean_allindices_allcities_ICHEC_EC_EARTH_rcp85_r1i1p1_KNMI_RACMO22E.csv"
data_path <- here::here("output", file_name)

tb <- read_csv(data_path)

years <- unique(tb$year)

# UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Yearly median - All cities"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("years",
                     "Years:",
                     min = min(years),
                     max = max(years),
                     step = 1, # year 
                     sep = "",
                     value = range(years)),
      
         
         selectInput("index", 
                     label = "Index:",
                     choices = c("Heat Wave Magnitude Index-daily (HWMId)" = "hwmid", 
                                 "Cold nights (TN10P)" = "tn10p",
                                 "Warm nights (TN90P)" = "tn90p",
                                 "Cold days (TX10P)" = "tx10p",
                                 "Warm days (TX90P)" = "tx90p")),
         
         
         selectInput("plot_type", 
                     label = "Plot type:",
                     choices = c("Spline" = "spline", 
                                 "Line" = "line",
                                 "Bar" = "column")),
         
         selectInput("theme", 
                     label = "Theme:",
                     choices = c("No theme", 
                                 "Chalk" = "chalk",
                                 "Dark Unica" = "darkunica", 
                                 "Economist" = "economist",
                                 "FiveThirtyEight" = "fivethirtyeight", 
                                 "Gridlight" = "gridlight", 
                                 "Handdrawn" = "handdrawn", 
                                 "Sandsignika" = "sandsignika"))
      ),
         
      # Output 
      mainPanel(
         highchartOutput("hcontainer", height = "500px")
      )
   )
)


# SERVER
server <- function(input, output) {
  
  # Calculate data for selected index
  tb_athens <- reactive({
   tb %>% 
     filter(between(year(date),input$years[1], input$years[2])) %>%
     filter(city %in% c('Athens'))
  })
  
  tb_berlin <- reactive({
    tb %>%
      filter(between(year(date),input$years[1], input$years[2])) %>%
      filter(city %in% c('Berlin'))
  })

  tb_brussels <- reactive({
    tb %>%
      filter(between(year(date),input$years[1], input$years[2])) %>%
      filter(city %in% c('Brussels'))
  })

  tb_lisbon <- reactive({
    tb %>%
      filter(between(year(date),input$years[1], input$years[2])) %>%
      filter(city %in% c('Lisbon'))
  })

  tb_london <- reactive({
    tb %>%
      filter(between(year(date),input$years[1], input$years[2])) %>%
      filter(city %in% c('London'))
  })
  
  tb_madrid <- reactive({
    tb %>%
       filter(between(year(date),input$years[1], input$years[2])) %>%
       filter(city %in% c('Madrid'))
   })
     
  tb_paris <- reactive({
    tb %>%
      filter(between(year(date),input$years[1], input$years[2])) %>%
      filter(city %in% c('Paris')) 
  })
  
  tb_rome <- reactive({
    tb %>%
      filter(between(year(date),input$years[1], input$years[2])) %>%
      filter(city %in% c('Rome'))
  })
  
  tb_warsaw <- reactive({
    tb %>%
      filter(between(year(date),input$years[1], input$years[2])) %>%
      filter(city %in% c('Warsaw'))
  })
  
  # Text string of selected years for plot subtitle 
  selected_years_to_print <- reactive({
    if(input$years[1] == input$years[2]) { 
      as.character(input$years[1])
    } else {
      paste("Between ",input$years[1], " and ", input$years[2])
    }
  })
  
  # Text string of selected index for plot title 
  selected_index_to_print <- reactive({
    switch(input$index,
           hwmid = "Heat Wave Magnitude Index-daily (HWMId)",
           tn10p = "Cold nights (TN10P)",
           tn90p = "Warm nights (TN90P)",
           tx10p = "Cold days (TX10P)",
           tx90p = "Warm days (TX90P)")
    })
    
   
  # Range of selected years for updating plot's xAxis categories  
  selected_years <- reactive({
    index_year_min = which(years == input$years[1])
    index_year_max = which(years == input$years[2])
    years[index_year_min:index_year_max]
  })
  
  # Highchart -------------------------------------------------------
  output$hcontainer <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = input$plot_type)
    
    if (input$index == "hwmid") {
      hc <- hc %>% 
        hc_add_series(name = "Athens", 
                      data = tb_athens()$index_hwmid) %>% 
        hc_add_series(name = "Berlin",
                      data = tb_berlin()$index_hwmid) %>%
        hc_add_series(name = "Brussels",
                      data = tb_brussels()$index_hwmid) %>%
        hc_add_series(name = "Lisbon",
                      data = tb_lisbon()$index_hwmid) %>%
        hc_add_series(name = "London",
                      data = tb_london()$index_hwmid) %>%
        hc_add_series(name = "Madrid",
                       data = tb_madrid()$index_hwmid) %>%
        hc_add_series(name = "Paris",
                      data = tb_paris()$index_hwmid) %>%
        hc_add_series(name = "Rome",
                      data = tb_rome()$index_hwmid) %>%
        hc_add_series(name = "Warsaw",
                      data = tb_warsaw()$index_hwmid)

    } else if (input$index == "tn10p") {
      hc <- hc %>%
        hc_add_series(name = "Athens",
                    data = tb_athens()$index_tn10p) %>%
        hc_add_series(name = "Berlin",
                      data = tb_berlin()$index_tn10p) %>%
        hc_add_series(name = "Brussels",
                      data = tb_brussels()$index_tn10p) %>%
        hc_add_series(name = "Lisbon",
                      data = tb_lisbon()$index_tn10p) %>%
        hc_add_series(name = "London",
                      data = tb_london()$index_tn10p)  %>%
        hc_add_series(name = "Madrid",
                      data = tb_madrid()$index_tn10p) %>%
        hc_add_series(name = "Paris",
                      data = tb_paris()$index_tn10p) %>%
        hc_add_series(name = "Rome",
                      data = tb_rome()$index_tn10p) %>%
        hc_add_series(name = "Warsaw",
                      data = tb_warsaw()$index_tn10p)
    } else if (input$index == "tn90p") {
      hc <- hc %>%
        hc_add_series(name = "Athens",
                      data = tb_athens()$index_tn90p) %>%
        hc_add_series(name = "Berlin",
                      data = tb_berlin()$index_tn90p) %>%
        hc_add_series(name = "Brussels",
                      data = tb_brussels()$index_tn90p) %>%
        hc_add_series(name = "Lisbon",
                      data = tb_lisbon()$index_tn90p) %>%
        hc_add_series(name = "London",
                      data = tb_london()$index_tn90p) %>%
        hc_add_series(name = "Madrid",
                      data = tb_madrid()$index_tn90p) %>%
        hc_add_series(name = "Paris",
                      data = tb_paris()$index_tn90p) %>%
        hc_add_series(name = "Rome",
                      data = tb_rome()$index_tn90p) %>%
        hc_add_series(name = "Warsaw",
                      data = tb_warsaw()$index_tn90p)
    } else if (input$index == "tx10p") {
      hc <- hc %>%
        hc_add_series(name = "Athens",
                      data = tb_athens()$index_tx10p) %>%
        hc_add_series(name = "Berlin",
                      data = tb_berlin()$index_tx10p) %>%
        hc_add_series(name = "Brussels",
                      data = tb_brussels()$index_tx10p) %>%
        hc_add_series(name = "Lisbon",
                      data = tb_lisbon()$index_tx10p) %>%
        hc_add_series(name = "London",
                      data = tb_london()$index_tx10p)  %>%
        hc_add_series(name = "Madrid",
                      data = tb_madrid()$index_tx10p) %>%
        hc_add_series(name = "Paris",
                      data = tb_paris()$index_tx10p) %>%
        hc_add_series(name = "Rome",
                      data = tb_rome()$index_tx10p) %>%
        hc_add_series(name = "Warsaw",
                      data = tb_warsaw()$index_tx10p)

    } else {
      hc <- hc %>%
        hc_add_series(name = "Athens",
                      data = tb_athens()$index_tx90p) %>%
        hc_add_series(name = "Berlin",
                      data = tb_berlin()$index_tx90p) %>%
        hc_add_series(name = "Brussels",
                      data = tb_brussels()$index_tx90p) %>%
        hc_add_series(name = "Lisbon",
                      data = tb_lisbon()$index_tx90p) %>%
        hc_add_series(name = "London",
                      data = tb_london()$index_tx90p) %>%
        hc_add_series(name = "Madrid",
                      data = tb_madrid()$index_tx90p) %>%
        hc_add_series(name = "Paris",
                      data = tb_paris()$index_tx90p) %>%
        hc_add_series(name = "Rome",
                      data = tb_rome()$index_tx90p) %>%
        hc_add_series(name = "Warsaw",
                      data = tb_warsaw()$index_tx90p)

    }

    hc <- hc %>% 
      hc_xAxis(
        type = "datetime",
        title = list(text = "Years"),
        opposite = TRUE,
        labels = list(format = "{value}"),
        gridLineWidth = 0.5,
        categories = selected_years()) %>%
        
      hc_yAxis(
        title = list(text = selected_index_to_print()),
        min = 0,
        tickInterval = 5,
        minorTickInterval = 2.5) %>%
      
      hc_title(text = paste(selected_index_to_print(), " - All cities")) %>% 
      
      hc_subtitle(text = paste("Yearly median values according to model EUR-11_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22E for major european cities - ",
                               selected_years_to_print())) %>% 
      
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: <b>{point.y:,.4f}</b><br/>",
               shared = TRUE, crosshairs = TRUE) %>% 
      
      hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal")  %>%
      
      hc_credits(enabled = TRUE,
                 text = "Sources: CORDEX, GEO-C",
                 style = list(fontSize = "10px")) %>%
      hc_exporting(enabled = TRUE) # enable exporting option
    
    # Determine theme and apply to highchart 
    if (input$theme != "No theme") {
      theme <- switch(input$theme,
                      chalk = hc_theme_chalk(),
                      darkunica = hc_theme_darkunica(),
                      fivethirtyeight = hc_theme_538(),
                      gridlight = hc_theme_gridlight(),
                      handdrawn = hc_theme_handdrawn(),
                      economist = hc_theme_economist(),
                      sandsignika = hc_theme_sandsignika()
      )
      
      hc <- hc %>%
        hc_add_theme(theme)
    }
  
    # Print highchart 
    hc  
  
  })
  
}


# RUN 
shinyApp(ui = ui, server = server)

