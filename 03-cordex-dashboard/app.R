#
# This is a Shiny dashboard web application.
# 
# https://shiny.rstudio.com/articles/dashboards.html
# http://rstudio.github.io/shinydashboard/get_started.html


library(shiny)
library(shinydashboard)
library(shinythemes)
library(highcharter)
library(DT)


source("global.R")

header <- dashboardHeader(
  title = "Climate simulation in major European cities (Marek)",
  titleWidth = 450)

body <- dashboardBody(
  tabItems(
    tabItem("decades",
            fluidRow(
              valueBoxOutput("modelInfo"),
              valueBoxOutput("indexInfo"),
              valueBoxOutput("decadeInfo")
            ),
            fluidRow(
              column(width = 9,
                box(
                  width = NULL, status = "info", 
                  title = "Compare", 
                  highchartOutput("hcontainer", width="100%", height = "400px")),
                box(
                  width = NULL,
                  title = "Explore data", 
                  DT::dataTableOutput("dataTable")
                  # uiOutput("dataTable")
                )
                
              ),
              column(width = 3,
               box(width = NULL, status = "danger",
                   selectInput("indexSelected", 
                               label = "Index:",
                               choices = c("Heat Wave Magnitude Index-daily (HWMId)" = "hwmid", 
                                           "Cold nights (TN10P)" = "tn10p",
                                           "Warm nights (TN90P)" = "tn90p",
                                           "Cold days (TX10P)" = "tx10p",
                                           "Warm days (TX90P)" = "tx90p"))),
                box(width = NULL, status = "warning",
                    sliderInput("decadeSelected",
                                "Decades:",
                                min = min(years),
                                max = max(years),
                                step = 10, # decades, not years 
                                sep = "",
                                value = range(years))),
               
               box(width = NULL,
                   selectInput("plotTypeSelected", 
                               label = "Plot type:",
                               choices = c("Spline" = "spline", 
                                           "Line" = "line",
                                           "Bar" = "column")))
                
              )
            )
          ),
    tabItem("years"),
    tabItem("about", 
            fluidRow(
              box(title = "Help", status = "warning",
                  p(
                    class = "text-muted",
                    paste("Use the Dashboard tab to compare diffent simulation plots. ",
                          "Use the Explorer tab to examine the raw data in tabular form. ",
                          "To learn more about the project, visit GEO-C project: http://www.geo-c.eu. ",
                          "Application author: Marek Smid.")
                    )
                )
            ))
  )
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Decadal charts ", tabName = "decades", icon = icon("bar-chart")),
    menuItem("Yearly charts", tabName = "years", icon = icon("bar-chart")),
    menuItem("About", tabName = "about")
    # box("Use the Dashboard tab to compare diffent simulation plots. ", br(),
    #     "Use the Explorer tab to examine the raw data in tabular form. ", br(),
    #     "To learn more about the project, visit GEO-C project: http://www.geo-c.eu. ", br(),
    #     "Application author: Marek Smid.")
    )
)
  

ui <- dashboardPage(skin = "blue",
  header,
  sidebar,
  body
)




server <- function(input, output) { 
  
  output$modelInfo <- renderValueBox({
    
    current_model <- "EUR-11_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22E"
    valueBox(
      value = current_model,
      subtitle = "Selected ensemble",
      color = "light-blue",
      icon = icon("cog", lib = "glyphicon")
    )
  })
  
  
  output$indexInfo <- renderInfoBox({
    
    # current_index <- switch(input$indexSelected,
    #          hwmid = "Heat Wave Magnitude Index-daily (HWMId)",
    #          tn10p = "Cold nights (TN10P)",
    #          tn90p = "Warm nights (TN90P)",
    #          tx10p = "Cold days (TX10P)",
    #          tx90p = "Warm days (TX90P)")
    # 
    current_index = input$indexSelected
    valueBox(
      value = current_index,
      subtitle = "Selected index",
      color = "red",
      icon = icon("list")
    )
  })
  
  output$decadeInfo <- renderInfoBox({
    if(input$decadeSelected[1] == input$decadeSelected[2]) { 
      current_decades <- paste(as.character(input$decadeSelected[1]), 
                               "-" , 
                               as.character(input$decadeSelected[1]+9))
    } else {
      current_decades <- paste(as.character(input$decadeSelected[1]), 
                               "/" , 
                               as.character(input$decadeSelected[1]+9),
                               "-",
                               as.character(input$decadeSelected[2]), 
                               "/" , 
                               as.character(input$decadeSelected[2]+9)
                               )
    }
    
    valueBox(
      value = current_decades,
      color = "yellow",
      subtitle = "Selected decades",
      icon = icon("calendar")
    )
  })
  
  
  # Recalculate data series for current selection
  tb_athens <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Athens'))
  })
  
  tb_berlin <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Berlin'))
  })
  
  tb_brussels <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Brussels'))
  })
  
  tb_lisbon <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Lisbon'))
  })
  
  tb_london <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('London'))
  })
  
  tb_madrid <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Madrid'))
  })
  
  tb_paris <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Paris'))
  })
  
  tb_rome <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Rome'))
  })
  
  tb_warsaw <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Warsaw'))
  })
  
  
  # Range of decades of selected years for updating plot's xAxis categories  
  selected_decades <- reactive({
    index_year_min = which(years == input$decadeSelected[1])
    index_year_max = which(years == input$decadeSelected[2])
    decades[index_year_min:index_year_max]
  })
  
  
  
  
  
  # Highchart -------------------------------------------------------
  output$hcontainer <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = input$plotTypeSelected)  
    
    if (input$indexSelected == "hwmid") {
      # if (!is.null(tb_athens)) {
      #   hc <- hc %>%
      #   hc_add_series(name = "Athens",
      #                 data = tb_athens()$index_hwmid)}
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

    # hc <- hc %>% 
    #   hc_xAxis(
    #     type = "datetime",
    #     title = list(text = "Decades"),
    #     opposite = TRUE,
    #     labels = list(format = "{value}"),
    #     gridLineWidth = 0.5,
    #     categories = selected_decades()) %>%
    #   
    #   hc_yAxis(
    #     title = list(text = input$indexSelected),
    #     min = 0,
    #     tickInterval = 5,
    #     minorTickInterval = 2.5) %>%
    # 
    #   
    #   hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: <b>{point.y:,.4f}</b><br/>",
    #              shared = TRUE, crosshairs = TRUE) %>% 
    #   
    #   hc_legend(align = "center", 
    #             verticalAlign = "bottom", 
    #             layout = "horizontal", 
    #             enabled = TRUE)  %>% # to enable/unable data series when clicking on a lengend item 
    #   
    #   hc_credits(enabled = TRUE,
    #              text = "Sources: CORDEX, GEO-C",
    #              href = "http://geo-c-eu", 
    #              style = list(fontSize = "10px")) %>%
    #   hc_exporting(enabled = TRUE) # enable exporting option
  
    
    # Print highchart 
    hc  
    
  })
  
  
  # output$dataTable <- renderTable({ 
  #   tb_decade %>%
  #     select(-year, -tmstmp, -date) %>%
  #     arrange(city)         
  #   }, striped = TRUE, hover = TRUE, width = "100%", digits = 2)  
  # 
  output$dataTable <- DT::renderDataTable({
    tb_decade %>%
      select(-year, -tmstmp, -date) %>%
      arrange(city)         
  })
  
  
}

shinyApp(ui, server)