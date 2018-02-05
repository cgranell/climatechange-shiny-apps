#
# This is a Shiny dashboard web application.
# 
# https://shiny.rstudio.com/articles/dashboards.html
# http://rstudio.github.io/shinydashboard/get_started.html


library(shiny)
library(shinydashboard)
# library(shinythemes)
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
              infoBoxOutput("modelInfo"),
              infoBoxOutput("indexInfo"),
              infoBoxOutput("decadeInfo")
            ),
            fluidRow(
              column(width = 9,
                tabBox(
                   title = "Compare & Explore",
                   # The id lets us use input$tabset1 on the server to find the current tab
                   id = "tabset1", width = 12, height = "550px",
                   selected = "tabChart",
                   tabPanel(title = "Compare", value = "tabChart", highchartOutput("hcontainer", width="100%", height = "500px")),
                   tabPanel(title = "Explore", value = "tabChart", DT::dataTableOutput("dataTable"))
                 ),
                # box(
                #   width = NULL, status = "info", 
                #   title = "Compare", 
                #   highchartOutput("hcontainer", width="100%", height = "400px")),
                # box(
                #   width = NULL,
                #   title = "Explore data", 
                #   DT::dataTableOutput("dataTable")
                # )
                box(
                  width = 12,
                  #title = "Sparklines",
                  htmlwidgets::getDependency('sparkline'),  
                  DT::dataTableOutput("sparklineTable")
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
               
                
                # helpText("Click the column header to sort a column."),
                
                box(width = NULL,
                   selectInput("plotTypeSelected", 
                               label = "Plot type:",
                               choices = c("Line (spline)" = "spline", 
                                           "Line" = "line",
                                           "Bar" = "column",
                                           "Scatter" = "scatter",
                                           "Area" = "area",
                                           "Area (spline)" = "areaspline")))
                
              )
            )
          ),
    tabItem("years"),
    tabItem("about", 
            fluidRow(
              box(title = "Help", status = "warning", width = 12,
                  p(
                    class = "text-muted",
                    paste("Use the Decadal chart tab to compare diffent simulation plots. ", 
                          "Use the Yearly Explorer tab to examine the raw data in tabular form. ",
                          "To learn more about the project, visit GEO-C project: http://www.geo-c.eu. ",
                          "Application author: Marek Smid.")
                    )
                )
            ))
  )
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Decadal forecast", tabName = "decades", icon = icon("bar-chart")),
    menuItem("Yearly forecast", tabName = "years", icon = icon("bar-chart")),
    menuItem("About", tabName = "about", icon = icon("info light"))
  )
)
  

ui <- dashboardPage(skin = "blue",
  header,
  sidebar,
  body
)




server <- function(input, output) { 
  
  output$modelInfo <- renderInfoBox({
    
    # current_model <- "EUR-11_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22E"
    current_model <- "CORDEX Model"
    valueBox(
      value = current_model,
      subtitle = "Selected ensemble",
      color = "light-blue",
      icon = icon("cog", lib = "glyphicon")
    )
  })
  
  
  output$indexInfo <- renderInfoBox({
    
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
      filter(city %in% c('Athens')) %>%
      filter(index %in% input$indexSelected)
  })
  
  tb_berlin <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Berlin')) %>%
      filter(index %in% input$indexSelected)
  })
  
  tb_brussels <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Brussels')) %>%
      filter(index %in% input$indexSelected)
  })
  
  tb_lisbon <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Lisbon')) %>%
      filter(index %in% input$indexSelected)
  })
  
  tb_london <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('London')) %>%
      filter(index %in% input$indexSelected)
  })
  
  tb_madrid <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Madrid')) %>%
      filter(index %in% input$indexSelected)
  })
  
  tb_paris <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Paris')) %>%
      filter(index %in% input$indexSelected)
  })
  
  tb_rome <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Rome')) %>%
      filter(index %in% input$indexSelected)
  })
  
  tb_warsaw <- reactive({
    tb_decade %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      filter(city %in% c('Warsaw')) %>%
      filter(index %in% input$indexSelected)
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
      hc_chart(type = input$plotTypeSelected)  %>%
      hc_add_series(name = "Athens",
                    data = tb_athens()$value) %>%
      hc_add_series(name = "Berlin",
                    data = tb_berlin()$value) %>%
      hc_add_series(name = "Brussels",
                    data = tb_brussels()$value) %>%
      hc_add_series(name = "Lisbon",
                    data = tb_lisbon()$value) %>%
      hc_add_series(name = "London",
                    data = tb_london()$value) %>%
      hc_add_series(name = "Madrid",
                    data = tb_madrid()$value) %>%
      hc_add_series(name = "Paris",
                    data = tb_paris()$value) %>%
      hc_add_series(name = "Rome",
                    data = tb_rome()$value) %>%
      hc_add_series(name = "Warsaw",
                    data = tb_warsaw()$v)
    
    hc <- hc %>%
      hc_xAxis(
        type = "datetime",
        title = list(text = "Decades"),
        opposite = TRUE,
        labels = list(format = "{value}"),
        gridLineWidth = 0.5,
        categories = selected_decades()) %>%

      hc_yAxis(
        title = list(text = input$indexSelected),
        min = 0,
        tickInterval = 5,
        minorTickInterval = 2.5) %>%


      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>: <b>{point.y:,.4f}</b><br/>",
                 shared = TRUE, crosshairs = TRUE) %>%

      hc_legend(align = "center",
                verticalAlign = "bottom",
                layout = "horizontal",
                enabled = TRUE)  %>% # to enable/unable data series when clicking on a lengend item

      hc_credits(enabled = TRUE,
                 text = "Sources: CORDEX, GEO-C",
                 href = "http://geo-c-eu",
                 style = list(fontSize = "10px")) %>%
      hc_exporting(enabled = TRUE) # enable exporting option
  
    
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
    tb_decade_table <- tb_decade %>%
      filter(index %in% input$indexSelected) %>%
      filter(between(year(date),input$decadeSelected[1], input$decadeSelected[2])) %>%
      select(-year, -tmstmp, -date) %>%
      arrange(city)       
    
    DT::datatable(tb_decade_table, options = list(lengthMenu = c(10, 15), pageLength = 10))
  })
  
  
  output$sparklineTable <- DT::renderDataTable(
    DT::datatable(tb_spark, 
                  escape= FALSE, 
                  options = list(drawCallback =  cb, 
                                 lengthMenu = c(5, 10), 
                                 pageLength = 5, 
                                 language = list(search = 'Filter:')))
  )
  
}

shinyApp(ui, server)