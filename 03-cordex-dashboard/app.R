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
  title = "MetroHeat web tool: a communication service of climate change impacts on temperature over European capitals",
  titleWidth = 450)

body <- dashboardBody(
  tabItems(
    ##########
    ## DECADAL FORECAST MENU ITEM
    ##########
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
                 )#,
                # box(
                #   width = 12,
                #   #title = "Sparklines",
                #   htmlwidgets::getDependency('sparkline'),  
                #   DT::dataTableOutput("sparklineTable")
                # )
            
              ),
              column(width = 3,
                     
                box(width = NULL, status = "danger",
                   selectInput("modelSelected", 
                               label = "Model:",
                               choices = c("ICHEC_EC_EARTH_rcp85_r1i1p1_KNMI_RACMO22E" = "ICHEC-EC-EARTH"))),
                     
                box(width = NULL, status = "danger",
                   selectInput("indexSelected", 
                               label = "Index:",
                               choices = c("Heat Wave Magnitude Index-daily (HWMId)" = "hwmid", 
                                           "Cold nights (TN10p)" = "tn10p",
                                           "Warm nights (TN90p)" = "tn90p",
                                           "Cold days (TX10p)" = "tx10p",
                                           "Warm days (TX90p)" = "tx90p"))),
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
    ##########
    ## YEARLY FORECAST MENU ITEM
    ##########
    tabItem("years",
            fluidRow(
              infoBoxOutput("yearModelInfo"),
              infoBoxOutput("yearIndexInfo"),
              infoBoxOutput("yearInfo")
            ),
            fluidRow(
              column(width = 9,
                     tabBox(
                       title = "Compare & Explore",
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabset1", width = 12, height = "550px",
                       selected = "tabChart",
                       tabPanel(title = "Compare", value = "tabChart", highchartOutput("yearHighChart", width="100%", height = "500px")),
                       tabPanel(title = "Explore", value = "tabChart", DT::dataTableOutput("yearDataTable"))
                     )#,
                    # box(
                    #   width = 12,
                    #   #title = "Sparklines",
                    #   htmlwidgets::getDependency('sparkline'),  
                    #   DT::dataTableOutput("yearSparklineTable")
                    # )
                     
              ),
              column(width = 3,
                     box(width = NULL, status = "danger",
                         selectInput("yearModelSelected", 
                                     label = "Model:",
                                     choices = c("ICHEC-EC-EARTH_rcp85_r12i1p1_SMHI-RCA4" = "ICHEC_SMHI",
                                                 "ICHEC_EC_EARTH_rcp85_r1i1p1_KNMI_RACMO22E" = "ICHEC_KNMI",
                                                 "ICHEC-EC-EARTH_rcp85_r3i1p1_DMI-HIRHAM5" = "ICHEC_DMI",
                                                 "IPSL-IPSL-CM5A-MR_rcp85_r1i1p1_IPSL-INERIS-WRF331F" = "IPSL_INERIS",
                                                 "IPSL-IPSL-CM5A-MR_rcp85_r1i1p1_SMHI-RCA4" = "IPSL_SMHI",
                                                 "MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E" = "MOHC_KNMI",
                                                 "MOHC-HadGEM2-ES_rcp85_r1i1p1_SMHI-RCA4" = "MOHC_SMHI",
                                                 "MPI-M-MPI-ESM-LR_rcp85_r1i1p1_SMHI-RCA4" = "MPI_SMHI"))),
                     
                     box(width = NULL, status = "danger",
                         selectInput("yearIndexSelected", 
                                     label = "Index:",
                                     choices = c("Heat Wave Magnitude Index-daily (HWMId)" = "HWMId", 
                                                 "Cold nights (TN10p)" = "tn10p",
                                                 "Warm nights (TN90p)" = "tn90p",
                                                 "Cold days (TX10p)" = "tx10p",
                                                 "Warm days (TX90p)" = "tx90p",
                                                 "Minimum of daily min temp. (TNn)" = "TNn",
                                                 "Maximum of daily min temp. (TNx)" = "TNx",
                                                 "Maximum of daily max temp. (TXx)" = "TXx"))),
            
                     box(width = NULL, status = "warning",
                         sliderInput("yearSelected",
                                     "Years:",
                                     min = min(years_list),
                                     max = max(years_list),
                                     step = 1, # year 
                                     sep = "",
                                     value = range(years_list))),
                     
                     
                     # box for city
                     box(width = NULL, status = "warning",
                         #uiOutput("yearCityBox")
                         selectizeInput("yearCitiesSelected", label = "City:", choices = list(
                                        `Mediterranean and Southern Europe` = c(`Athens` = "Athens", `Bucharest` = "Bucharest", `Lefkosia` = "Lefkosia", 
                                                                                `Lisbon` = "Lisbon", `Ljubjana` = "Ljubjana", `Madrid` = "Madrid",
                                                                                `Rome` = "Rome", `Sofia` = "Sofia", `Valleta` = "Valleta", `Zagreb` = "Zagreb"),
                                        `North and Continental Europe` = c(`Amsterdam` = "Amsterdam", `Berlin` = "Berlin", `Bratislava` = 'Bratislava', `Brussels` = "Brussels",
                                                                          `Budapest` = "Budapest", `Copenhagen` = "Copenhagen", `Dublin` = 'Dublin', `Helsinki` = "Helsinki",
                                                                          `London` = "London", `Luxembourg` = "Luxembourg", `Moscow` = 'Moscow', `Paris` = "Paris",
                                                                          `Prague` = "Prague", `Riga` = "Riga", `Stockholm` = 'Stockholm', `Tallin` = "Tallin",
                                                                          `Vilnius` = "Vilnius", `Warsaw` = "Warsaw", `Wien` = 'Wien', `Zurich` = "Zurich", `Oslo` = "Oslo")
                                          ),
                                        multiple = TRUE,
                                        options = list(placeholder = "Type a city name, e.g. Athens"))
                     ),
 
                     
                     # helpText("Click the column header to sort a column."),
                     
                     box(width = NULL,
                         selectInput("yearPlotTypeSelected", 
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
    
    ##########
    ## SPARKLINES MENU ITEM
    ##########
    tabItem("sparklines",
            fluidRow(
              column(width = 12,
                     box(
                       width = 12,
                       title = "Sparklines - Decadal simulation",
                       htmlwidgets::getDependency('sparkline'),  
                       DT::dataTableOutput("sparklineTable")
                     ),
                     box(
                       width = 12,
                       title = "Sparklines - Yearly simulation",
                       htmlwidgets::getDependency('sparkline'),  
                       DT::dataTableOutput("yearSparklineTable")
                     )))
                    
    ),
    
    ##########
    ## ABOUT MENU ITEM
    ##########
    tabItem("about", 
            fluidRow(
              
              box(title = "Contact", status = "warning", width = 12,
                  withTags({
                    div(class="header", checked=NA,
                        h4("Application author"),
                        b("Marek Smid"),
                        h4("Project web site"),
                        a(href="http://www.geo-c.eu", "GEO-C project")
                    )
                  })
              
              ),
              
              box(title = "Usage", status = "warning", width = 12,
                  withTags({
                    div(class="header", checked=NA,
                        p(class = "text-muted", "Use the Decadal forecast tab to examine and compare plots for decadal simulated data."),
                        p(class = "text-muted", "Use the Yearly forecast tab to examine and compare plots for yearly simulated.")
                    )
                  })
                ),
              
              box(title = "Information", status = "warning", width = 12,
                  withTags({
                    div(class="header", checked=NA,
                        p(
                          class = "text-muted",
                          paste("Absolute indices represent the upper or lower extreme per chosen period of time (Sillmann, Kharin, Zwiers, et al., 2013),  ",
                                "and they are often used by engineers to infer the design of the infrastructures (Zhang et al., 2011). ",
                                "Here, we computed the maximum and minimum of daily minimum temperatures for each year (TNx and TNn respectively) (Alexander et al., 2006), ",
                                "and also the annual maximum of daily maximum temperature (<b>TXx</b>) (Alexander et al., 2006) ",
                                "to capture the evolution of extreme heat peaks."
                          )),
                        a(href="https://doi.org/10.1002/jgrd.50203", "(Sillmann, Kharin, Zwiers, et al., 2013)"),
                        a(href="https://doi.org/10.1029/2005JD006290", "(Alexander et al., 2006)")
                    )
                  })
                  
                  # p(
                  #   class = "text-muted",
                  #   paste("Absolute indices represent the upper or lower extreme per chosen period of time (Sillmann, Kharin, Zwiers, et al., 2013b),  ",
                  #         "https://doi.org/10.1002/jgrd.50203",
                  #         "and they are often used by engineers to infer the design of the infrastructures (Zhang et al., 2011). ",
                  #         "Here, we computed the maximum and minimum of daily maximum temperatures for each year (<b>TNx</b> and </b>TNn respectively) (Alexander et al., 2006), ",
                  #         "and also the annual maximum of daily maximum temperature (<b>TXx</b>) (Alexander et al., 2006) ",
                  #         "to capture the evolution of extreme heat peaks.",
                  #         " "
                  # 
                  #         )
                  # )
              )
            
            ))
  )
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Decadal forecast", tabName = "decades", icon = icon("bar-chart", lib="font-awesome")),
    menuItem("Yearly forecast", tabName = "years", icon = icon("bar-chart", lib="font-awesome")),
    menuItem("Sparklines", tabName = "sparklines", icon = icon("line-chart", lib="font-awesome")),
    menuItem("About", tabName = "about", icon = icon("info light", lib="font-awesome"))
  )
)
  

ui <- dashboardPage(skin = "blue",
  header,
  sidebar,
  body
)




server <- function(input, output, session) { 
  
  ########################
  ## DECADAL FORECAST PAGE
  ########################
  
  output$modelInfo <- renderInfoBox({
    current_model <- input$modelSelected
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
                    data = tb_warsaw()$value)
    
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
    DT::datatable(tb_spark_decade, 
                  escape= FALSE, 
                  options = list(drawCallback =  cb, 
                                 lengthMenu = c(5, 10), 
                                 pageLength = 5, 
                                 language = list(search = 'Filter:')))
  )
  
  
  ########################
  ## YEARLY FORECAST PAGE
  ########################
  
  
  output$yearModelInfo <- renderInfoBox({
    
    # current_model <- "ICHEC-EC-EARTH_rcp85_r12i1p1_SMHI-RCA4"
    # current_model <- "CORDEX Model"
    current_model <- input$yearModelSelected
    valueBox(
      value = current_model,
      subtitle = "Selected ensemble",
      color = "light-blue",
      icon = icon("cog", lib = "glyphicon")
    )
  })
  
  
  output$yearIndexInfo <- renderInfoBox({
    
    current_index = input$yearIndexSelected
    valueBox(
      value = current_index,
      subtitle = "Selected index",
      color = "red",
      icon = icon("list")
    )
  })
  
  
  output$yearInfo <- renderInfoBox({
    if(input$yearSelected[1] == input$yearSelected[2]) { 
        current_years <- as.character(input$yearSelected[1])
    } else {
      current_years <- paste("Between ",input$yearSelected[1], " and ", input$yearSelected[2])
    }
    
    valueBox(
      value = current_years,
      color = "yellow",
      subtitle = "Selected years",
      icon = icon("calendar")
    )
  })
  
  
  
  # FLags: https://github.com/hjnilsson/country-flags/tree/master/png100px
  # Help:  http://shiny.leg.ufpr.br/daniel/
  #        https://github.com/selectize/selectize.js/blob/master/docs/usage.md
  #        https://www.r-bloggers.com/bike-services-api-shiny-nice-app/
  
  
  
  # # update the render function for selectize
  # updateSelectizeInput(
  #   session, 'yearCitiesSelected', server = TRUE, 
  #   #choices = c(`Athens` = "Athens", `Berlin` = "Berlin", `Brussels` = "Brussels"), #cities_list,
  #   choices = cities_list,
  #   options = list(
  #     create = FALSE,
  #     #valueField = 'url',
  #     labelField = 'name',
  #     searchField = 'name',
  #     #options = list(),
  #     render = I(
  #       "{
  #         option: function(item, escape) {
  #         return '<div>' +
  #           '<img src=\"images/flag/' + 
  #           (item.name) + 
  #           '.png\" width=20 />'  +
  #           '&nbsp; &nbsp; &nbsp;' + 
  #           escape(item.name) +
  #           '</div>';
  #         }
  #       }"))
  #   )
  
  
  # output$yearCityBox = renderUI({
  #     selectizeInput('yearCitiesSelected', 'City:', 
  #                     choices = cities_list, 
  #                     multiple = TRUE,
  #                     selected = "Athens",
  #                     options = list(
  #                       valueField = 'url',
  #                       labelField = 'name',
  #                       searchField = 'name',
  #                       options = list(),
  #                       create = FALSE)) #,
  #                       # #To add the flag next to the countries
  #                       # render = I(
  #                       #   '{
  #                       #     option: function(item, escape) {
  #                       #       return "<div>" +
  #                       #           "<img src=\'/images/flag/" +
  #                       #             item.name +
  #                       #             ".png\' width=20 />"  +
  #                       #             "&nbsp; &nbsp; &nbsp;" +
  #                       #             escape(item.name) +
  #                       #             "</div>";
  #                       #             }
  #                       #           }')))
  # })

  
  # Reload simulated data for the selected model, which meand to read a separate data file per model 
  tb_year <- reactive({
    if (input$yearModelSelected=="ICHEC_SMHI") {
      tb_year1
    } else if (input$yearModelSelected=="ICHEC_KNMI") {
      tb_year2
    } else if (input$yearModelSelected=="ICHEC_DMI") {
      tb_year3
    } else if (input$yearModelSelected=="IPSL_INERIS") {
      tb_year4
    } else if (input$yearModelSelected=="IPSL_SMHI") {
      tb_year5
    } else if (input$yearModelSelected=="MOHC_KNMI") {
      tb_year6
    } else if (input$yearModelSelected=="MOHC_KNMI") {
      tb_year7
    } else if (input$yearModelSelected=="MPI_SMHI") {
      tb_year8
    }
  })
  

  
  # Recalculate data series for current selection of cities
  selected_cities <- reactive({
    list_cities <- list()
    if (length(input$yearCitiesSelected)>0) {
      for (c in input$yearCitiesSelected) {
        list_cities[[c]] <- tb_year() %>%
          filter(between(year(date),input$yearSelected[1], input$yearSelected[2])) %>%
          filter(city %in% c) %>%
          filter(index %in% input$yearIndexSelected)
      }
    }
    list_cities
  })
  
  
  # Range of selected years for updating plot's xAxis categories  
  selected_years <- reactive({
    index_year_min = which(years_list == input$yearSelected[1])
    index_year_max = which(years_list == input$yearSelected[2])
    years_list[index_year_min:index_year_max]
  })
  
  
  # Highchart -------------------------------------------------------
  output$yearHighChart <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_chart(type = input$yearPlotTypeSelected) 

    if (!is.null(names(selected_cities()))) {
        for (cityname in names(selected_cities())) {
          hc <- hc %>%
            hc_add_series(name = cityname,
                          data = selected_cities()[[cityname]]$value)
        }
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
        title = list(text = input$yearIndexSelected),
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
  
  
  output$yearDataTable <- DT::renderDataTable({
    tb_year_table <- tb_year() %>%
      filter(index %in% input$yearIndexSelected) %>%
      filter(between(year(date),input$yearSelected[1], input$yearSelected[2])) %>%
      filter(city %in% names(selected_cities())) %>%
      select(-tmstmp, -date) %>%
      arrange(city, year)       
    
    DT::datatable(tb_year_table, options = list(lengthMenu = c(10, 15), pageLength = 10))
  })
  
  
  # Reload sparkline data for the selected model, which depends on the current data in tb_year
  tb_spark_year <- reactive({
    
    # prepare (yearly) data for sparklines
    tb_year() %>%
      spread(key = "index", value = "value") %>%
      select(-tmstmp, -date) %>%
      group_by(city) %>%
      summarise(
        hwmid= spk_chr(
          HWMId, 
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
  })
  
  
  output$yearSparklineTable <- DT::renderDataTable(
    DT::datatable(tb_spark_year(), 
                  escape= FALSE, 
                  options = list(drawCallback =  cb, 
                                 lengthMenu = c(10, 15), 
                                 pageLength = 10, 
                                 language = list(search = 'Filter:')))
  )
  

  
}

shinyApp(ui, server)