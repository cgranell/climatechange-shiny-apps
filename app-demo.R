library("shiny")
library("highcharter")

ui <- fluidPage(
  selectInput("nplots", "Choose", multiple = TRUE, width = "100%",
              choices = c("cars", "mtcars", "iris",
                          "Puromycin", "ChickWeight")),
  fluidRow(
    column(12, htmlOutput("hcontainer")),
    highchartOutput("hcontainer2", height = "0", width = "0")
    # the previous output is hide. This is needed load highcharts/highcharter
    # javascript in the app
  )
)

server <- function(input, output) {
  
  gethc <- function(dfname = "cars") {
    # function to return the chart in a column div
    df <- get(dfname)
    
    hc <- highchart(height = 300) %>% 
      hc_title(text = dfname) %>% 
      hc_xAxis(title = list(text = names(df)[1])) %>% 
      hc_yAxis(title = list(text = names(df)[2])) %>% 
      hc_add_series_scatter(df[,1], df[, 2]) %>% 
      hc_add_theme(
        list(hc_theme_538(), hc_theme_economist(), hc_theme_darkunica())[sample(1:3, size = 1)][[1]]
      )
    
    column(width = 6, hc)
    
  }
  
  output$hcontainer <- renderUI({
    # input <- list(nplots = c("cars", "mtcars"))
    charts <- lapply(input$nplots, gethc)
    do.call(tagList, charts)
    
  })  
  
}

shinyApp(ui = ui, server = server)