#
# This is a Shiny dashboard web application.
# 
# https://shiny.rstudio.com/articles/dashboards.html
# http://rstudio.github.io/shinydashboard/get_started.html


library(shiny)
library(shinydashboard)
library(shinythemes)
library(highcharter)

source("global.R")

header <- dashboardHeader(
  title = "Climate simulation in major European cities (Marek)",
  titleWidth = 450)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            fluidRow(
              valueBoxOutput("modelInfo"),
              valueBoxOutput("indexInfo"),
              valueBoxOutput("appInfo")
            ),
            fluidRow(
              column(width = 8,
                box(
                  width = NULL, status = "info", solidHeader = TRUE,
                  title = "Decade chart", 
                  highchartOutput("hcontainer", width="100%", height = "500px"))
              ),
              column(width = 4,
                box(width = NULL, status = "warning",
                    uiOutput("indexSelect")
                )
              )
            )
          ),
    tabItem("explorer"),
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
    menuItem("Dashboard", tabName = "dashboard"),
    menuItem("Data explorer", tabName = "explorer"),
    menuItem("About", tabName = "about"),
    box("Use the Dashboard tab to compare diffent simulation plots. ", br(),
        "Use the Explorer tab to examine the raw data in tabular form. ", br(),
        "To learn more about the project, visit GEO-C project: http://www.geo-c.eu. ", br(),
        "Application author: Marek Smid.")
    )
)
  

ui <- dashboardPage(skin = "blue",
  header,
  sidebar,
  body
)






server <- function(input, output) { 
  
  output$modelInfo <- renderValueBox({
    
    current_model <- "CORDEX"
    valueBox(
      value = current_model,
      subtitle = "Selected model",
      icon = icon("download")
    )
  })
  
  
  output$indexInfo <- renderInfoBox({
    
    current_index <- input$index
    valueBox(
      value = current_index,
      subtitle = "Selected index",
      icon = icon("list")
    )
  })
  
  
}

shinyApp(ui, server)