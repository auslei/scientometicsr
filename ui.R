library(shiny)
library(shinydashboard)
library(highcharter)

source('./server.R')

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  title = "Scientometic Data Analyser",
  # Application title
  header = dashboardHeader(title = "Scientometic Data Analyser"),
  
  # Sidebar with a slider input for number of bins 
  sidebar = dashboardSidebar(
       
    uiOutput('filters'),
    fileInput(inputId= 'data_file', label = "Upload Data File (WoS)", multiple = FALSE,
              accept = c("txt/tsv", ".txt", '.csv'))
    
    
  ),
  
  body = dashboardBody(
    #useShinyjs(),
    #tags$script(HTML("$('body').addClass('fixed');")),
    #tags$head(
    #  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #),
    
    # Show a plot of the generated distribution
    
    fluidRow(
      box(title = "Trending", width = 6, highchartOutput("trend_chart")),
      box(title = "Publications", width = 6, highchartOutput("treemap")),
    ),
    fluidRow(
      box(title = "Data Table" , width = 8, div(dataTableOutput("data_table"), style = "font-size:80%;")),
      box(title = "Word Cloud", width = 4, wordcloud2Output("wordcloud", width = "auto"))
    )
  
  )
)