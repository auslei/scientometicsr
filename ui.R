library(shiny)
library(shinydashboard)
library(highcharter)
library(shinycssloaders)
library(dashboardthemes)

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
    
    
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    
    # Show a plot of the generated distribution
    
    fluidRow(width = 12,
      uiOutput("infoboxes")
    ),
    
    tabBox(title = "", width = 12, 
        tabPanel("General Stats", width = "100%",
          fluidRow(width = 12,
            column(width = 6,
              box(title = "Trending", width = "100%", highchartOutput("trend_chart") %>% withSpinner(color="#0dc5c1"))
            ),
            column(width = 6,
              box(title = "Publications", width = "100%", highchartOutput("treemap") %>% withSpinner(color="#0dc5c1"))
            )
          ),
          fluidRow(
            width = 12,
            column(width = 6, 
              box(title = "Word Cloud", width = "100%", wordcloud2Output("wordcloud") %>% withSpinner(color="#0dc5c1"))
            )
          ),
        ),
        tabPanel("Co-citation Analysis", 
             
             fluidRow(width = 12, 
                  column(6,
                    box("Article Network", width = "100%", 
                        uiOutput("network_filter"),
                        plotOutput("network") %>% withSpinner(color="#0dc5c1"),
                        wordcloud2Output("network_wc") %>% withSpinner(color="#0dc5c1")
                    )),
                  column(6,
                    box("Summary", width = "100%", div(dataTableOutput("dt_citation_summary"), style = "font-size:80%;"))
                  )
             ),
             fluidRow(width = 12,
                  column(width = 12,
                      box("Inter-connected Articles", width = "100%", div(dataTableOutput("dt_citation"), style = "font-size:80%;"))
                  )
             )
            
        ),
        
        tabPanel("Raw Data", 
                 fluidRow(width = 12,
                          column(width = 12,
                                 box(title = "Data Table" , width = "100%", div(dataTableOutput("data_table"), style = "font-size:80%;"))
                          )
        )
                 )
    )
  )
)