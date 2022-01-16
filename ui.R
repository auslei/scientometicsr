library(shiny)
library(shinyjs)
library(shinydashboard)
library(highcharter)
library(shinycssloaders)
library(dashboardthemes)

source('./src/ui_elements/ui_components.R')
source('./src/ui_elements/dynamic_ui_components.R')
source('./server.R')

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  title = "Scientometic Data Analyser",
  # Application title
  header = dashboardHeader(title = "Scientometic Data Analyser"),
  
  # Sidebar with a slider input for number of bins 
  sidebar = dashboardSidebar(
    uiOutput('navbar_filter'),
    fileInput(inputId= 'data_file', label = "Upload Data File (WoS)", multiple = TRUE,
              accept = c("txt/tsv", ".txt", '.csv'))
    
    
  ),
  
  body = dashboardBody(
    useShinyjs(),
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
        ui_gen_status_panel(),
        tabPanel("Co-citation Analysis", 
             fluidRow(width = 12, 
                  column(6,
                    box("Article Network", width = "100%", 
                        uiOutput("network_filter"),
                        plotOutput("network") %>% withSpinner(color="#0dc5c1")
                    ),
                    box("Word Cloud (community)", width = "100%",
                        wordcloud2Output("network_wc") %>% withSpinner(color="#0dc5c1")
                        )
                    ),
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
        tabPanel("Topic Model",
            fluidRow(width = 12,
                column(6,
                      box("Inputs", 
                        fluidRow(width = 6, 
                                 sliderInput("topcs", "# of topics",  min = 2, max = 20, value = 5),
                                 sliderInput("ngram", label = "# words (ngrams)" , min = 1, max = 4, value = 2),
                                 checkboxInput("stem", "Stem Words", value = T),
                                 checkboxInput("clean", "Keep only alphabets", value = T),
                                 textInput("stopwords", "Stop Words", "Enter words to exclude")
                                 ),
                        fluidRow(width = 6,
                          actionButton("generate_tm", "Generate Topic Model", icon = icon("table"))
                        )
                      )
                ),
                column(6,
                  box("Top Terms", width = "100%",
                      div(dataTableOutput("tdm_stat"), style = "font-size:80%;")),
                  
                  
                )
            ),
            fluidRow(width = 12,
                     box("Top Terms in Topics", width = "100",
                         plotOutput("topic_plot")
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