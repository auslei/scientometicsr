library(shiny)
library(shinyjs)
library(shinydashboard)
library(highcharter)
library(shinycssloaders)
library(dashboardthemes)
library(shiny)
library(wordcloud2)
library(htmltools)

source('./src/modules/upload_files/module_upload_files.R')
source('./src/modules/data_filters/module_data_filter.R')
source('./src/modules/data_visualisation/mod_summary_tab.R')
source('./src/modules/network_analysis/mod_network_analysis.R')
#source("./src/modules/data_visualisation/module_wordcloud.R")
#source("./src/modules/data_visualisation/ui_utils.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  title = "ScientometricR",
  # Application title
  header = dashboardHeader(title = "ScientometicR"),
  # Sidebar with a slider input for number of bins
  sidebar = dashboardSidebar(
    mod_data_filter_ui("data_filter"),
    mod_upload_file_ui('upload_file')
  ),

  body = dashboardBody(
    useShinyjs(),
    includeScript(path = "www/custom.js"),
    #includeCSS(path = "www/custom.css"),
    
    fluidRow(uiOutput("infoboxes")),
    #fluidRow(module_wordcloud_ui("wordcloud")),
    fluidRow(
      tabBox(title = "Summary", width = 12,
        mod_data_summary_tab_ui("summary"),
        tabPanel("Raw Data", width = "100%",
          box(title = "Data Table" , width = "100%", div(dataTableOutput("data_table"), style = "font-size:80%;"))
        ),
        tabPanel("Network Analysis", width = "100%",
          mod_visnetwork_ui("network")
        )
      )
    )
  )
)