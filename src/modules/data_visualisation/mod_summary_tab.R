######################################################
## Model to generate information boxes
## 
######################################################

# Dependency
library(shiny)
library(highcharter)

source("./src/modules/data_visualisation/ui_utils.R")


mod_data_summary_tab_ui <- function(id, data){
  ns <- NS(id)
  
  tabPanel("General Stats", width = "100%",
    fluidRow(width = 12,
      column(width = 6,
        uiOutput(ns("trend_chart_block")),
      ),
      column(width = 6,
        uiOutput(ns("tree_chart_block"))
      )
    ),
    
    fluidRow(width = 12,
      column(width = 6,
        uiOutput(ns("impact_analysis"))
      ),
      column(width = 6, 
        uiOutput(ns("wordcloud_ui"))
      ) #column
    ) #fluidRow
  )
  
}

mod_data_summary_tab_server <- function(id, data){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      # impact analysis screen
      output$impact_analysis <- renderUI({
        req(data())
        box(title = "Impact Analysis", width = "100%",
            radioButtons(inputId = ns("radio_author"), label = "Author/Article",
                         choices = c("Author" = "author", "Article" = "title"), selected = "author",
                         inline = T),
            dataTableOutput(ns("impact_table"))
        )
      })
      
      # populate impact table
      output$impact_table <- renderDataTable({
        req(data())
        req(input$radio_author)
        gen_impact_summary(data(), input$radio_author)
      }, options = list(pageLength = 10))
      
      # trend chart block
      output$trend_chart_block <- renderUI({
        req(data())
        box(title = "Trending Chart", width = "100%", 
            checkboxInput(ns("gs_chk_total_citation"), "Show total citations"),
            highchartOutput(ns("trend_chart"))
        )
      })
      
      # tree chart block
      output$tree_chart_block <- renderUI({
        req(data())
        box(title = "Journals/Conferences", width = "100%", 
            radioButtons(inputId = ns("gs_radio_type"), 
                         label = "Category",
                         choiceValues = c("publication", "publication_type"),
                         choiceNames = c("Publication", "Publication Type"),
                         selected = "publication", inline = T),
            highchartOutput(ns("treemap"))
        )
      })
      
      output$wordcloud_ui <- renderUI({
        req(data())
        tagList(
          box(title = "Word Cloud", width = "100%",
              radioButtons(inputId = ns("gs_wc_radio_type"), 
                           label = "Category",
                           choiceValues = c("raw", "clean", "kw"),
                           choiceNames = c("Raw Data", "Cleansed Data", "Keywords Only"),
                           selected = "clean", inline = T),
              selectInput(ns("gs_wc_ngrams"), label = "# Words", choices = c(1, 2, 3, 4), selected = 2),
              actionButton(ns("btn_gen_wordcloud"), label = "Generate"),
              wordcloud2Output(ns("wordcloud"))
          ) 
        )
      })
      
      
      #display a trend chart
      output$trend_chart<- renderHighchart({
       req(data())
       plot_publication_trend(data(), input$gs_chk_total_citation)
      })
      
      #display a trend chart
      output$treemap<- renderHighchart({
        req(data())
        gen_tree_map_chart(data(), input$gs_radio_type)
      })
      
      # display wordcloud2
      observeEvent(input$btn_gen_wordcloud, {
        output$wordcloud <- renderWordcloud2({
          req(data())
          plot_wordcloud(data(), type = input$gs_wc_radio_type, ngram = input$gs_wc_ngrams)
        })
      })
    }
  )
}
