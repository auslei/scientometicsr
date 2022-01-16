
#'@description generate generaal status tabPanel for ui
ui_gen_status_panel <- function(){
  tabPanel("General Stats", width = "100%",
    fluidRow(width = 12,
      column(width = 6,
        box(title = "Trending", width = "100%", 
            checkboxInput("gs_chk_total_citation", "Show total citations"),
            highchartOutput("trend_chart") %>% 
            withSpinner(color="#0dc5c1"))
      ),
      column(width = 6,
        box(title = "Journals/Conferences", width = "100%", 
            radioButtons(inputId = "gs_radio_type", 
                         label = "Category",
                         choiceValues = c("publication", "publication_type"),
                         choiceNames = c("Publication", "Publication Type"),
                         selected = "publication", inline = T),
            highchartOutput("treemap") %>% withSpinner(color="#0dc5c1")
        )
      )
    ),
    fluidRow(width = 12,
      column(width = 6, 
        box(title = "Word Cloud", width = "100%",
          radioButtons(inputId = "gs_wc_radio_type", 
                       label = "Category",
                       choiceValues = c("raw", "clean", "kw"),
                       choiceNames = c("Raw Data", "Cleansed Data", "Keywords Only"),
                       selected = "clean", inline = T),
          selectInput("gs_wc_ngrams", label = "# Words", choices = c(1, 2, 3, 4), selected = 2),
          wordcloud2Output("wordcloud")
        ) %>% withSpinner(color="#0dc5c1")
      )
    )
  )
}

#'@description generate network analysis tabPanel for ui
#'@return tabPanel
ui_gen_network_analysis <- function(){
  tabPanel("Network Analysis", 
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
  )
}


#'@description generate raw data tabPanel for ui
#'@return tabPanel
ui_gen_raw_data <- function(){
  tabPanel("Raw Data", 
    fluidRow(width = 12,
      column(width = 12,
        box(title = "Data Table" , width = "100%", div(dataTableOutput("data_table"), style = "font-size:80%;"))
      )
    )
  )
}