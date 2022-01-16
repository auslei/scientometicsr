
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
        box(title = "Journals/Conferences", width = "100%", highchartOutput("treemap") %>% withSpinner(color="#0dc5c1"))
      )
    ),
    fluidRow(width = 12,
      column(width = 6, 
        box(title = "Word Cloud", width = "100%", wordcloud2Output("wordcloud") %>% withSpinner(color="#0dc5c1"))
      )
    )
  )
}