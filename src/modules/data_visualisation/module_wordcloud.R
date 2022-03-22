######################################################
## module_wordcloud.R
## 
######################################################

library(highcharter)


#'@description Generate a wordcloud for the dataframe
#'@param df dataframe of the data file
#'@param type full_text or keywords, by default keywords
#'@return wordcloud2 object
plot_wordcloud <- function(df, type = "raw", ngram = 2){
  tryCatch({
    cat(file = stderr(), "Generating wordcloud: ", nrow, " rows,", ";type =", type, ";ngram = ", ngram, "\n")
    
    if(type == "raw"){
      text_col = "raw_text"
      ngram = strtoi(ngram)
    } else if (type == "clean"){
      text_col = "clean_text"
      ngram = strtoi(ngram)
    } else {
      text_col = "formated_keywords"
      ngram = 1
    }
    
    withProgress(message = "Generating wordcloud, please wait ...", {
      wc_data <- df %>%
        rowwise() %>%
        select(doc_id, text = sym(text_col)) %>%
        unnest_tokens(word, text, token = "ngrams", n = ngram) %>%
        group_by(word) %>%
        summarise(freq = n(), .groups = "drop") %>%
        ungroup() %>%
        arrange(desc(freq))
      
      wc <- wordcloud2(data = wc_data, minSize = 5)
    })
    
    wc
  }, error = function(e){
    message("Error has occurred rendering wordcloud for literature data", "\n")
    message(e)
    NULL
  })
}



# wordcloud ui
module_wordcloud_ui <- function(id){
  ns <- NS(id)

  tagList(
    box(title = "Word Cloud", width = "100%",
        radioButtons(inputId = ns("gs_wc_radio_type"), 
                     label = "Category",
                     choiceValues = c("raw", "clean", "kw"),
                     choiceNames = c("Raw Data", "Cleansed Data", "Keywords Only"),
                     selected = "clean", inline = T),
        selectInput(ns("gs_wc_ngrams"), label = "# Words", choices = c(1, 2, 3, 4), selected = 2),
        wordcloud2Output(ns("wordcloud"))
    ) %>% withSpinner(color="#0dc5c1")
  )
  
}

# wordcloud server
module_wordcloud_server <- function(id, data){
  moduleServer(
    id,
    function(input, output, session) {
      output$wordcloud <- renderWordcloud2({
        req(data())
        plot_wordcloud(data(), type = input$gs_wc_radio_type, ngram = input$gs_wc_ngrams)
      })
    }
  )
}