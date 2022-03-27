#'@description Generate a trend chart for # publications and citations
#'@param df Data frame (expected to be already filtered)
#'@param include_total_citations by default average citiation is used for spline chart
#'@return high chart representing trend 
plot_publication_trend <- function(df, include_total_citations = T){
  out <- tryCatch({
    df_summary <- df %>% group_by(year) %>% 
      summarise(n_pubs = n(), tc = sum(citations)) %>%
      mutate(avg_cited = round(tc/n_pubs, 0))
    
    
    #print(df_summary)
    hc <- highchart() %>%
      hc_title(text = "Literature Trends") %>%
      hc_legend(enabled = T) %>%
      hc_yAxis_multiples(list(title = list(text = "Number of literature"), opposite=FALSE, min = 0),
                         list(title = list(text = "Number of citations"), opposite=TRUE, min = 0)) 
    
    if(!include_total_citations) {
      hc <- hc %>% hc_add_series(data = df_summary, mapping = hcaes(x = year, y = avg_cited), 
                                 name = "Average citations", type = 'spline', color = "grey", yAxis = 1)  #,
      #hc <- hc %>% hc_add_series(data = df_summary, mapping = hcaes(x = year, y = n_pubs, size = tc), 
      #                           name = "#puiblications", type = 'scatter', color = "lightblue", alpha = 0.5,
      #                           tooltip = list(pointFormat = "{point.year}: {point.n_pubs} literatures with {point.tc} citations")) 
      hc <- hc %>% hc_add_series(data = df_summary, mapping = hcaes(x = year, y = n_pubs), 
                                 name = "#puiblications", type = 'column', color = "lightblue", alpha = 0.5) 
      
    } else {
      hc <- hc %>% hc_add_series(data = df_summary, mapping = hcaes(x = year, y = n_pubs), 
                                 name = "#puiblications", type = 'column', color = "lightblue", alpha = 0.5) 
      hc <- hc %>% hc_add_series(data = df_summary, mapping = hcaes(x = year, y = tc), 
                                 name = "total citations", type = 'spline', color = "orange", yAxis = 1) 
    }
    
    hc <- hc %>% hc_tooltip(crosshairs = T)
    
    hc
    
  },
  error = function(e){
    message("Error rendering trend chart: \n")
    message(e)
    NULL
  })
}

#'@description Generate a treemap chart showing distribution of articles
#'@param df either full or filtered dataframe
#'@param limit_data how many categories do we want to use
gen_tree_map_chart <- function(df, category, limit_data = 25){
  tryCatch({
    x <- rlang::enquo(category)
    req(df)
    
    if(nrow(df) > 0) {
      df_summary <- df %>% 
        group_by_at(category) %>% 
        summarise(n = n(), s = sum(citations)) %>% 
        arrange(desc(s)) %>% head(limit_data)
      
      hchart(df_summary, "treemap", hcaes(x = !!category, value = n, color = s)) %>%
        hc_exporting(enabled = TRUE)
    }
  },
  error = function(e){
    message("Error rendering tree-map chart: \n")
    message(e)
    NULL
  })
}

#'@description Generate a wordcloud for the dataframe
#'@param df dataframe of the data file
#'@param type full_text or keywords, by default keywords
#'@return wordcloud2 object
plot_wordcloud <- function(df, type = "raw", ngram = 2){
  tryCatch({
    if(type == "raw"){
      text_col = "raw_text"
      ngram = strtoi(ngram)
    } else if (type == "clean"){
      text_col = "clean_text"
      ngram = strtoi(ngram)
    } else if (type == "abstract") {
      text_col = "abstract"
      ngram = strtoi(ngram)
    } else if (type == "title") {
      text_col = "title"
      ngram = strtoi(ngram)
    }
    else {
      text_col = "formated_keywords"
      ngram = 1
    }
    
    #withProgress(message = "Generating wordcloud, please wait ...", {
    wc_data <- df %>%
              select(doc_id, text = sym(text_col)) %>%
              unnest_tokens(word, text) %>%
              anti_join(stop_words) %>%
              group_by(doc_id) %>%
              summarise(text = paste0(word, collapse = " ")) %>%
              unnest_tokens(word, text, token = "ngrams", n = ngram) %>%
              filter(!is.na(word)) %>%
              group_by(word) %>%
              summarise(freq = n(), .groups = "drop") %>%
              arrange(desc(freq)) %>% 
              as.data.frame()
    
    wc <- wordcloud2(data = wc_data %>% head(500), size = 2)

    wc
    
  }, error = function(e){
    message("Error has occurred rendering wordcloud for literature data\n")
    message(e)
    NULL
  })
}

#'@description Generate summary infomation boxes
#'@param df dataframe of the data file
#'@return tagList object
gen_information_boxes <- function(df){
  df_summary <- df %>% 
    summarise(n = n(), total_cited = sum(citations), num_publications = n_distinct(publication),
              num_publishers = n_distinct(publisher)) %>% 
    mutate(average_cited = round(total_cited / n, 0))
  
  total_articles <- valueBox(df_summary[['n']], paste0("# Articles (", df_summary[['num_publications']], " journals/conferences)"), color = "navy", icon = icon("file", lib = "glyphicon"), width = 3)
  average_citations <- valueBox(df_summary[['average_cited']], "Avearge Citation", color = "teal",  icon = icon("record", lib = "glyphicon"), width = 3)
  
  # if(isTruthy(df_network)){
  #   network <- valueBox(length(V(df_network)), paste0("Connected Articles (", length(E(rv$g)), " connections)"), color = "green",  icon = icon("asterisk", lib = "glyphicon"), width = 3)
  # } else {
  #   network <- NA
  # }
  
  n_publishers <- valueBox(df_summary[['num_publishers']], "Publishers", color = "orange", icon = icon("folder-open", lib = "glyphicon"), width = 3)
  
  tagList(
    total_articles, average_citations, n_publishers
  )
}

# search data frame for data
#' @param df dataframe for the search to be applied 
#' @param type generate based on author or title
#' @return summary data frame
gen_impact_summary <- function(df, type = "author"){
  tryCatch({
    if(type == "author")
      grouping = "author"
    else
      grouping = type
    
    cat(file = stderr(), "ui_utils.R/gen_impact_summary: ", nrow(df), " rows, type = ", type, "\n")
    ret <- df %>%
      group_by_at(grouping) %>%
      dplyr::summarise(n_articles = n_distinct(doi), total_citations = sum(citations)) %>%
      arrange(desc(total_citations), desc(n_articles))
    
    if(type != "author")
      ret <- ret %>% select(-n_articles)
    
    ret
  }, error = function(e){
    cat(file = stderr(), "ui_utils.R/gen_impact_summary: ", "error caught" , "\n")
    print(e)
    browser()
  })
}

# 
# df %>% select(author, citations) %>% 
#   df %>%  filter(author == "[Anonymous]")
# 
# %>% 
#         arrange(desc(citations)) %>% 
#         mutate(cumsum_citations = cumsum(citations), g2 = row_number()^2)
# 
# df %>% filter(publication_type == "Journal") %>%
#   select(author, doi, title, citations) %>% 
#   group_by(author) %>%
#   summarise(n = n_distinct(doi)) %>%
#   arrange(desc(n)) %>% 
#   write.table(x, "clipboard", sep="\t", row.names=FALSE)
#   
#   datatable(class = 'cell-border stripe',
#                                  colnames = c('Author', 'Number of Papers'))
# 
# 

