library(shiny)

# generate filters based on data provided
gen_summary_boxes <- function(df, df_network = NA){
  df_summary <- df %>% 
    summarise(n = n(), total_cited = sum(cited_count), num_publications = n_distinct(publication),
              num_publishers = n_distinct(publisher)) %>% 
    mutate(average_cited = round(total_cited / n, 0))
  
  total_articles <- valueBox(df_summary[['n']], paste0("# Articles (", df_summary[['num_publications']], " journals/conferences)"), color = "navy", icon = icon("file", lib = "glyphicon"), width = 3)
  average_citations <- valueBox(df_summary[['average_cited']], "Avearge Citation", color = "teal",  icon = icon("record", lib = "glyphicon"), width = 3)
  
  if(isTruthy(df_network)){
    network <- valueBox(length(V(df_network)), paste0("Connected Articles (", length(E(rv$g)), " connections)"), color = "green",  icon = icon("asterisk", lib = "glyphicon"), width = 3)
  } else {
    network <- NA
  }
  
  n_publishers <- valueBox(df_summary[['num_publishers']], "Publishers", color = "orange", icon = icon("folder-open", lib = "glyphicon"), width = 3)
  
  tagList(
    total_articles, average_citations, network, n_publishers
  )
  
}