library(tidyverse)
library(rvest)
library(tidytext)
library(igraph)
library(networkD3)
library(tools)
library(SnowballC)


# process WoS (Web Of Science) data
process_data <- function(file_path) {
  read_tsv(file = file_path) %>%
    select(authors = AU, title = TI, publication = SO, conference = CT, keywords = DE, 
           keywordsp = ID, abstract = AB, researcher_id = RI, digital_object_id = DI, cited_reference_id = CR,
           cited_count = NR, publisher = PU, year = PY, wos_category = WC,
           research_area = SC, publication_type = PT) %>% 
           filter(!is.na(abstract)) %>%
           mutate(text = gsub("[^a-z]", " ", tolower(paste(abstract, keywords, keywordsp))), doc_id = row_number()) # combine all text you can find
}

# process data from mulitple files
#' @param file_path the directory where files are situated
#' @param file_ext the file extension of the data files
#' @return data frame
load_data <- function(file_path, file_ext = 'txt'){
  df <- NULL
  if(dir.exists(file_path)){
    for(f in list.files(file_path)){
      full_path = file.path(file_path, f)
      if(file_test("-f", full_path) && file_ext(full_path) == file_ext){
        if(!is.null(df)){
          print(paste0("loading file: ", full_path))
          t <- process_data(full_path)
          #print(t %>% head())
          df <- df %>% bind_rows(t)
        }
        else
          df <- process_data(full_path)
      }
    }
  }
  
  return(df)
}


# this is to download web of science field definitions (for view purpose only)
get_wos_field_tags <- function() {
  url = 'https://images.webofknowledge.com/images/help/WOS/hs_wos_fieldtags.html'
  df <- url %>% 
        read_html() %>% 
        html_nodes('table') %>%
        html_table(fill = 'T')
  tibble(df[[2]]) %>% rename(Tag = X1, Desc = X2)
}


#wos <- get_wos_field_tags()
extract_doi <- function(df) {
  ret <- df %>% drop_na(digital_object_id) %>%
          separate_rows(cited_reference_id, sep = ";") %>%
          separate_rows(cited_reference_id, sep = ",") %>%
          #mutate(has_doi = grepl("DOI", cited_reference_id), raw_cited_reference_id = cited_reference_id) %>%
          #mutate(cited_digital_object_id = str_extract(cited_reference_id, "DOI \\d+\\.\\d+\\/.+")) %>%
          mutate(cited_doi = str_extract(cited_reference_id, "10\\.\\w+\\/[^\\]]+"))  %>%
          select(doi = digital_object_id, cited_doi, raw_cited_doi = cited_reference_id) %>%
          drop_na(cited_doi) %>%
          unique()
          #mutate(cited_digital_object_id = gsub("DOI ", "", cited_digital_object_id))

  ret
}


# generate graph based on the data frame passed in
generate_net_work <- function(df) {
  data <- df %>% filter(!is.na(df$digital_object_id))
            
  
  e <- data %>% 
    extract_doi() %>%
           filter(cited_doi %in% data$digital_object_id) %>%  # contained within selected articles (do not consider others)
           select(from = doi, to = cited_doi)   # node to node strength can be considered as 1, unless cross reference
  
  
  v <- as.data.frame(list(doi = unique(c(e$from, e$to)))) %>% #this won't be required if only consider articles in the list (can just use data table)
           left_join(data %>% select(doi = digital_object_id, title, cited_count) %>% unique()) %>%
           mutate(size = cited_count / 10, alias = row_number()) 

  #edges <- edges %>% mutate(edges, source = match(digital_object_id, nodes$digital_object_id), 
  #                       target = match (cited_digital_object_id, nodes$digital_object_id))
  
  g <- graph_from_data_frame(e, vertices = v) #directed graph

  wc <- walktrap.community(g) # random walk performed to identify optimal clusters
  print(paste(length(wc), "communities found.."))
  
  members <- membership(wc)
  
  library(RColorBrewer)
  
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  # addition vertex additional attributes
  V(g)$group = as.vector(members)
  V(g)$group_colour = as.vector(unlist(lapply(V(g)$group, function(x){col_vector[x]})))
  V(g)$betweeness = centr_betw(g)$res
  V(g)$degree= centr_degree(g, mode = "out")$res
  V(g)$eigen = centr_eigen(g)$vector
  V(g)$page_rank = page_rank(g)$vector %>% as.vector()
  
  return(g)
}  
 

get_term_freq <- function(df){
  bigrams <- df %>% 
    unnest_tokens(word, abstract) %>% 
    anti_join(stop_words) %>% select(title, word) %>%
    group_by(title) %>% 
    summarise(text = paste0(word, collapse = " ")) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    group_by(bigram) %>%
    summarise(freq = n()) %>% 
    arrange(desc(freq))
  words <- df %>% 
    unnest_tokens(word, abstract) %>% 
    anti_join(stop_words) %>%
    group_by(word) %>%
    summarise(freq = n()) %>%
    arrange(desc(freq))
  
}

# remove stopwords and stem text document
#' @param df dataframe containing text (expected doc_id and text column to exist)
#' @return df_cleansed, data frame containing doc_id and cleansed text
cleanse_text <- function(df){
  df_cleansed <- df %>% 
    unnest_tokens(word, text) %>% 
    anti_join(get_stopwords()) %>%
    mutate(word = wordStem(word)) %>%
    group_by(doc_id) %>%
    summarise(text_cleansed = paste0(word, collapse = " ")) 
  
  return(df_cleansed)
}

# search data frame for data
#' @param df dataframe for the search to be applied 
#' @param search_text string searching for a specific text (this will be converted into regexp)
#' @return filtered data frame
search <- function(df, search_text){
  words <- strsplit(search_text, " ")[[1]] %>% wordStem() %>% paste(collapse = '|')
  df %>% filter(grepl(words, text_cleansed))
}

