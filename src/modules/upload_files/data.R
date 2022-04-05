library(tidyverse)
library(rvest)
library(tidytext)
library(igraph)
library(networkD3)
library(tools)
library(SnowballC)
library(word2vec)
library(tokenizers)

# custom stop-words
customer_sw <- c('automated', 'automation', 'building', 'construction', 'code', 'compliance', 'machine', 'learning', 'na')

# cleanse, remove stop-words and stem
text_cleanse <- function(raw_text, stopwords = c(get_stopwords()$word)) {
  text_cleansed <- gsub("[^a-z\\s]", " ", tolower(raw_text))
  text_cleansed <-  paste0(tokenize_word_stems(text_cleansed, stopwords = stopwords)[[1]], collapse = " ")
  return(text_cleansed)
}

#'@description keywords are separated by semi columns, this function ensures keywords are treated as one word
#'@param text text to be treated
#'@return treated keywords
format_keywords <- function(text, sep = ";"){
  tryCatch({
    if(!is.null(text)){
      splitted <- str_split(text, pattern = sep)
      splitted[[1]] %>% 
        trimws() %>% 
        str_replace_all(" ", "_") %>% 
        paste(collapse = " ") %>%
        tolower()
    }
  }, error = function(e) {
    message("Error occurred formatting string")
    message(e)
  })
}


# process WoS (Web Of Science) data
#' @param  file_path path to the tsv WoS file
#' @return data frame with all data for the specific file
read_wos_data_file <- function(file_path) {
  
  read_tsv(file = file_path) %>%
    select(authors = AU, researchId = RI, title = TI, abstract = AB, publication = SO, conference = CT, keywords = DE, 
           keywordsp = ID, researcher_id = RI, doi = DI, book_doi = D2, cited_references = CR,
           citations = Z9, references = NR, publisher = PU, ISSN = SN, ISBN = BN, year = PY, wos_category = WC,
           research_area = SC, publication_type = PT) %>% 
    replace_na(list(abstract = "", keywords = "", keywordsp = "")) %>%
    rowwise() %>%
    mutate(author = str_split(authors, ";")[[1]][1],
           raw_text = tolower(paste(abstract, keywords, keywordsp)),
           clean_text = text_cleanse(raw_text),
           formated_keywords = format_keywords(paste(keywords, keywordsp, sep = ";")),
           publication_type = ifelse(publication_type == "J", "Journal",
                                     ifelse(publication_type == "B", "Book",
                                            ifelse(publication_type == "S", "Series",
                                                   ifelse(publication_type == "P", "Patent",
                                                          ifelse(publication_type == "C", "Conference",
                                                                 publication_type)))))
    ) %>% 
    ungroup() %>%
    mutate(doc_id = row_number())
  
    # combine all text you can find
}

#'@description Read data file from a literature indexing service
#'@param data_path Data file path (full path)
#'@param df Data frame to concatenate to, this is used to handle directory
#'@param type Source of the data file, at the moment only wos tsv is supported
read_data_file <- function(data_path, df = NULL, type = "wos"){
  out <- tryCatch(
    {
      if (type == "wos") {
        out <- read_wos_data_file(data_path)
      } else {
        e <- simpleError(paste0(type, "is currently not supported")) #throw an error if the method is not supported
        stop(e)
      }
      
      if(!is.null(df)){
        out <- df %>% bind_rows(out) #concatenate with existing data
      }
      
      out
    },
    
    error = function(e){
      message("Data file loading error (refer to below error): \n")
      message(paste("File Path:", data_path, "\n"))
      message(e)
      message("\n")
      NULL
    }
  )
}


# process data from multitple files
#' @param file_path the directory where files are situated
#' @param file_ext the file extension of the data files
#' @return data frame
read_wos_data_files <- function(file_path, file_ext = 'txt'){
  df <- NULL
  if(dir.exists(file_path)){
    for(f in list.files(file_path)){
      full_path = file.path(file_path, f)
      if(file_test("-f", full_path) && file_ext(full_path) == file_ext){
        if(!is.null(df)){
          print(paste0("loading file: ", full_path))
          t <- read_wos_data_file(full_path)
          #print(t %>% head())
          df <- df %>% bind_rows(t)
        }
        else
          df <- read_wos_data_file(full_path)
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


extract_split <- function(text, sep = ";", pattern = "10\\.\\w+\\/[^\\]]+"){
  text %>% 
    str_split(pattern = ";") %>%
    lapply(function(x){str_extract(x, pattern)})
}



extract_linkages <- function(df) {
  
  author_pat <- "^([a-zA-Z ]+.), \\d{4}"
  doi_pat <- "10\\.\\w+\\/[^\\]]+"
  
  ret <- df %>% filter(publication_type == "Journal") %>%
    select(doi, author, title, cited_references) %>% 
    mutate(referenced_doi = extract_split(cited_references),
           referenced_author = extract_split(cited_references, pattern = author_pat)) %>%
    unnest(cols = c(referenced_doi, referenced_author)) %>%
    select(doi, author, referenced_doi, referenced_author) %>%
    rowwise() %>%
    mutate(author = gsub(",", "", author), 
           referenced_autor = gsub("\\.", "", trimws(str_split(referenced_author, ',')[[1]][1])))
  
  ret
  
}


#'@description generate edge
#'@param df dataframe
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



#'@description calculate a score for each of the author, NA if the year of the author is unknow, possible due to that the artcile is yet published
#'@param df dataframe containing WOS data
#'@param this_year the year to calculate time
#'@param alpha coeffient to dimish contribution score
#'@retun dataframe containing authors and scores.
get_author_score <- function(df, this_year = 2022, alpha = 0.9){
  df %>% filter(!is.na(year)) %>% 
    mutate(s = alpha^(this_year-year) * citations) %>% 
    group_by(author) %>% summarise(score = floor(sum(s)), total_citation = sum(citations)) %>% 
    arrange(desc(score))
}



proc <- function(text){
  text %>% 
    str_split(pattern = ",") %>%
    unlist() %>%
    sapply(function(x){
            if(x %in% c("a", "b")) 
              return(T) 
            else 
              return(F)
          })
}
