library(tidyverse)
library(rvest)
library(tidytext)
# process WoS (Web Of Science) data
process_data <- function(file_path) {
  read_tsv(file = file_path) %>%
    select(authors = AU, title = TI, publication = SO, conference = CT, keywords = DE, 
           keywordsp = ID, abstract = AB, research_id = RI, cited_reference_id = CR,
           cited_count = NR, publisher = PU, year = PY,
           research_area = SC, publication_type = PT)
}

get_wos_field_tags <- function() {
  url = 'https://images.webofknowledge.com/images/help/WOS/hs_wos_fieldtags.html'
  df <- url %>% 
        read_html() %>% 
        html_nodes('table') %>%
        html_table(fill = 'T')
  tibble(df[[2]]) %>% rename(Tag = X1, Desc = X2)
}

#df <- process_data('./data/savedrecs.tsv')
#wos <- get_wos_field_tags()

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
