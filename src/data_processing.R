library(tidyverse)
library(rvest)
library(tidytext)
library(igraph)
library(networkD3)

# process WoS (Web Of Science) data
process_data <- function(file_path) {
  read_tsv(file = file_path) %>%
    select(authors = AU, title = TI, publication = SO, conference = CT, keywords = DE, 
           keywordsp = ID, abstract = AB, researcher_id = RI, digital_object_id = DI, cited_reference_id = CR,
           cited_count = NR, publisher = PU, year = PY, wos_category = WC,
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


#wos <- get_wos_field_tags()
separate_citataions <- function(df) {
  ret <- df %>% drop_na(digital_object_id) %>%
          separate_rows(cited_reference_id, sep = ";") %>%
          mutate(has_doi = grepl("DOI", cited_reference_id), raw_cited_reference_id = cited_reference_id) %>%
          mutate(cited_digital_object_id = str_extract(cited_reference_id, "DOI \\d+\\.\\d+\\/.+")) %>%
          select(digital_object_id, cited_digital_object_id, cited_count, title, has_doi, raw_cited_reference_id) %>%
          #drop_na(cited_digital_object_id) %>%
          mutate(cited_digital_object_id = gsub("DOI ", "", cited_digital_object_id))

  ret
}


generate_net_work <- function(df) {
  
  data <- df %>% 
            filter(!is.na(df$digital_object_id)) %>% 
            unique()
  
  e <- data %>% 
           separate_citataions() %>%
           filter(cited_digital_object_id %in% data$digital_object_id) %>%  # contained within selected articles (do not consider others)
           select(from = digital_object_id, to = cited_digital_object_id)   # node to node strength can be considered as 1, unless cross reference
  
  
  v <- as.data.frame(list(id = unique(c(e$from, e$to)))) %>% #this won't be required if only consider articles in the list (can just use data table)
           left_join(data %>% select(id = digital_object_id, title, cited_count)) %>%
           mutate(size = cited_count / 10, alias = row_number()) 

  #edges <- edges %>% mutate(edges, source = match(digital_object_id, nodes$digital_object_id), 
  #                       target = match (cited_digital_object_id, nodes$digital_object_id))
  
  g <- graph_from_data_frame(e, vertices = v) #directed graph

  

  return(g)
}  
 
plot_net_work <- function(g) {
 
  #d3_g <- igraph_to_networkD3(g, members, "both")


  #grp_summary <- nodes %>% 
  #               group_by(group) %>% mutate(top_count = max(cited_count)) %>% 
  #               filter(cited_count == top_count) 

  
  #forceNetwork(Links = edges, Nodes = nodes, Source = 'source',
  #             Target = 'target', NodeID = 'title', Group = 'group',
  #             zoom = TRUE, linkDistance = 200)
  
  #g <- as.undirected(g, mode = "collapse")
  
  wc <- walktrap.community(g)
  # random walk performed to identify optimal clusters
  print(paste(length(wc), "clusters found.."))
  
  members <- membership(wc)
  
  library(RColorBrewer)
  
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  V(g)$group = as.vector(members)
  V(g)$color = as.vector(unlist(lapply(V(g)$group, function(x){col_vector[x]})))
  V(g)$degree = as.vector(degree(g, mode = "out"))
 
  
  plot(wc, g, 
       vertex.label = ifelse(degree(g, mode = "out") >= 1, str_wrap(V(g)$alias, 25), NA),
       edge.arrow.size=.2, edge.curved = T)
       #vertex.label=NA,
       #mark.groups = members,
       #vertex.size=5,
       #vertex.size = V(g)$cited_count,
       #mark.border="black", 
       #mark.col=col_vector,
       #vertex.label.cex = 0.7,
       #vertex.size = V(g)$degree,
       #edge.arrow.size=.2, edge.curved = T, 
       #asp = 0.5, #aspect ratio
       #layout=layout.fruchterman.reingold)
  
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



df <- process_data('./data/savedrecs.tsv')
g <- generate_net_work(df)

plot_net_work(g)
