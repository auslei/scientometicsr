source('./src/modules/data_visualisation/ui_utils.R')


#'@description Generate reference summary from a data frame, expanding author + doi article references
#'@param df either full or filtered dataframe
#'@return reference summary
generate_reference_table <- function(df){
  
  author_pat <- "^([a-zA-Z ]+.), \\d{4}"
  doi_pat <- "10\\.\\w+\\/[^\\]]+"
  
  ret <- df %>% filter(publication_type == "Journal") %>%
    #filter(!is.na(year)) %>%
    select(doi, author, year, title, cited_references) %>%
    mutate(referenced_doi = extract_split(cited_references),
           referenced_author = extract_split(cited_references, pattern = author_pat)) %>%
    unnest(cols = c(referenced_doi, referenced_author)) %>%
    select(year, title, doi, author, referenced_doi, referenced_author) %>%
    rowwise() %>%
    mutate(author = gsub(",", "", author),
           referenced_author = gsub("\\.", "", trimws(str_split(referenced_author, ',')[[1]][1]))) #%>%
    #filter(!(is.na(referenced_doi) && is.na(referenced_author)))
  
  ret
}

#'@description Generate author information based on authors and referenced authros 
#'@param df either full or filtered dataframe
#'@return author information
get_all_authors <- function(df){
  author_pat <- "^([a-zA-Z ]+.), \\d{4}"
  
  # get all references (this includes doi)
  df_ref <- generate_reference_table(df)
  
  # generate summary for authors
  df_au_summary <- gen_impact_summary(df, "author") %>%
    mutate(author = gsub(",", "", author))
  
  # generate summary for referenced authors
  df_ref_au_summary <- df_ref %>% 
    filter(!is.na(referenced_author)) %>%
    group_by(referenced_author) %>% 
    summarise(total_reference_citations = n_distinct(doi)) %>%
    rename(author = referenced_author) %>%
    arrange(desc(total_reference_citations))
  
  cat(file = stderr())
  
  # combine both summary to generate author summary
  authors <- df_au_summary %>% 
    merge(df_ref_au_summary, by = "author" , all = T) %>%
    replace_na(replace = list(n_articles = 0, total_citations = 0, total_reference_citations = 1)) %>%
    rowwise() %>%
    mutate(total_citations = max(total_citations, total_reference_citations)) %>%
    select(author, total_citations) %>% arrange(desc(total_citations)) %>%
    ungroup() %>%
    mutate(id = author, label = author, value = total_citations) 
  
  authors
  
}


#'@description Generate doi information based on authors and referenced authros 
#'@param df either full or filtered dataframe
#'@return doi information
get_all_doi <- function(df){
  df_ref <- generate_reference_table(df)
  df_doi_summary <- gen_impact_summary(df, "doi")
  df_ref_doi_summary <- df_ref %>% 
                        filter(!is.na(referenced_doi)) %>%
                        group_by(referenced_doi) %>%
                        summarise(total_reference_citations = n_distinct(doi)) %>%
                        rename(doi = referenced_doi)
  
  dois <- df_doi_summary %>%
          merge(df_ref_doi_summary, by= "doi") %>%
          rowwise() %>%
          mutate(total_citations = max(total_citations, total_reference_citations)) %>%
          ungroup() %>%
          select(-total_reference_citations) %>% arrange(desc(total_citations))
}

# for a given df generate a visnetwork layout
generate_author_network <- function(df) {
  
  cat(file = stderr(), "network_util.R/generate_author_network: ", nrow(df), " rows", "\n")

  # get information of all authors (this are verticies)
  df_authors <- get_all_authors(df)
  df_ref <- generate_reference_table(df)
  # get connections from references (this are edges)
  df_au_connections <- df_ref %>% 
    filter(!is.na(author) && 
             !is.na(referenced_author)) %>% 
    select(author, referenced_author) %>% 
    unique()  %>%
    rename(from = author, to = referenced_author) %>% 
    mutate(arrows = "to") %>% 
    filter(from %in% authors$author)
  
  
  # generate a group based on connections and authors
  g <- graph_from_data_frame(df_au_connections, vertices = df_authors, directed = T) #directed graph
  
  print((V(g)$total_citations %>% summary())["Mean"])
  
  # remove verticies with smaller citations <=
  g2 <- induced_subgraph(g, V(g)[V(g)$total_citations >= 20]) 
  
  # randome walk to determing communities
  wc <- walktrap.community(g2) # random walk performed to identify optimal clusters
  print(paste(length(wc), "communities found.."))
  
  # get membership details
  keep = which(table(wc$membership) >1)
  
  #keep only verticies/edges in groups with at least 2 members
  g3 <- induced_subgraph(g2, V(g2)[wc$membership %in% keep])
    
  
  # https://arxiv.org/abs/physics/0512106
  # random walk performed to identify optimal clusters
  wc <- walktrap.community(g3) 
  cat(file = stderr(), length(wc), "communities found..", "\n")
  V(g3)$group = wc$membership
  V(g3)$title = V(g3)$label
  
  # generate nodes 
  nodes <- data.frame(id = V(g3)$label, 
                      label = V(g3)$label, 
                      value = V(g3)$value,
                      group = wc$membership,
                      title = V(g3)$label)
  
  edges <- as_edgelist(g3) %>% as.tibble()
  colnames(edges) = c("from", "to")
  edges <- as.data.frame(edges)
  
  visNetwork(nodes = nodes, edges = edges) %>%
    visIgraphLayout(layout = "layout_nicely", physics = T, smooth = T, type = "full") %>%
    visOptions(highlightNearest = list(enabled = T, hover = T), 
               nodesIdSelection = T)
}


# plot communit using visNetwork
plot_visnetwork <- function(g, wc = NULL, layout = "layout_nicely"){
  if(!is.null(wc)){
    nodes <- data.frame(id = V(g)$label, 
                        label = V(g)$label, 
                        value = V(g)$value,
                        group = wc$membership,
                        title = V(g)$label)
  } else {
    nodes <- data.frame(id = V(g)$label, 
                        label = V(g)$label, 
                        value = V(g)$value,
                        title = V(g)$label)   
  }
  edges <- as_edgelist(g) %>% as.tibble()
  colnames(edges) = c("from", "to")
  edges <- as.data.frame(edges)
  
  visNetwork(nodes = nodes, edges = edges) %>%
    visIgraphLayout(layout = layout, physics = F, smooth = T, type = "full") %>%
    visOptions(highlightNearest = list(enabled = T, hover = T), 
               nodesIdSelection = T)
}

plot_visnetwork(sub_g, sub_wc)

# plot a specific graph grooup
plot_community <- function(g, wc, index, layout = "layout_nicely") {
  sub_graph = induced_subgraph(g, V(g)[wc$membership == index])
  wc_louvain <- cluster_louvain(sub_graph)
  plot_visnetwork(sub_graph, wc = wc_louvain, layout = layout)
}

