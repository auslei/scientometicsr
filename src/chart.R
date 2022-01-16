library(highcharter)
library(igraph)
library(ggplot2)
library(ggraph)


# plot network using default plot function
plot_net_work <- function(g) {
  
  wc <- walktrap.community(g) # random walk performed to identify optimal clusters
  print(paste(length(wc), "communities found.."))
  
  max_degree <- max(V(g)$degree)
  max_page_rank <- max(V(g)$page_rank)
  max_eigen <- max(V(g)$eigen)
  max_betweeness <- max(V(g)$betweeness)
  
  #print(max_degree)
  #print(max_page_rank)
  #print(max_eigen)
  #print(max_betweeness)
  
  plot(wc, g, 
       vertex.label = ifelse(degree(g, mode = "out") >= 1, str_wrap(V(g)$alias, 25), NA),
       edge.arrow.size=.2, 
       vertex.size=ifelse(V(g)$degree == max_degree, 20, 5),
       vertex.col = ifelse(V(g)$degree == max_degree, "black", "green"),
       mark.border="black",
       edge.curved = T)
  #vertex.label=NA,
  #mark.groups = members,
  #vertex.size = V(g)$cited_count,
  #mark.border="black", 
  #mark.col=col_vector,
  #vertex.label.cex = 0.7,
  #vertex.size = V(g)$degree,
  #edge.arrow.size=.2, edge.curved = T, 
  #asp = 0.5, #aspect ratio
  #layout=layout.fruchterman.reingold)
  
}

plot_d3_forced <- function(g){
  
  
  #d3_g <- igraph_to_networkD3(g, members, "both")
  nodes <- g %>% vertex_attr() %>% as.data.frame() %>% mutate(id = alias - 1)
  edges <- g %>% as_data_frame() %>% mutate(from = match(from, nodes$name) - 1, to = match(to, nodes$name) -1)
  
  grp_summary <- nodes %>% 
    group_by(group) %>% mutate(top_count = max(cited_count)) %>% 
    filter(cited_count == top_count) 
  
  
  forceNetwork(Links = edges, Nodes = nodes, Source = 'from',
               Target = 'to', NodeID = 'id', Group = 'group',
               zoom = TRUE, fontSize = 12, arrows = F)
  
}


plot_ggraph <- function(g){
  ggraph(g, layout = "fr") +
    #geom_edge_elbow() +
    geom_edge_arc(strength = 0.2, width = 0.5, alpha = 0.15) + 
    geom_node_point(aes(color = factor(group), size = size)) + 
    geom_node_text(aes(label = alias, size = degree), repel = TRUE) +
    theme_void() +
    
    theme(legend.position = "none") 
}

#' param 
plot_publication_trend <- function(df, include_total_citations = T){
  df_summary <- df %>% group_by(year) %>% 
    summarise(n_pubs = n(), tc = sum(cited_count)) %>%
    mutate(avg_cited = round(tc/n_pubs, 0))
  
  
  #print(df_summary)
  hc <- highchart() %>%
    hc_title(text = "Literature Trends") %>%
    hc_legend(enabled = T) %>%
    hc_yAxis_multiples(list(title = list(text = "Number of literature"), opposite=FALSE, min = 0),
                       list(title = list(text = "Number of citations"), opposite=TRUE, min = 0)) 
    
  #hc_add_series(data = df_summary, mapping = hcaes(x = year, y = n_pubs), name = "# Publications", type = 'column', color = "#FF9500") %>% #,
  #tooltip = list(pointFormat = "<span style='color:{FF9500}'>\u25CF</span> # Publications {point.n_pubs} </span>")) %>%
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
}

