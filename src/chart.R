

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