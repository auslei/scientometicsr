library(igraph)
library(visNetwork)
library(shiny)

source('./src/modules/network_analysis/util.R')

#'@description ui component of the network analysis module
mod_visnetwork_ui <- function(id){
  ns <- NS(id)
  cat(file = stderr(), "Network UI componenet", "\n")
  tagList({
    tabPanel("General Stats", width = "100%",
      uiOutput(ns("network_ui"))
    )
  })
}


#'@desciption take a filtered data element from wos and generate network 
#'@param id the id of the module
#'@param data filtered data
mod_visnetwork_server <- function(id, data){
    moduleServer(
      id,
      function(input, output, session) {
        ns <- NS(id)
        
        network_data <- reactive({
          req(data())
          generate_network(data())
        })
        
        output$network_ui <- renderUI({
          #browser()
          req(data())
          cat(file = stderr(), "mod_network_analysis.R/network_ui", "Rendering UI component", "\n")
          tagList(
            radioButtons(inputId = ns("rdo_author_doi"), label = "Author/DOI (article)",
                         choices = c("Author" = "author", "DOI (article)" = "doi"), 
                        inline = T),
            visNetworkOutput(outputId = ns("visnetwork"))
          )
        })
        
        # generate vis network
        output$visnetwork <- renderVisNetwork({
          cat(file = stderr(), "mod_network_analysis.R/visnetwork", "Generating network", "\n")
          req(network_data())
          nw <- network_data()
          plot_visnetwork(nw$g, nw$wc)
        })
      }
    )
}


# 
# # https://arxiv.org/abs/0803.0476 
# cLuo <- cluster_louvain(g)
# layout <- layout_with_fr(g)
# 
# ## identify which communities have fewer than 5 members
# Small = which(table(cLuo$membership) < 120)
# ## Which nodes should be kept?
# Keep = V(g)[!(cLuo$membership %in% Small)]
# 
# plot(cLuo, g, vertex.label = NA, vertex.size = 5, edge.arrow.size = .2, layout = layout)
# 
# 
# gD2  = induced_subgraph(g, Keep)
# lou2 = cluster_louvain(gD2)
# LO2 = layout[Keep,]
# plot(lou2, gD2, vertex.label = NA, vertex.size=5, 
#      edge.arrow.size = .2, layout=LO2)
# 
# 
# 
# df_ti_summary <- gen_impact_summary(df, "title")
# 
# extract_split <- function(text, sep = ";", pattern = "10\\.\\w+\\/[^\\]]+"){
#   text %>% 
#     str_split(pattern = ";") %>%
#     lapply(function(x){str_extract(x, pattern)})
# }
# 
# 
# 
# 
# 
# # generate graph based on the data frame passed in
# generate_net_work <- function(df) {
#   data <- df %>% filter(!is.na(digital_object_id))
#   
#   # prepare data
#   data <- df %>% filter(!is.na(digital_object_id))
#   
#   # generating edges basically a node to another node
#   e <- data %>% 
#     extract_doi() %>%
#     filter(cited_doi %in% data$digital_object_id) %>%  # contained within selected articles (do not consider others)
#     select(from = doi, to = cited_doi)   # node to node strength can be considered as 1, unless cross reference
#   
#   v <- as.data.frame(list(doi = unique(c(e$from, e$to)))) %>% #this won't be required if only consider articles in the list (can just use data table)
#     left_join(data %>% select(doi = digital_object_id, title, authors, cited_count) %>% unique()) %>%
#     mutate(size = cited_count, alias = row_number()) 
#   
#   #edges <- edges %>% mutate(edges, source = match(digital_object_id, nodes$digital_object_id), 
#   #                       target = match (cited_digital_object_id, nodes$digital_object_id))
#   
#   g <- graph_from_data_frame(e, vertices = v) #directed graph
#   
#   wc <- walktrap.community(g) # random walk performed to identify optimal clusters
#   print(paste(length(wc), "communities found.."))
#   
#   members <- membership(wc)
#   
#   library(RColorBrewer)
#   
#   qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
#   col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#   
#   # addition vertex additional attributes
#   V(g)$group = as.vector(members)
#   V(g)$group_colour = as.vector(unlist(lapply(V(g)$group, function(x){col_vector[x]})))
#   V(g)$betweeness = centr_betw(g)$res
#   V(g)$degree= centr_degree(g, mode = "out")$res
#   V(g)$eigen = centr_eigen(g)$vector
#   V(g)$page_rank = page_rank(g)$vector %>% as.vector()
# 
#   
#   return(g)
# }  
# 
# 
# 
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
       #vertex.label = ifelse(degree(g, mode = "out") >= 1,
        #                     str_wrap(V(g)$alias, 25), NA),
       edge.arrow.size = .2,
       vertex.size = ifelse(V(g)$total_citations/10),
       #vertex.col = ifelse(V(g)$degree == max_degree, "black", "green"),
       mark.border = "black",
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
# 
# 
# doi <- extract_doi(df)
# 
# 
# df %>% select (authors, title, cited_reference_id) %>% 
#         separate_rows(authors, sep = ";") %>%
#         separate_rows(cited_reference_id, sep = ",")


# layout = layout_with_fr(sub_g)
# plot(sub_wc, sub_g, 
#       vertex.label = NA, 
#       vertex.size = log(V(sub_g)$value), edge.arrow.size = .2)


