library(shiny)
library(highcharter)
library(treemap)
library(viridisLite)
library(wordcloud2)


source('./src/data_processing.R')
source('./src/chart.R')
source('./src/topic_models.R')

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rv = reactiveValues()
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Loading default data...", value = 0)
  
  rv$data <- process_data("./data/savedrecs.tsv") %>%
    drop_na(year)
  
  
 
  
  observeEvent(input$data_file, {
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Loading data...", value = 0)
    
    
    rv$data <- process_data(input$data_file$datapath) %>%
               drop_na(year)
  })

  # generate global summarise
  observeEvent(rv$data, {
    rv$publication_summary <- rv$data %>% 
      group_by(publication) %>% 
      summarise(n_pubs = n(), n_citations = sum(cited_count)) %>% 
      arrange(desc(n_citations)) %>% 
      mutate(cite_pub_ration = n_citations / n_pubs)
    rv$publisher_summary <- rv$data %>% 
      group_by(publication) %>% 
      summarise(n_pubs = n(), n_citations = sum(cited_count)) %>% 
      arrange(desc(n_citations)) %>% 
      mutate(cite_pub_ration = n_citations / n_pubs)
    
    rv$research_area_summary <- rv$data %>% 
      group_by(research_area) %>% 
      summarise(n_pubs = n(), n_citations = sum(cited_count)) %>% 
      arrange(desc(n_citations)) %>% 
      mutate(cite_pub_ration = n_citations / n_pubs)
  })
  
  
  ## Information Boxes
  infoboxes <- reactive({
    
    if(isTruthy(df_display())){
      df_summary <- df_display() %>% 
            summarise(n = n(), total_cited = sum(cited_count), num_publications = n_distinct(publication),
            num_publishers = n_distinct(publisher)) %>% 
        mutate(average_cited = round(total_cited / n, 0))
      
      total_articles <- valueBox(df_summary[['n']], paste0("# Articles (", df_summary[['num_publications']], " journals/conferences)"), color = "navy", icon = icon("file", lib = "glyphicon"), width = 3)
      average_citations <- valueBox(df_summary[['average_cited']], "Avearge Citation", color = "teal",  icon = icon("record", lib = "glyphicon"), width = 3)
      
      if(isTruthy(rv$g)){
        network <- valueBox(length(V(rv$g)), paste0("Connected Articles (", length(E(rv$g)), " connections)"), color = "green",  icon = icon("asterisk", lib = "glyphicon"), width = 3)
      } else {
        network <- NA
      }
      
      n_publishers <- valueBox(df_summary[['num_publishers']], "Publishers", color = "orange", icon = icon("folder-open", lib = "glyphicon"), width = 3)
      
      tagList(
        total_articles, average_citations, network, n_publishers
      )
    }
    
  })
  
  output$infoboxes <- renderUI({
    infoboxes() 
  })
  
  ui_filter <- reactive({
    
    if(isTruthy(rv$data)){
      
      publications <-  rv$publication_summary$publication
      research_areas <- rv$research_area_summary$research_area
      year_min <-  min(rv$data$year)
      year_max <- max(rv$data$year)
      publication_types <- rv$data$publication_type %>% unique() %>% sort()
      
      tagList(
        checkboxGroupInput(inputId = "publication_types", label = "Publication Type", choices = list(Journal = "J", Conference = "C", Others = "S")),
        selectInput(inputId = "research_areas", label = "Research area", choices = research_areas, multiple = T, selected = NULL),
        selectInput(inputId = "publications", label = "Journal/Conference", choices = publications, multiple = T, selected = NULL),
        sliderInput(inputId = "year", label = 'Year', min = year_min, max = year_max, value = c(year_min, year_max))
      )
    }
  })
  
  
  # renders the ui
  output$filters <- renderUI({
    if(isTruthy(ui_filter()))
      ui_filter()
  }
  )
  
  
  # renders network filters
  output$network_filter <- renderUI({
    if(isTruthy(rv$g)){
      groups = V(rv$g)$group %>% unique()
      selectInput(inputId = "communities", label = "Communities", choices = groups, multiple = T, selected = NULL)
    }
  })
  
  
  # filter data based on navbar filters for display
  df_display <- reactive({
    
    if(isTruthy(rv$data)) {
        df <- rv$data
        
        if(isTruthy(input$publication_types)){
          df <- df %>% filter(publication_type %in% input$publication_types)
        }
        
        if(isTruthy(input$research_areas)){
          df <- df %>% filter(research_area %in% input$research_areas)
        }
        
      
        if(isTruthy(input$publications)){
          df <- df %>% filter(publication %in% input$publications)
        }
         
        if(isTruthy(input$publishers)){
           df <- df %>% filter(publisher %in% input$publishers)
        }
        
        if(isTruthy(input$year)){
          df <- df %>% filter(year>=input$year[1] & year<=input$year[2])
        }
      
        df
    }
  })
  
  
  # display a trend chart
  output$trend_chart<- renderHighchart({
    if(isTruthy(df_display())){
      df <- df_display()
      df_summary <- df %>% group_by(year) %>% 
        summarise(n_pubs = n(), tc = sum(cited_count)) %>%
        mutate(avg_cited = tc/n_pubs)
      
      
      #print(df_summary)
      hc <- highchart() %>%
        hc_title(text = "Publication Trends") %>%
        hc_legend(enabled = T) %>%
        #hc_xAxis(title = "Year", categories = df_summary$year, crosshair = T) %>%
        hc_yAxis_multiples(list(title = list(text = "# Publications"), opposite=FALSE, min = 0),
                           list(title = list(text = "Total Citations"), opposite=TRUE, min = 0)) %>% 
        
        #hc_add_series(data = df_summary, mapping = hcaes(x = year, y = n_pubs), name = "# Publications", type = 'column', color = "#FF9500") %>% #,
                      #tooltip = list(pointFormat = "<span style='color:{FF9500}'>\u25CF</span> # Publications {point.n_pubs} </span>")) %>%
        hc_add_series(data = df_summary, mapping = hcaes(x = year, y = avg_cited), name = "Average citations", type = 'line', color = "grey", yAxis = 1) %>% #,
                      #tooltip = list(pointFormat = "<span style='color:{#2670FF}'>\u25CF</span> Total Citations {point.tc} </span>")) %>%
        
        hc_add_series(data = df_summary, mapping = hcaes(x = year, y = n_pubs, size = tc), 
                      name = "#puiblications", type = 'scatter', color = "lightblue", alpha = 0.5) %>% #,
                     # tooltip = list(pointFormat = "<span style='color:{#00FF2A}'>\u25CF</span> Avg Citations {point.avg_cited}"), yAxis = 1) %>%
        #hc_add_series(name = "# Publications", type = 'column', data = df_summary$n_pubs, color = 'blue', yAxis = 0) %>%
        #hc_add_series(name = "Average Citations", type = 'line', data = df_summary$avg_cited, color = 'orange', yAxis = 1) %>%
        #hc_add_series(name = "Total Citations", type = 'line', data = df_summary$avg_cited, color = 'orange', yAxis = 1) %>%
        hc_tooltip(crosshairs = T, shared = TRUE, headerFormat = "<b>Year {point.key}</b><br>") %>%
        hc_add_theme(hc_theme_tufte())
      
      hc
      
    }
  })
  
  #display portion chart
  output$treemap <- renderHighchart({
    var <- c('publication')
    if(isTruthy(df_display())){
      df_summary <- df_display() %>% group_by(!!!syms(var)) %>% summarise(n = n(), s = sum(cited_count)) %>% arrange(desc(s)) %>% head(25)
      hchart(df_summary, "treemap", hcaes(x = publication, value = n, color = s)) 
    }
  })
  
  
  #wordcloud for filtered data
  output$wordcloud <- renderWordcloud2({
    if(isTruthy(df_display())){
      df <- df_display()
      bigrams <- df %>% 
        unnest_tokens(word, abstract) %>% 
        anti_join(stop_words) %>% select(title, word) %>%
        group_by(title) %>% 
        summarise(text = paste0(word, collapse = " ")) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        group_by(bigram) %>%
        summarise(freq = n()) %>% 
        arrange(desc(freq))
      wordcloud2(data = bigrams, minSize = 10)
    }
  })
  
  observeEvent(df_display(), {
    if(isTruthy(df_display()) && nrow(df_display())>0){
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Generating network data...", value = 0)
      
      #print(nrow(df_display()))
      
      rv$g <- generate_net_work(df_display())
      g <- rv$g
      
      # data frame representing the vertices
      rv$g_df <- as.data.frame(list(
                                    alias = V(rv$g)$alias,
                                    vertex = V(rv$g)$name, 
                                    title = V(rv$g)$title, 
                                    citation_count = V(rv$g)$cited_count,
                                    community = V(rv$g)$group, 
                                    degree = as.vector(degree(rv$g, mode ="out")),
                                    betweenness = centr_betw(rv$g)$res,
                                    eigen = centr_eigen(rv$g)$vector,
                                    page_rank = page_rank(rv$g)$vector %>% as.vector()
                                    ))
      
    }
  })
  
  
  #networkplot
  # output$network <- renderPlot({
  #   if(isTruthy(rv$g)){
  #     
  #     g <- rv$g
  #     
  #     # if selection is made, only plot a sub-graph
  #     if(isTruthy(input$communities)){
  #       g <- induced.subgraph(g, V(g)[V(g)$group %in% input$communities])
  #     }
  #     
  #     # generate networks
  #       
  #     plot_net_work(g)
  #   }
  #   
  # })
  
  #
  output$network <- renderPlot({
    g <- rv$g
    
    # if selection is made, only plot a sub-graph
    if(isTruthy(input$communities)){
      g <- induced.subgraph(g, V(g)[V(g)$group %in% input$communities])
    }
    
    # generate networks
    plot_ggraph(g)
  })
  
  ##########################################
  #               DATA TABLES              #
  ##########################################
  
  
  
  output$data_table <- renderDataTable({
    df <- df_display()
    if(isTruthy(df)){
      df %>% select(year, publication_type, title, cited_count, abstract, keywords, keywordsp) %>% arrange(year)
    }
  }, options = list(pageLength = 10, info = FALSE))
  
  
  output$dt_citation <- renderDataTable({
    if(isTruthy(rv$g_df)){
      df <- rv$g_df %>% mutate(vertex = paste0("<a href = 'https://www.doi.org/", vertex, "'>", vertex, "</a>"))
      
      if(isTruthy(input$communities)){
        df <- df %>% filter(community %in% input$communities)
      }
      
      df
    }
    
  }, escape = FALSE, options = list(dom = 't'))
  
  
  # display a citation summary table
  output$dt_citation_summary <- renderDataTable({
    if(isTruthy(rv$g_df)){
      df <- rv$g_df
      
      df %>% group_by(community) %>%
        summarise(num_articles = n(), total_citations = sum(citation_count)) %>%
        arrange(desc(num_articles))
      
    }
  }, options = list(dom = 't'))
  
  
  # render the wordcloud for selected communities
  output$network_wc <- renderWordcloud2({
    if(isTruthy(rv$g_df)){
      df <- rv$g_df
      
      if(isTruthy(input$communities)){
        df <- df %>% filter(community %in% input$communities)
      }
      
      df <- rv$data %>% filter(digital_object_id %in% df$vertex) %>% 
                select(keywords, keywordsp) %>% 
                mutate(text = paste(tolower(keywords), tolower(keywordsp), sep = ";")) %>% 
                separate_rows(text, sep = ';') %>% 
                mutate(text = gsub("[^a-z]", " ", trimws(text))) %>%
                group_by(text) %>% summarise(n=n()) %>% 
                arrange(desc(n))
      
      
      wordcloud2(df)
    }
    
    
  })
  
  
  observeEvent(input$generate_tm, {
    if(isTruthy(df_display())){
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Generating topic model, this may take some time...", value = 0)
      
      
      k <- input$topcs
      stem <- input$stem
      ngram <- input$ngram
      clean <- input$clean
      sw <- stop_words$word
      if(isTruthy(input$stopwords)){
        sw <- c(sw, str_split(tolower(input$stopwords), "[^a-z]")[[1]])
      }
      
      # Create a Progress object
      rv$df_clean <- preprocess_text(df_display(), sw = sw,  removeNonAlphabet = clean, stem = stem) #TODO: to implement customised stopwords
      rv$dtm <- generate_dtm(rv$df_clean, ngram_min = 1, ngram_max = ngram) #TODO: to make it tweakable
      
      # create lda model
      rv$lda_model <- lda_model(dtm = rv$dtm, k = k)
      
      rv$df_clean$topic <- rv$lda_model$theta %>% max.col()
    }
  })
  
  # display frequent terms
  output$tdm_stat <- renderDataTable({
    if(isTruthy(rv$dtm)){
      tf <- TermDocFreq(rv$dtm) %>% 
              arrange(desc(term_freq))
      
      tf
    }
  }, options = list(pageLength = 10))
  
  
  output$topic_plot <- renderPlot({
    if(isTruthy(rv$lda_model)){
      model <- rv$lda_model
      term <- model$phi %>% as.matrix() %>%
              t() %>% 
              as.data.frame() %>% 
              rownames_to_column("term") %>% gather(key = "topic", value = "phi", -term)
      
      top_terms <- term %>% 
                    group_by(topic) %>%
                    top_n(15, phi) %>% 
                    ungroup() %>%
                    arrange(topic, -phi)
      
      top_terms %>%
        mutate(term = reorder_within(term, phi, topic)) %>%
        ggplot(aes(term, phi, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()
    }
  })
}

