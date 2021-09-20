library(shiny)
library(highcharter)
library(treemap)
library(viridisLite)
library(wordcloud2)

source('./src/data_processing.R')

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rv = reactiveValues()
  
  observeEvent(input$data_file, {
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Loading data...", value = 0)
    
    
    rv$data <- process_data(input$data_file$datapath) %>%
               drop_na(year)
    
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

  # renders the ui
  output$filters <- renderUI({
    if(isTruthy(ui_filter()))
      ui_filter()
    }
  )
  
  ui_filter <- reactive({
    
    if(isTruthy(rv$data)){
      
      publications <-  rv$publication_summary$publication
      publishers <- rv$publisher_summary$publisher
      research_areas <- rv$research_area_summary$research_area
      year_min <-  min(rv$data$year)
      year_max <- max(rv$data$year)
      publication_types <- df$publication_type %>% unique() %>% sort()
      
      
      tagList(
        selectInput(inputId = "publication_types", label = "Publication Type", choices = list(Journal = "J", Conference = "C", Others = "S"), 
                    selected = publication_types, multiple = T),
        selectInput(inputId = "research_areas", label = "Research area", choices = research_areas, multiple = T, selected = NULL),
        selectInput(inputId = "publications", label = "Publication", choices = publications, multiple = T, selected = NULL),
        selectInput(inputId = "publishers", label = "Publisher", choices = publishers, multiple = T, selected = NULL),
        sliderInput(inputId = "year", label = 'Year', min = year_min, max = year_max, value = c(year_min, year_max))
      )
    }
  })
  
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
  
  output$data_table <- renderDataTable({
    df <- df_display()
    if(isTruthy(df)){
        df %>% select(year, publication_type, title, cited_count, abstract, keywords, keywordsp) %>% arrange(year)
    }
  })
  
  # display a trend chart
  output$trend_chart<- renderHighchart({
    if(isTruthy(df_display())){
      df <- df_display()
      df_summary <- df %>% group_by(year) %>% 
        summarise(n_pubs = n(), tc = sum(cited_count)) %>%
        mutate(avg_cited = tc/n_pubs)
      
      
      print(df_summary)
      hc <- highchart() %>%
        hc_title(text = "Publication Trends") %>%
        hc_legend(enabled = T) %>%
        #hc_xAxis(title = "Year", categories = df_summary$year, crosshair = T) %>%
        hc_yAxis_multiples(list(title = list(text = "# Publications"), opposite=FALSE),
                           list(title = list(text = "Total Citations"), opposite=TRUE)) %>% 
        
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
  
  
  #wordcloud
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
}

