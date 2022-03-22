library(shiny)

#'@description generate filters based on data changes
#'@param df literature data files
gen_navbar_filters <- function(df, ns){
  out <- tryCatch(
    {
      # if df is not null
      req(df)
      cat(file = stderr(), "module_data_filter.R/gen_navbar_filters: ", nrow(df), "\n")
      publications <-  df %>% group_by(publication) %>% summarise(n = n()) %>% arrange(desc(n)) %>% pull(publication)
      research_areas <- df %>% group_by(research_area) %>% summarise(n = n()) %>% arrange(desc(n)) %>% pull(research_area)
      year_min <-  min(df$year, na.rm = T)
      year_max <- max(df$year, na.rm = T)
      publication_types <- df$publication_type %>% unique() %>% sort()
      
      filters <- tagList(
        selectInput(inputId = ns("publication_types"), label = "Publication Type", choices = publication_types, multiple = T, selected = NULL),
        selectizeInput(inputId = ns("research_areas"), label = "Research area", choices = research_areas, multiple = T, selected = NULL),
        selectizeInput(inputId = ns("publications"), label = "Journal/Conference", choices = publications, multiple = T, selected = NULL),
        sliderInput(inputId = ns("year"), label = 'Year', min = year_min, max = year_max, value = c(year_min, year_max), sep = ""),
        div(
          p("Advanced Search:"),
          textInput(inputId = ns("search"), label = "Search Areas", value = NA),
          checkboxGroupInput(inputId = ns("chk_search_areas"), label = "Attribute to search", 
                             choiceNames = c("Publication", "Category", "Abstract", "Keywords", "Topic"),
                             choiceValues = c("publication", "wos_category", "abstract", "keywords", "raw_text"),
                             selected = c("publication", "wos_category")),
          checkboxInput(inputId = ns("chk_search_regex"), label = "Regular Expression", value = T),
          style = "background: rgba(255, 255, 255, .2); margin-top: 5%; margin-bottom: 5%; margin-left: 10%;"
        )
      )
      filters
    }, 
    error = function(e){
      cat(file = stderr(), "module_data_filter.R: Error occurred rendering filters:\n")
      message(e)
      NULL
    }
  )
}


# search data frame for data
#' @param df dataframe for the search to be applied 
#' @param search_text string searching for a specific text 
#' @param regex if using regex or just select based on matching text
#' @return filtered data frame
simple_search <- function(df, search_text, search_columns, regex = T){
  cat(file = stderr(), "module_data_filter.R/simple_search: ", search_text, "\n")
  if(regex){
    ret <- df %>% filter(str_detect(paste(!!!syms(search_columns)), regex(search_text, ignore_case = T)))
  } else {
    ret<- df %>% filter(tolower(search_text) %in% tolower(paste(!!!syms(search_columns))))
  } 
  
  if(nrow(ret) > 0)
    return(ret)
  else
    return(NULL)
}


#'@description data filter ui module
mod_data_filter_ui<- function(id, label = "Data Filters") {
  ns <- NS(id)
  tagList(
    uiOutput(ns('navbar_filter'))
  )
}

#'@description generate filters based on data changes
#'@param data literature data files
mod_data_filter_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues()
      
      # renders navigation bar filters
      output$navbar_filter <- renderUI({
        req(data())
        gen_navbar_filters(data(), session$ns)
      })
      
      # update filtered data frame
      filtered_data <- reactive({
        req(data())
        
        ret <- data()
        
        #cat(file = stderr(), "module_data_filter:observeEvent(filter data) ",
        #     nrow(rv$df), " rows are being filtered.", "\n")
         
         if(isTruthy(input$publication_types)){
           ret <- ret %>% filter(publication_type %in% input$publication_types)
         }
         
         if(isTruthy(input$research_areas)){
           ret <- ret %>% filter(research_area %in% input$research_areas)
         }
         
         if(isTruthy(input$publications)){
           ret <- ret %>% filter(publication %in% input$publications)
         }
       
         if(isTruthy(input$publishers)){
           ret <- ret %>% filter(publisher %in% input$publishers)
         }
         
         if(isTruthy(input$year)){
           ret <- ret %>% filter(year>=input$year[1] & year<=input$year[2])
         }
         
         if(isTruthy(input$search)){
           if(isTruthy(input$chk_search_areas))
             search_cols <- input$chk_search_areas
           else
             search_cols <- c("publication")
           
           search_result <- ret %>% simple_search(input$search,  
                                            search_columns = input$chk_search_areas, 
                                            regex = input$chk_search_regex)
           if(isTruthy(search_result)){
             showNotification(paste("Search results returned for: '", input$search, "'", " with ",
                                    nrow(search_result), "result."))
             ret <- search_result
           } else {
             showNotification("Advanced search did not return any result, using filter results only.")
           }
         }
        
         ret
       })
      
      
      return(filtered_data)
    } #function
    
  )
}
      