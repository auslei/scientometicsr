library(tidytext)
library(reshape2)
library(ggplot2)
library(dplyr)
library(SnowballC)
library(tidyverse)
library(textmineR)


# pre-process text for modelling
preprocess_text <- function(df, sw = stop_words$word, removeNonAlphabet = T,
                                min_word_length = 3, 
                                stem = T){
  
  df_clean <- df %>% mutate(text = tolower(text))
    
  if(removeNonAlphabet){
    df_clean <- df_clean %>% mutate(text = gsub("[^a-z]", " ", text))
  }
  
  df_clean <- df %>% 
    mutate(doc_id = row_number()) %>%
    unnest_tokens(word, text) %>%
    anti_join(list(word = sw) %>% as.data.frame()) %>%
    filter(length(word) >= min_word_length)
  
  
  # do we want to steam?
  if(stem){
    df_clean <- df_clean %>% mutate(word = wordStem(word))
  }
  
  
  # join tokens back together
  df_clean <- df_clean %>%
              select(doc_id, word) %>%
              group_by(doc_id) %>% summarise(text = str_c(word, collapse = " ")) %>%
              ungroup()
}


# generate document term matrix (doc_id vs terms freq)
#' @param df_clean dataframe with cleansed data, require column text_cleansed
#' @param ngram_min minimal grams 
#' @param ngram_max maxiumum grams
#' @min_occ get rid of any terms below min occ
#' @max_occ get rid of any terms occurring too freq (default 50%)
#' @return document term matrix
generate_dtm <- function(df_clean, ngram_min = 1, ngram_max = 2, min_occ = 10, max_occ = 0.5){
  
  dtm <- CreateDtm(doc_vec = df_clean$clean_text, 
                   doc_names = df_clean$doc_id, 
                   ngram_window = c(ngram_min, ngram_max),
                   verbose = TRUE)

  dtm <- dtm[,colSums(dtm) > min_occ] # get rid of any terms with a count lower than 2 across all documents
  dtm <- dtm[,colSums(dtm) <= nrow(dtm) * max_occ] # get rid of any terms that is contained in more than 80% of  all documents
  
  # get stats
  tf_mat <- TermDocFreq(dtm)
  
  # get ngrams
  tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]
  
  #
  #print(head(tf_mat[order(tf_mat$term_freq, decreasing = TRUE) , ], 10))
  #print(head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 10))

  return(dtm)  
}


lda_model <- function(dtm, k, iterations = 500){
  set.seed(12345)
  
  model <- FitLdaModel(dtm = dtm, 
                       k = k,
                       iterations = iterations, # I usually recommend at least 500 iterations or more
                       burnin = 180,
                       alpha = 0.1,
                       beta = 0.05,
                       optimize_alpha = TRUE,
                       calc_likelihood = TRUE,
                       calc_coherence = TRUE,
                       calc_r2 = TRUE) 

  #print(str(model)) # get an understanding of the structure
  
  return(model)
}



# find best k using LDA
lda_model_optimise <- function(dtm, k_min = 1, k_max = 6, step = 1){
  k_list <- seq(k_min, k_max, by = step)
  
  model_list <- TmParallelApply(X = k_list, FUN = function(k){
    
    m <- FitLdaModel(dtm = dtm, 
                     k = k, 
                     iterations = 200, 
                     burnin = 180,
                     alpha = 0.1,
                     beta = colSums(dtm) / sum(dtm) * 100,
                     optimize_alpha = TRUE,
                     calc_likelihood = FALSE,
                     calc_coherence = TRUE,
                     calc_r2 = FALSE,
                     cpus = 1)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    m
  }, export= ls()) 
 
  model_list
}

# tokenise data
generate_tokens <- function(df) {
  data <- df %>% unnest_tokens(word, text) %>%
    anti_join(data.frame(word = c(stopwords(), customer_sw))) %>%
    group_by(title, word) %>%
    summarise(n = n()) %>%
    slice_max(n, n = 10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) 
}

# 
# df <-process_data('./data/savedrecs.tsv')
# df_clean <- preprocess_text(df)
# 
# dtm <- generate_dtm(df)
#   
# model <- lda_model(dtm)
# 
# ml <- lda_model_optimise(dtm)
# 
# 
# # Get average coherence for each model
# coherence_mat <- data.frame(k = sapply(ml, function(x) nrow(x$phi)), 
#                             coherence = sapply(ml, function(x) mean(x$coherence)), 
#                             stringsAsFactors = FALSE)
# 
# 
# # Plot the result
# # On larger (~1,000 or greater documents) corpora, you will usually get a clear peak
# plot(coherence_mat, type = "o")

