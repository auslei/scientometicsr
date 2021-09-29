library(topicmodels)
library(tidytext)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tm)
library(SnowballC)
library(tidyverse)
library(quanteda)
library(ldatuning)
library(textmineR)

# stopwords
customer_sw <- c('automated', 'automation', 'building', 'construction', 'code', 'compliance',
                 'machine', 'learning', 'na')

sw <- c(stopwords(), customer_sw)


# pre-process text for modelling
preprocess_text <- function(df, sw = stopwords(), removeNonAlphabet = T,
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

generate_dtm <- function(df_clean, ngram_min = 1, ngram_max = 2){
  dtm <- CreateDtm(doc_vec = df_clean$text, 
                   doc_names = df_clean$doc_id, 
                   ngram_window = c(ngram_min, ngram_max),
                   verbose = TRUE)

  dtm <- dtm[,colSums(dtm) > 2] # get rid of any terms with a count lower than 2 across all documents
  dtm <- dtm[,colSums(dtm) <= nrow(dtm) * 0.8] # get rid of any terms that is contained in more than 80% of  all documents
  
  # get stats
  tf_mat <- TermDocFreq(dtm)
  
  # get ngrams
  tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]
  
  #
  print(head(tf_mat[order(tf_mat$term_freq, decreasing = TRUE) , ], 10))
  print(head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 10))

  return(dtm)  
}

lda_model <- function(dtm, k, iterations = 500){
  set.seed(12345)
  
  model <- FitLdaModel(dtm = dtm, 
                       k = 20,
                       iterations = 200, # I usually recommend at least 500 iterations or more
                       burnin = 180,
                       alpha = 0.1,
                       beta = 0.05,
                       optimize_alpha = TRUE,
                       calc_likelihood = TRUE,
                       calc_coherence = TRUE,
                       calc_r2 = TRUE,
                       cpus = 2) 

  #print(str(model)) # get an understanding of the structure
  
  return(model)
}




# dtm to dense data frame
dtm_to_dense_df <- function(tm){
  
  ret <- tidy(dtm) %>% spread(term, count)
  ret[is.na(ret)] = 0
  
  return(ret)
}


#dfm <- quanteda::dfm(df$text, verbose = FALSE)

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

#apply LDA
backup <- function(){
ap_lda <- LDA(dtm, k = 6, control = list(seed = 1234))
ap_lda


ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

doc_gamma <- tidy(ap_lda, matrix = "gamma")



ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

thefindFreqTerms(tm, 5)

getTransformations()

}


