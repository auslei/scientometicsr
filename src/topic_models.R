library(topicmodels)
library(tidytext)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tm)
library(SnowballC)



customer_sw <- c('automated', 'automation', 'building', 'construction', 'code', 'compliance',
                 'machine', 'learning')

docs <- Corpus(VectorSource(df$abstract))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, customer_sw)
docs <- tm_map(docs,stemDocument)

tm <- DocumentTermMatrix(docs)

ap_lda <- LDA(tm, k = 6, control = list(seed = 1234))
ap_lda


ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics


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

findFreqTerms(tm, 5)

getTransformations()
