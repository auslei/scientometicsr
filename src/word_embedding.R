library(word2vec)
source('./src/data_processing.R')
library(parallel)


set.seed(1234)


df <- process_data('./data/savedrecs.tsv')
df <- df %>% mutate(text = tolower(abstract))

model <- word2vec(x = df$text, type = "cbow", dim = 50, iter = 20, threads = detectCores())

embedding <- as.matrix(model)
embedding.pca <- prcomp(embedding)

pcs <- embedding.pca$x %>% as.data.frame() %>% head(50)

ggplot(pcs, aes(x=PC1, y=PC2)) +
  geom_text(label=rownames(pcs)) +
  geom_point(aes(size=5), alpha = 0.5, colour = 'blue') +
  theme(legend.position = "none") 


emb <- predict(model, c("compliance", "regulatory", "bim"), type = "embedding")
emb
lookslike <- predict(model, c("compliance", "regulatory", "bim"), type = "nearest", top_n = 5)
lookslike


library(uwot)
viz <- umap(embedding, n_neighbors = 15, n_threads = 2)
library(ggplot2)
library(ggrepel)
df  <- data.frame(word = gsub("//.+", "", rownames(embedding)), 
                  xpos = gsub(".+//", "", rownames(embedding)), 
                  x = viz[, 1], y = viz[, 2], 
                  stringsAsFactors = FALSE)
df  <- subset(df, xpos %in% c("JJ"))
ggplot(df, aes(x = x, y = y, label = word)) + 
  geom_text_repel() + theme_void() + 
  labs(title = "word2vec - adjectives in 2D using UMAP")


library(word2vec)
model <- read.word2vec(file = "cb_ns_500_10.w2v", normalize = TRUE)


# Speech tagging
library(udpipe)
x <- udpipe_download_model(language = "english")
x$file_model

model_location = "./english-ewt-ud-2.5-191206.udpipe"
ud_english <- udpipe_load_model(model_location)

txt <- c('this is a good day', 'i pooed my pants')

df_annote <- udpipe_annotate(ud_english, x = df$text)
df_annote <- as.data.frame(df_annote)
v_noun <- df_annote %>% filter(upos %in% "NOUN") %>% pull(token)

df_annote %>% filter(upos %in% "NOUN") %>%
              group_by(token) %>% 
              summarise(n = n()) %>% 
              arrange(desc(n)) %>% head(30)

# coocurrance 

library(tidytext)

df_tidy <- df %>% select (doc_id, title, text) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)  %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

  
df_tidy %>%
  count(word1, word2, sort = TRUE)


stats <- keywords_collocation(x = df_annote, 
                              term = "token", group = c("doc_id"),
                              ngram_max = 4)

## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(df_annote, upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id"))

## Co-occurrences: How frequent do words follow one another
stats2 <- cooccurrence(x = df_annote$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"))

## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats3 <- cooccurrence(x = df_annote$lemma, 
                      relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
head(stats)



library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(stats, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")



## Text rank
library(textrank)
stats4 <- textrank_keywords(df_annote$lemma, 
                           relevant = x$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 8, sep = " ")
stats4 <- subset(stats$keywords, ngram > 1 & freq >= 5)

library(wordcloud)
wordcloud(words = stats$keyword, freq = stats$freq)



#RAKE (Rapid Automatic Keyword)