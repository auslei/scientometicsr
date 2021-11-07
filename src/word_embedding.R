library(word2vec)
source('./src/data_processing.R')
library(parallel)


set.seed(1234)


df <- process_data('./data/savedrecs.tsv')
df <- df %>% mutate(text = tolower(abstract))

model <- word2vec(x = df$text, type = "cbow", dim = 300, iter = 20, threads = detectCores())

embedding <- as.matrix(model)
embedding.pca <- prcomp(embedding)

pcs <- embedding.pca$x %>% as.data.frame() %>% head(50)

ggplot(pcs, aes(x=PC1, y=PC2)) +
  geom_text(label=rownames(pcs)) +
  geom_point(aes(size=5), alpha = 0.5, colour = 'blue') +
  theme(legend.position = "none") 


emb <- predict(model, c("the"), type = "embedding")
emb
lookslike <- predict(model, c("code"), type = "nearest", top_n = 5)
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
                           relevant = df_annote$upos %in% c("NOUN"), 
                           ngram_max = 8, sep = " ")
keywords <- subset(stats4$keywords, ngram > 1 & freq >= 5)

library(wordcloud)
wordcloud(words = keywords$keyword, freq = keywords$freq)



#RAKE (Rapid Automatic Keyword)
stats <- keywords_rake(x = df_annote, 
                      term = "token", group = c("doc_id"),
                      relevant = df_annote$upos %in% c("NOUN", "ADJ"),
                      ngram_max = 4)

head(subset(stats, freq > 3))


# phrases

# Next option is to extract phrases. These are defined as a sequence of Parts 
# of Speech Tags. Common type of phrases are noun phrases or verb phrases. 
# How does this work? Parts of Speech tags are recoded to one of the following 
# one-letters: (A: adjective, C: coordinating conjuction, D: determiner, 
# M: modifier of verb, N: noun or proper noun, P: preposition). Next you can 
#define a regular expression to indicate a sequence of parts of speech tags 
#which you want to extract from the text.

## Simple noun phrases (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
df_annote$phrase_tag <- as_phrasemachine(df_annote$upos, type = "upos")
stats <- keywords_phrases(x = df_annote$phrase_tag, term = df_annote$token, 
                          pattern = "(A|N)+N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, ngram_max = 4, detailed = FALSE)
head(subset(stats, ngram > 2))


#Option 6: Use dependency parsing output to get the nominal subject and the adjective of it
stats6 <- merge(df_annote, df_annote, 
               by.x = c("doc_id", "head_token_id"),
               by.y = c("doc_id", "token_id"),
               all.x = TRUE, all.y = FALSE, 
               suffixes = c("", "_parent"), sort = FALSE)
stats6 <- subset(stats6, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
stats6$term <- paste(stats6$lemma_parent, stats6$lemma, sep = " ")
stats6 <- txt_freq(stats6$term)
library(wordcloud)
wordcloud(words = stats6$key, freq = stats6$freq, min.freq = 3, max.words = 100,
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))

