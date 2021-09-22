

library(word2vec)
source('./src/data_processing.R')

set.seed(1234)


df <- process_data('./data/savedrecs.tsv')
df <- df %>% mutate(text = tolower(abstract))

model <- word2vec(x = df$text, type = "cbow", dim = 25, iter = 20)

embedding <- as.matrix(model)
embedding


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

library(udpipe)
x <- udpipe_download_model(language = "english")
x$file_model

ud_english <- udpipe_load_model(x$file_model)

txt <- c('this is a good day', 'i pooed my pants')

x <- udpipe_annotate(ud_english, x = df$text)
x <- as.data.frame(x)
str(x)
