source('./src/data_processing.R')
source('./src/chart.R')
source('./src/topic_models.R')


df <- read_wos_data_file("./data/savedrecs-6.txt")

plot_publication_trend(df)

x <- df %>% filter(year < 2000)

dtm <- df %>% filter(year < 2000) %>% generate_dtm()

generate_dtm(x)
