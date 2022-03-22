source('./src/data_processing.R')
source('./src/chart.R')
source('./src/topic_models.R')


df <- read_wos_data_file("./data/savedrecs-6.txt")

plot_publication_trend(df)

x <- df %>% filter(year < 2000)

dtm <- df %>% filter(year < 2000) %>% generate_dtm()

generate_dtm(x)


df_summary <- df %>% select(doc_id, wos_category) %>% 
                  separate_rows(wos_category, sep = ";") %>% 
                  mutate(wos_category = trimws(wos_category)) %>% 
                  group_by(wos_category) %>% 
                  summarise(n = n_distinct(doc_id)) %>% 
                  arrange(desc(n))

hchart(df_summary, "treemap", hcaes(x = wos_category, value = n, color = n)) 

hchart(df_summary, "bar", hcaes(x = wos_category, y = n)) %>% 
  hc_xAxis(title = list(text = "Research Area")) %>%
  hc_yAxis(title = list(text = "# literature"))


df_summary <- df %>% select(doc_id, publication) %>% 
  group_by(publication) %>% 
  summarise(n = n_distinct(doc_id)) %>% 
  arrange(desc(n))

hchart(df_summary, "bar", hcaes(x = publication, y = n)) %>% 
  hc_xAxis(title = list(text = "Journal")) %>%
  hc_yAxis(title = list(text = "# literature"))


df %>% filter(str_detect(wos_category, "eng"))
