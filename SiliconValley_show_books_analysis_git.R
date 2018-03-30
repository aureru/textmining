rm(list=ls())

library(tidytext)
library(topicmodels)
library(tidyverse)
library(textreadr)
library(ggplot2)
library(janeaustenr)
library(stringr)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(devtools)
library(subtools)
library(tidyr)



hard <- as.data.frame(read_docx("the_hard_thing_about_hard_thing_horowitz.docx"),stringsAsFactors=FALSE)
lean <- as.data.frame(read_docx("the lean startup.docx"),stringsAsFactors=FALSE)
zero <- as.data.frame(read_docx("zero to one.docx"),stringsAsFactors=FALSE)

colnames(hard) <- c("text")
colnames(lean) <- c("text")
colnames(zero) <- c("text")

hard <- hard %>% mutate(book = "hard")
lean <- lean %>% mutate(book = "lean")
zero <- zero %>% mutate(book = "zero")

books_df <- bind_rows(hard,lean,zero)

stop_words <- stop_words %>%
  rbind(data.frame(word = c("it's"),
                   lexicon = "custom"))

# not going to have chapters

tidy_hard <- hard %>%
  unnest_tokens(word, text)%>%
  mutate(book = "hard")%>%
  anti_join(stop_words)

tidy_lean <- lean %>%
  unnest_tokens(word, text)%>%
  mutate(book = "lean")%>%
  anti_join(stop_words)

tidy_zero <- zero %>%
  unnest_tokens(word, text)%>%
  mutate(book = "zero")%>%
  anti_join(stop_words)

tidy_books <- bind_rows(tidy_hard,tidy_lean,tidy_zero)

tidy_books %>% group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(30) %>%
  ggplot(aes(reorder(word,n), n, fill = book)) +
  geom_col() +
  theme(text = element_text(size=18)) +
  coord_flip() +
  ggtitle("Top 30 most frequent words by book") +
  facet_wrap(~book, scales = "free_y") +
  labs(x = NULL) +
  guides(fill = FALSE) +
  scale_fill_brewer(palette = "Set1")

# word frequency by book

top_nr <- 50

top_hard <- tidy_hard %>%
  count(word, sort = TRUE) %>%
  top_n(top_nr)

top_lean <- tidy_lean %>%
  count(word, sort = TRUE) %>%
  top_n(top_nr)

top_zero <- tidy_zero %>%
  count(word, sort = TRUE) %>%
  top_n(top_nr)

# N-grams

bigram_graph <- books_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  group_by(book) %>%
  count(word1, word2, sort = TRUE) %>%
  select(word1, word2, book, n) %>%
  filter(n > 4) %>%
  graph_from_data_frame()


largest_graph <- decompose.graph(bigram_graph)[[1]]

vertex_attr(largest_graph, "dgc")<-centr_degree(largest_graph)$res
ggraph(largest_graph, layout = "kk") +
  geom_edge_link(alpha = .5) +
  geom_node_point(aes(color = dgc, size = dgc * 5)) +
  geom_node_text(aes(label = name, size = dgc*5), vjust = 1, hjust = 1, check_overlap = T)+
  scale_colour_gradient(low="blue",high="green") +
  guides(size=FALSE) +
  ggtitle("Central Bi-gram Network of the books") +
  theme_graph() +
  theme(legend.position = "bottom")



# tf-idf
## tf by book
tf_idf_books <- tidy_books %>% 
  count(book, word, sort = TRUE) %>%
  bind_tf_idf(word, book, n)



# create rank with words having at least 5 mentions
tf_hard<- tf_idf_books %>%
  select(-tf_idf,-idf)%>%
  arrange(desc(tf)) %>%
  filter( book == "hard", n > 4)%>%
  mutate(word = factor(word, levels = rev(unique(word))), rank_hard = dense_rank(desc(tf)))%>%
  rename(tf_hard=tf, n_hard = n)

tf_zero<- tf_idf_books %>%
  select(-tf_idf,-idf)%>%
  arrange(desc(tf)) %>%
  filter( book == "zero", n > 4)%>%
  mutate(word = factor(word, levels = rev(unique(word))), rank_zero = dense_rank(desc(tf)))%>%
  rename(tf_zero=tf, n_zero = n)

tf_lean<- tf_idf_books %>%
  select(-tf_idf,-idf)%>%
  arrange(desc(tf)) %>%
  filter( book == "lean", n > 4)%>%
  mutate(word = factor(word, levels = rev(unique(word))), rank_lean = dense_rank(desc(tf)))%>%
  rename(tf_lean=tf, n_lean = n)


tf_idf_books %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(30) %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ book, scales = "free") +
  coord_flip() +
  ggtitle("Words with the Highest TF-IDF Scores") +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")


# topic modeling

popular_words <- c("company","product", "customer","time",
                   "business")

books_dtm <- tidy_books %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  cast_dtm(book, word, n) 

books_lda <- LDA(books_dtm, k = 3, control = list(seed = 1234))

topic_1<- tidy(books_lda, matrix = "beta")%>%
  filter(topic == 1)
topic_2<- tidy(books_lda, matrix = "beta")%>%
  filter(topic == 2)
topic_3<- tidy(books_lda, matrix = "beta")%>%
  filter(topic == 3)

tidy(books_lda, matrix = "beta")%>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Word / Topic Probabilities")

t<- c("hard", "lean","zero")
b<- c("hard","lean","zero")

titles <- data.frame(document = as.character(b),title = factor(t, levels = t))

tidy(books_lda, matrix = "gamma")%>%
  inner_join(titles) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  ggtitle("Dominant Topics per Book")

#### SERIES

devtools::install_github("fkeck/subtools")


setwd("C:")
a <- read.subtitles.serie(dir = "/series/silicon valley/")
series_df <- subDataFrame(a)

data(stop_words)
tidy_series <- series_df %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words)%>%
  select(-ID,-Timecode.in,-Timecode.out,-season_num,-episode_num,-serie)

# word frequencies

series_word <- tidy_series %>% group_by(word) %>%
  mutate(document = "series_text") 

series_tf<- series_word%>%
  count(document, word, sort = TRUE) %>%
  bind_tf_idf(word, document, n)%>%
  select(-tf_idf,-idf)%>%
  mutate(rank = dense_rank(desc(tf)))

series_tf <- data.frame(series_tf)%>% 
  mutate(rank = dense_rank(desc(n)))

tidy_series %>% group_by(season) %>%
  count(word, sort = TRUE) %>%  
  top_n(30) %>%
  ggplot(aes(reorder(word,n), n, fill = season)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 30 most frequent words by season") +
  facet_wrap(~season, scales = "free_y") +
  labs(x = NULL) +
  guides(fill = FALSE) +
  scale_fill_brewer(palette = "Set1")

# N-grams

bigram_graph <- tidy_series %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  group_by(season) %>%
  count(word1, word2, sort = TRUE) %>%
  select(word1, word2, season, n) %>%
  filter(n > 2) %>%
  graph_from_data_frame()

print(bigram_graph)

# Decompose graph to get largest connected network,
largest_graph <- decompose.graph(bigram_graph)[[1]]

# Calculate degree centrality 
# and add as vertex attribute
vertex_attr(largest_graph, "dgc")<-centr_degree(largest_graph)$res
ggraph(largest_graph, layout = "kk") +
  geom_edge_link(alpha = .5) +
  geom_node_point(aes(color = dgc, size = dgc * 5)) +
  geom_node_text(aes(label = name, size = dgc*5), vjust = 1, hjust = 1, check_overlap = T)+
  scale_colour_gradient(low="blue",high="green") +
  guides(size=FALSE) +
  ggtitle("Central Bi-gram Network of the show") +
  theme_graph() +
  theme(legend.position = "bottom")


# tf-idf

tf_idf_series <- tidy_series %>% 
  count(season, word, sort = TRUE) %>%
  bind_tf_idf(word, season, n)

tf_idf_series %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(30) %>%
  ggplot(aes(word, tf_idf, fill = season)) +
  geom_col() +
  facet_wrap(~ season, scales = "free") +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  ggtitle("Words with the Highest TF-IDF Scores of the show") +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")

# not going to have topic modeling for series as data is missing

# Combine books and series analysis

combined_series_hard<- series_tf%>%
  inner_join(tf_hard)%>%
  mutate(diff= abs(tf-tf_hard), rank_res_hard = rank*rank_hard)%>%
  arrange(rank_res_hard)%>%
  select(-document, -n, -tf,-rank, -book, -diff, -n_hard, -tf_hard, -rank_hard)
  
combined_series_lean<- series_tf%>%
  inner_join(tf_lean)%>%
  mutate(diff= abs(tf-tf_lean), rank_res_lean = rank*rank_lean)%>%
  arrange(rank_res_lean)%>%
  select(-document, -n, -tf,-rank, -book, -diff, -n_lean, -tf_lean, -rank_lean)

combined_series_zero<- series_tf%>%
  inner_join(tf_zero)%>%
  mutate(diff= abs(tf-tf_zero), rank_res_zero = rank*rank_zero)%>%
  arrange(rank_res_zero)%>%
  select(-document, -n, -tf,-rank, -book, -diff, -n_zero, -tf_zero, -rank_zero)

t<- c("The Hard Thing", "Lean Startup", "From Zero to One")
b<- c("hard","lean","zero")

results_1 <- combined_series_hard%>%
  full_join(combined_series_lean, by = "word")%>% 
  full_join(combined_series_zero, by = "word")

results_2 <-gather(results_1,"word")
colnames(results_2) <- c("word","book","value")

results_2%>%
    filter(value <1000)%>% 
    group_by(book)%>%
    count(book, sort = TRUE)%>%
    ggplot(aes(book, n, fill=book)) +
    geom_col()
  
results_2%>%
  filter(value <1000)%>% 
  ggplot(aes(reorder(word,-value, order =TRUE), value, fill = book)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~book, scales = "free_y") +
  labs(x = NULL) +
  guides(fill = FALSE)
  



