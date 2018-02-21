rm(list=ls())

library(tidyverse)
library(tidytext)
library(gutenbergr)
library(scales)
library(tidyr)

gutenberg_metadata %>%
  filter(author == "Kafka, Franz") 

kafka <- gutenberg_download(c(5200, 7849)) #downloading the works of Kafka available in English: "The Trial" and "Methamorphosis" 

gutenberg_metadata %>%
  filter(author == "Dostoyevsky, Fyodor") %>%
  head(30)

dostoyevsky <- gutenberg_download(c(600, 2554, 2638)) #downloading some works of Dostoyevsky that I have read


# tidying
tidy_kafka <- kafka %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_dostoyevsky <- dostoyevsky %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# cheking most frequent words
tidy_kafka %>%
  count(word, sort = TRUE)

tidy_dostoyevsky %>%
  count(word, sort = TRUE)

# Compairing vocabularies

frequency1 <- bind_rows(mutate(tidy_kafka, author = "Kafka, Franz"),
                       mutate(tidy_dostoyevsky, author = "Dostoyevsky, Fyodor")) #binding the data frames


frequency1 <- frequency1%>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) #extracting relevant words, grouping them by author

frequency1 <- frequency1%>%
  mutate(proportion = n / sum(n))%>% 
  select(-n) %>% 
  spread(author, proportion) #word proportions by authors in separate columns

ggplot(frequency1, aes(x = `Dostoyevsky, Fyodor`, y = `Kafka, Franz`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "Kafka, Franz", x = "Dostoyevsky, Fyodor")
# from the frequency plot we notice that there aren't many words that dostoyevsky and Kafka use with similar frequencies.
# It seems their vocabulary differs largely which is not surprising if we consider the different styles of the auhors and that 
# they have lived in different countries.  

## Term frequency and tf-idf in Kafka's novels

frequency_kafka <- bind_rows(mutate(tidy_kafka, author = "Kafka, Franz")%>%
                             mutate( book = ifelse(gutenberg_id == 5200, "Metamorphosis", "The Trial"))%>% #adding book names
                            count(book, word, sort = TRUE))

total_words_kafka <- frequency_kafka %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
#We notice that The Trial is much longer (i.e. has more words) than Methamorphosis.


book_words_kafka <- left_join(frequency_kafka, total_words_kafka)

ggplot(book_words_kafka, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")
# We notice the typical long tailed distributions. Methamorphosis is really a short piece,
# it has only a sparsely populated histogram when compared with The Trial on the same scale.

book_words_kafka <- book_words_kafka %>%
  bind_tf_idf(word, book, n)
book_words_kafka

book_words_kafka %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words_kafka %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()
# The words selected by tf_idf all make perfect sense. 
# Just by hearing the first three words selceted from The Trial, one could identify the book easily.

## Term frequency and tf-idf in Dostoyevsy's novels

frequency_dostoyevsky <- bind_rows(mutate(tidy_dostoyevsky, author = "Dostoyevsky, Fyodor")%>%
                               mutate( book = ifelse(tidy_dostoyevsky$gutenberg_id == 600, "Notes from the Underground", 
                                              ifelse(tidy_dostoyevsky$gutenberg_id == 2554, "Crime and Punishment","The Idiot")))) %>% #adding book names
  count(book, word, sort = TRUE)
                                  

total_words_dostoyevsky <- frequency_dostoyevsky %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
total_words_dostoyevsky
#We notice that Notes from the Underground is much shorter (i.e. has fewer words) than the two other books.

book_words_dostoyevsky <- left_join(frequency_dostoyevsky, total_words_dostoyevsky)

ggplot(book_words_dostoyevsky, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")
#We notice the typical long tailed distributions. The Idiot has the longest tail.

book_words_dostoyevsky <- book_words_dostoyevsky %>%
  bind_tf_idf(word, book, n)
book_words_dostoyevsky

book_words_dostoyevsky %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words_dostoyevsky %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

## Combined tf-idf of Kafka and Dostoyevsky

combined_words <- bind_rows(book_words_kafka,book_words_dostoyevsky)

combined_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()


# Very interesting how in the longer works of Dostoyevsky (Crime and Punishment, The Idiot) 
# the top representative words are exclusively names of people. In Kafka's case however, both works have only very 
# few personal names mutch rather objects. This tells a lot about the two authors different styles: Kafka writes from the  
# individual's point of view and puts his characters in a world that is absurd and alienated.
# Dostoyevsky on the other hand talks about society narrating his many characthers' stories from a distance.
