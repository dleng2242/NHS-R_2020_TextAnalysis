
# Text Analysis - NHS R 2020

# The github repo is found here: 
#   https://github.com/dleng2242/NHS-R_2020_TextAnalysis

# Please ask questions on the chat
# Please ensure you are on mute


library(tidyverse)


str_c("Mango", "the", "cat")
str_c("Mango", "the", "cat", sep = " ")

str_vec <- c("Mango", "the", "cat")
str_c(str_vec, collapse = " ")

str_c(str_vec, 1:3, collapse = " ", sep = "-")


str_sub("Mango the cat", start = 1, end = 5)
str_sub("Mango the cat", start = -3)

str_sub("Mango the cat", start = -10, end = -1)


# regular expressions

people_data <- readRDS("data/people_data.rds")

head(people_data)

people_data %>% 
  mutate(amount = as.numeric(amount))

people_data %>% 
  mutate(amount = str_remove_all(amount, pattern = "£"))

people_data %>% 
  mutate(amount = str_remove_all(amount, pattern = "[£r]"))

people_data %>% 
  mutate(amount = as.numeric(str_remove_all(amount, pattern = "[^0-9]")))


people_data %>% 
  filter(str_detect(amount, pattern = "[^0-9]", negate = TRUE))


people_data %>% 
  filter(str_detect(id, pattern = "A$"))

people_data %>% 
  filter(str_detect(id, pattern = "^[A-M]"))


people_data %>% 
  filter(str_detect(id, pattern = "^[AEIOU]"))

people_data %>% 
  filter(str_detect(id, pattern = "^.[AEIOU]"))

str_view(people_data$email, "\\.")



# ids 
people_data

str_view(people_data$id, pattern = "[AB]$")

str_view(people_data$id, pattern = "^[A-Z][A-Z]")
str_view(people_data$id, pattern = "^[A-Z]{2}")

str_view(people_data$id, pattern = "[0-9]{3}")

str_view(people_data$id, pattern = "^[A-Z]{2}[0-9]{3}[AB]$")


# tidyr separate and unite

people_data %>% 
  separate(col = email, into = c("username", "domain"), sep = "@")

people_data %>% 
  unite("full_name", first_name, second_name, sep = " ", remove = FALSE)




# tidy text

star_wars <- readRDS("data/star_wars_scripts.rds")

head(star_wars)
View(star_wars)

library(tidytext)


star_wars_IV <- star_wars %>% 
  filter(movie == "IV") %>% 
  select(line, character, dialogue)

head(star_wars_IV)

star_wars_IV_tidy <- star_wars_IV %>% 
  unnest_tokens(output = word, input = dialogue)

head(star_wars_IV_tidy)

star_wars_IV_tidy %>% 
  count(word, sort = TRUE)


head(stop_words)

star_wars_IV_tidy %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE)  %>% 
  head()

main_characters <- data.frame(word = c("leia", "darth", "vader", "luke", "obi", "wan"))

star_wars_IV_tidy %>% 
  anti_join(stop_words, by = "word") %>% 
  anti_join(main_characters, by = "word") %>% 
  count(word, sort = TRUE)  %>% 
  head()


library(SnowballC)

wordStem(c("fear", "fearing", "fearful", "play", "playing", "played"))

star_wars_IV_tidy %>% 
  anti_join(stop_words, by = "word") %>% 
  mutate(word = wordStem(word)) %>% 
  count(word, sort = TRUE)  %>% 
  head(10)


# word clouds


library(wordcloud)

star_wars_IV <- star_wars %>% 
  filter(movie == "IV") %>% 
  select(line, character, dialogue)

star_wars_IV_cloud <- star_wars_IV %>% 
  unnest_tokens(word, dialogue) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

head(star_wars_IV_cloud)

wordcloud(words = star_wars_IV_cloud$word,
          freq = star_wars_IV_cloud$n,
          max.words = 50, colors = "coral")


# comparison cloud


star_wars_IV_comP_cloud <- star_wars_IV %>% 
  filter(character %in% c("LEIA", "VADER")) %>% 
  unnest_tokens(word, dialogue) %>% 
  anti_join(stop_words) %>% 
  count(word, character, sort = TRUE) %>% 
  pivot_wider(names_from = character,
              values_from = n,
              values_fill = list(n = 0)) %>% 
  data.frame()


head(star_wars_IV_comP_cloud)


rownames(star_wars_IV_comP_cloud) <- star_wars_IV_comP_cloud$word
star_wars_IV_comP_cloud <- select(star_wars_IV_comP_cloud, -word)

head(star_wars_IV_comP_cloud)

comparison.cloud(star_wars_IV_comP_cloud, max.words = 50)


# ngrams

star_wars_IV <- star_wars %>% 
  filter(movie == "IV") %>% 
  select(line, character, dialogue)

star_wars_IV_ngram <- star_wars_IV %>% 
  unnest_tokens(word, dialogue, token = "ngrams", n = 2)

star_wars_IV_ngram %>% 
  count(word, sort = TRUE)

star_wars_IV_ngram_clean <- star_wars_IV_ngram %>% 
  count(word, sort = TRUE) %>% 
  separate(word, c("firstword", "secondword"), sep = " ") %>%
  anti_join(stop_words, by = c("firstword" = "word")) %>% 
  anti_join(stop_words, by = c("secondword" = "word")) %>% 
  drop_na()

head(star_wars_IV_ngram_clean)


# network plot


library(ggraph)
library(igraph)


star_wars_IV_ngram_clean_luke <- star_wars_IV_ngram_clean %>% 
  filter((firstword %in% c("luke", "force") | 
           secondword %in% c("luke", "force")))


igraph_object <- graph_from_data_frame(star_wars_IV_ngram_clean_luke)


ggraph(igraph_object, layout = 'stress') +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "coral", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# sentiment analysis

get_sentiments(lexicon = "bing")

?get_sentiments



star_wars_IV <- star_wars %>% 
  filter(movie == "IV") %>% 
  select(line, character, dialogue)

star_wars_IV_tidy <- star_wars_IV %>% 
  unnest_tokens(word, dialogue, token = "words")

head(star_wars_IV_tidy)


star_wars_IV_tidy %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  inner_join(get_sentiments(lexicon = "bing"), by = "word") %>% 
  group_by(sentiment) %>% 
  summarise(total = sum(n))


star_wars_IV_tidy_afinn <- star_wars_IV_tidy %>% 
  anti_join(stop_words) %>% 
  count(word, character, sort = TRUE) %>% 
  inner_join(get_sentiments(lexicon = "afinn"), by = "word")

head(star_wars_IV_tidy_afinn)

star_wars_IV_tidy_afinn_characters <- star_wars_IV_tidy_afinn %>% 
  group_by(character) %>% 
  summarise(score = mean(value),
            n = n()) %>% 
  filter(n > 20)

head(star_wars_IV_tidy_afinn_characters)  

star_wars_IV_tidy_afinn_characters %>% 
  ggplot(aes(x = reorder(character, -score), y = score)) + 
  geom_col()
  


# sentiment progression


star_wars_IV <- star_wars %>% 
  filter(movie == "IV") %>% 
  select(line, character, dialogue)

star_wars_IV_tidy <- star_wars_IV %>% 
  unnest_tokens(word, dialogue, token = "words")

head(star_wars_IV_tidy)  


star_wars_IV_tidy_sections <- star_wars_IV_tidy %>% 
  inner_join(get_sentiments(lexicon = "afinn"), by = "word") %>% 
  mutate(line = as.numeric(line)) %>% 
  mutate(scriptsection = line %/% 100) %>% 
  group_by(scriptsection) %>% 
  summarise(score = mean(value)) 

star_wars_IV_tidy_sections %>% 
  ggplot(aes(scriptsection, score)) +
  geom_line() +
  geom_point()


# term frequency - inverse document frequency


# bind_tf_idf

star_wars_IV <- star_wars %>% 
  filter(movie == "IV") %>% 
  select(line, character, dialogue)

star_wars_IV_tidy <- star_wars_IV %>% 
  unnest_tokens(word, dialogue, token = "words")

head(star_wars_IV_tidy)

star_wars_IV_tidy %>% 
  count(word, sort = TRUE)

star_wars_IV_tidy %>% 
  mutate(line = as.numeric(line)) %>% 
  mutate(scriptsection = line %/% 100) %>% 
  count(word, scriptsection, sort = TRUE) %>% head()
  
star_wars_IV_tidy_split <- star_wars_IV_tidy %>% 
  mutate(line = as.numeric(line)) %>% 
  mutate(scriptsection = line %/% 100) %>% 
  count(word, scriptsection, sort = TRUE) %>%
  bind_tf_idf(term = word, document = scriptsection, n = n)


star_wars_IV_tidy_split %>%
  group_by(scriptsection) %>%
  top_n(6, tf_idf) %>% 
  ggplot(aes(reorder_within(x = word, 
                            by = tf_idf, 
                            within = scriptsection),
             y = tf_idf)) +
  geom_col(show.legend = FALSE, fill = "azure4") +
  facet_wrap(vars(scriptsection), ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered("words")


  

