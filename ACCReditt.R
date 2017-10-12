library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(stringr)
library(scales)
library(purrr)
library(broom)
library(wordcloud)
library(SnowballC)
library(RedditExtractoR)


# ##example reddit
# example_urls = reddit_urls(search_terms = "science")
# ## Not run:
# example_attr = reddit_content(URL = "reddit.com/r/gifs/comments/39tzsy/whale_watching")
# example_data = get_reddit(search_terms = "economy")
# ## End(Not run)


##test reddit selection
link1 = reddit_content(URL="https://www.reddit.com/r/toronto/comments/6wtbpi/mlse_agrees_to_record_arena_rights_deal_with/")
link2 = reddit_content(URL="https://www.reddit.com/r/leafs/comments/6wtb9y/mlse_agrees_to_record_arena_rights_deal_with/")
link3 = reddit_content(URL="https://www.reddit.com/r/torontoraptors/comments/6wtbki/goodbye_acc_hello_scotiabank_arena/")
link4 = reddit_content(URL="https://www.reddit.com/r/nba/comments/6wtdu4/the_air_canada_centre_will_be_renamed_the/")
link5_nicknames = reddit_content(URL = "https://www.reddit.com/r/leafs/comments/6wv747/what_kind_of_nicknames_can_we_think_of_for_the/")

#combining datasets
accRaw <- rbind(link1, link2, link3, link4)

accRaw <- as_tibble(accRaw)
head(accRaw)
tail(accRaw)

nickNames <- as_tibble(link5_nicknames)
head(nickNames)
tail(nickNames)

# Use regrex to extract 

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

# Remove stop words, Tokenize and create a column called word 

redittComments <- accRaw %>%
  mutate(text = str_replace_all(comment, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  anti_join(stop_words)

nickNamesWords <- nickNames %>%
  mutate(text = str_replace_all(comment, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  anti_join(stop_words)

nNamesBiGram <- nickNames %>%
  mutate(text = str_replace_all(comment, replace_reg, "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

# Display most used words
redittComments %>%
  #filter(str_detect(word, "#")) %>% 
  count(word) %>%
  filter(n > 10) %>%
  with(wordcloud(word, n, max.words = 100))

# Display most used Nick names
nickNames %>%
  #filter(str_detect(word, "#")) %>% 
  count(word) %>%
  filter(n > 10) %>%
  with(wordcloud(word, n, max.words = 100))

# Calculate sentiments and arrange by count of words
reditt_sentiment <- redittComments %>%
  filter(!str_detect(word, "#") & !str_detect(word, "@")) %>% 
  count(word) %>%
  inner_join(get_sentiments("bing")) %>%
  arrange(word)

redit_stem_sentiment <- redittComments %>%
  filter(!str_detect(word, "#") & !str_detect(word, "@")) %>% 
  mutate(word = wordStem(word, language="english")) %>%
  count(word) %>%
  inner_join(get_sentiments("bing")) %>%
  arrange(word)


# Top 30 words by Negative and Positive sentiments
redit_stem_sentiment %>%
  select(word, sentiment, n) %>%
  group_by(sentiment) %>%
  top_n(30) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

