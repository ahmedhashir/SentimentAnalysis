library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(stringr)
library(scales)
library(purrr)
library(broom)
library(wordcloud)

# Load @Delata tweets in a tibble
deltaRaw <- read_csv("Data/delta.csv",col_names = c("num","timestamp","text"),
                     col_types = 
                       cols(
                        num = col_double(),
                        timestamp = col_datetime(format = "%.%.%.%.%b%.%d%.%H:%M:%S%.%z%.%Y"),
                        text = col_character()
                       )
                    )

head(deltaRaw)
tail(deltaRaw)

# Use regrex to extract 

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

# Tokenize and create a column called word 

delta <- deltaRaw %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  anti_join(stop_words)

# Display Wordcloud of non-delta twitter handles
delta %>%
  filter(str_detect(word, "@")) %>% 
  count(word) %>%
  filter(n > 10 & word != "@delta") %>%
  with(wordcloud(word, n, max.words = 100))

# Display most used 
delta %>%
    filter(str_detect(word, "#")) %>% 
    count(word) %>%
    filter(n > 10) %>%
    with(wordcloud(word, n, max.words = 100))

# Calculate sentiments and arrange by count of words
delta_sentiment <- delta %>%
  filter(!str_detect(word, "#") & !str_detect(word, "@")) %>% 
  count(word) %>%
  inner_join(get_sentiments("bing")) %>%
  arrange(desc(n))

# Top 30 words by Negative and Positive sentiments
delta_sentiment %>%
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
