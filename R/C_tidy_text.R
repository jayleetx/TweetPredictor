library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(tidyr)
library(tibble)
library(tm)
load("../clean_data/tweet_df.RData")
load("../clean_data/congress_df.RData")

#define groupings
group_phrases <- function(string) {
  text <- gsub("pro.?life", "pro_life", string)
  text <- gsub("pro.?choice", "pro_choice", text)
  text <- gsub("grass.?roots", "grass_roots", text)
  text <- gsub("alt.?right", "alt_right", text)
  text <- gsub("fake.?news", "fake_news", text)
  text <- gsub("gun.?control", "gun_control", text)
  text <- gsub("#?make america great again|#?maga", "maga", text)
  text <- gsub("american ?health ?care ?act", "ahca", text)
  text <- gsub("affordable ?care ?act", "aca", text)
  text <- gsub("obama.?care", "obamacare", text)
  text <- gsub("health.?care", "health_care", text)
  text <- gsub("health.?care", "health_care", text)
  text <- gsub('“|”|"|‘|’', "", text)
  text
}

# start simplifying text
str_sub(tweet_df$timestamp, 21, 26) <- "" 
tweet_df <- tweet_df %>%
  mutate(timestamp = parse_date_time(timestamp, "a b d HMS Y"))

replace_reg <- " ?https?.*\\S|&amp;|&lt;|&gt;|RT|['\",.?!]"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tidy_tweets <- tweet_df %>% 
  mutate(text = str_replace_all(text, replace_reg, ""),
         text = removeNumbers(text),
         text = tolower(text),
         text = group_phrases(text)) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# count occurrences of each word by tweet, spread users into columns
frequency <- tidy_tweets %>% 
  group_by(username) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(username) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total) %>%
  select(username, word, freq) %>% 
  spread(username, freq, fill = 0) %>%
  filter()

# only keep words that get used by at least 10 people
row_exists <- apply(frequency[-1], 1, function(x) length(which(x!=0)))
frequency <- filter(frequency, word %in% word[row_exists >= 10])

# transpose to get words on columns and users on rows
transpose_freq <- frequency %>%
  t() %>%
  data.frame(stringsAsFactors = FALSE) %>%
  rownames_to_column()
colnames(transpose_freq) <- c("username", transpose_freq[1, -1])
transpose_freq <- transpose_freq[-1, ]
rownames(transpose_freq) <- NULL

# make all word frequencies (by user) numeric again, they get messed up in the transpose
words <- colnames(transpose_freq)[-1]
transpose_freq <- transpose_freq %>%
  mutate_at(words, as.numeric)

# scale the columns to go from 0-1
# this makes it relative, so for any word the most prevalent user uses it with rate 1
# and if it's unused the rate is 0
# this scales all our variables so no one is more important just due to scale
range01 <- function(x, max){x/max}
max_vector <- apply(transpose_freq[ ,words], 2, max)
scaled_freq <- transpose_freq
scaled_freq[ ,words] <- sweep(x = scaled_freq[ ,words],
                                            MARGIN = 2,
                                            STATS = max_vector,
                                            FUN = range01)

full_data <- inner_join(congress_df, scaled_freq, by = c("twitter" = "username"))

save(tidy_tweets, "../clean_data/tidy_tweets.RData")
save(full_data, "../clean_data/full_data.RData")
