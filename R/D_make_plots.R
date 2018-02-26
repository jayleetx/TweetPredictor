library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
load("../clean_data/tidy_tweets.RData")
load("../clean_data/congress_df.RData")
load("../clean_data/full_data.RData")

tidy_freqs <- tidy_tweets %>%
  filter(word %in% colnames(full_data)) %>%
  inner_join(congress_df, by = c("username" = "twitter"))
tidy_freqs <- tidy_freqs %>%
  mutate(total = ifelse(party_id == "Democrat",
                        sum(tidy_freqs$party_id == "Democrat"),
                        sum(tidy_freqs$party_id == "Republican"))) %>%
  group_by(party_id, word) %>%
  summarize(n = n(),
            total = mean(total),
            prop = n/total) %>%
  select(party_id, word, prop) %>% 
  spread(party_id, prop, fill = 0)

tidy_freqs <- tidy_freqs[sample(nrow(tidy_freqs)),]

tidy_freqs_user_tags <- filter(tidy_freqs, str_detect(word, "^@"))
tidy_freqs_hash_tags <- filter(tidy_freqs, str_detect(word, "^#"))
tidy_freqs_no_tags <- filter(tidy_freqs, !str_detect(word, "^@|^#"))

# all words
all_words_dot <- ggplot(tidy_freqs, aes(Democrat, Republican)) +
  geom_jitter(alpha = 0.05, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE,
            hjust = "inward", vjust = "inward") +
  scale_x_log10(labels = percent_format(), expand = c(.1,.1)) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  ggtitle("All Terms Used, by Political Party")

#user tags
user_tags_dot <- ggplot(tidy_freqs_user_tags, aes(Democrat, Republican)) +
  geom_jitter(alpha = 0.25, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE,
            hjust = "inward", vjust = "inward") +
  scale_x_log10(labels = percent_format(), expand = c(.1,.1)) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  ggtitle("Tagged Users, by Political Party")

# hashtags
hash_tags_dot <- ggplot(tidy_freqs_hash_tags, aes(Democrat, Republican)) +
  geom_jitter(alpha = 0.25, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE,
            hjust = "inward", vjust = "inward") +
  scale_x_log10(labels = percent_format(), expand = c(.1,.1)) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  ggtitle("Hashtags, by Political Party")

# regular words
reg_words_dot <- ggplot(tidy_freqs_no_tags, aes(Democrat, Republican)) +
  geom_jitter(alpha = 0.05, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE,
            hjust = "inward", vjust = "inward") +
  scale_x_log10(labels = percent_format(), expand = c(.1,.1)) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  ggtitle("Regular Words Used, by Political Party")


log_ratios <- tidy_freqs %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(Democrat / Republican)) %>%
  arrange(desc(logratio))

head(arrange(log_ratios, abs(logratio)))
# all terms
all_words_bar <- log_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (Democrat/Repubican)") +
  ggtitle("Highest Usage Difference, by Party - All Terms") +
  scale_fill_manual(name = "", labels = c("Democrat", "Republican"), values=c("#00BFC4","#F8766D"))
# user tags
user_tags_bar <- log_ratios %>%
  filter(str_detect(word, "^@")) %>%
  group_by(logratio < 0) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (Democrat/Repubican)") +
  ggtitle("Highest Usage Difference, by Party - User Tags") +
  scale_fill_manual(name = "", labels = c("Democrat", "Republican"), values=c("#00BFC4","#F8766D"))
#hashtags
hash_tags_bar <- log_ratios %>%
  filter(str_detect(word, "^#")) %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (Democrat/Repubican)") +
  ggtitle("Highest Usage Difference, by Party - Hashtags") +
  scale_fill_manual(name = "", labels = c("Democrat", "Republican"), values=c("#00BFC4","#F8766D"))

# regular terms
reg_words_bar <- log_ratios %>%
  filter(!str_detect(word, "^#|^@")) %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (Democrat/Repubican)") +
  ggtitle("Highest Usage Difference, by Party - Regular Terms") +
  scale_fill_manual(name = "", labels = c("Democrat", "Republican"), values=c("#00BFC4","#F8766D"))

ggsave(filename="images/all_words_dot.png", plot=all_words_dot)
ggsave(filename="images/all_words_bar.png", plot=all_words_bar)
ggsave(filename="images/user_tags_dot.png", plot=user_tags_dot)
ggsave(filename="images/user_tags_bar.png", plot=user_tags_bar)
ggsave(filename="images/hash_tags_dot.png", plot=hash_tags_dot)
ggsave(filename="images/hash_tags_bar.png", plot=hash_tags_bar)
ggsave(filename="images/reg_words_dot.png", plot=reg_words_dot)
ggsave(filename="images/reg_words_bar.png", plot=reg_words_bar)
