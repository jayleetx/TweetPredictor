library(jsonlite)
library(dplyr)
unzip("../raw_data/10001_house.zip", exdir = "../raw_data")
unzip("../raw_data/50000_senate.zip", exdir = "../raw_data")
house_file <- "../raw_data/sample1.json"
senate_file <- "../raw_data/senate_sample1.json"
house_list <- fromJSON(house_file, simplifyVector = FALSE)
senate_list <- fromJSON(senate_file, simplifyVector = FALSE)
# Each [[i]] is a tweet

house_df <- matrix(ncol = 6, nrow = length(house_list))
senate_df <- matrix(ncol = 6, nrow = length(senate_list))
for (i in seq_along(house_list)) {
  tweet <- house_list[[i]]
  house_df[i, ] <- c(tweet$id_str,
                     tweet$full_text,
                     tweet$created_at,
                     tweet$user$screen_name,
                     tweet$favorite_count,
                     tweet$retweet_count)
}
for (i in seq_along(senate_list)) {
  tweet <- senate_list[[i]]
  senate_df[i, ] <- c(tweet$id_str,
                     tweet$full_text,
                     tweet$created_at,
                     tweet$user$screen_name,
                     tweet$favorite_count,
                     tweet$retweet_count)
}

tweet_df <- data.frame(rbind(house_df, senate_df))
colnames(tweet_df) <- c("doc_id", "text", "timestamp","username", "favorites","retweets")
# saved_tweet_df <- tweet_df

save(tweet_df, file = "../clean_data/tweet_df.RData")
