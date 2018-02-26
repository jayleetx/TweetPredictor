library(dplyr)
congress_df <- read.csv("../raw_data/legislators-current.csv",
                        stringsAsFactors = FALSE) %>%
  select(last_name, first_name, type, state, party, twitter) %>%
  rename(party_id = party,
         chamber_type = type)

# identify which accounts present in the twitterdata aren't in the congress list
missing <- unique(tweet_df$username)[-which(unique(tweet_df$username) %in% congress_df$twitter)]

# these are mostly just changing incorrect capitalizations
index <- c(457,369,321,418,509,
           498,471,481,296,482,522,
           483,270,135,499,452,524,
           502,492,455,241,401,207,
           283,76,61,186,40,508,
           505,516,514,300,475,136,
           233,146,425,528,264,124,
           112,84,307,117,89,454,
           178,331,97,182,217,51,
           316,183,427,504,109,8,
           534,479,466,214,480,236,
           147,45,69,213,37,9,
           226)
congress_df$twitter <- replace(congress_df$twitter, list = index, values = as.character(missing[-c(5,74)]))

# Chaffetz resigned and isn't in the congress df, Harris has two usernames in the tweet df
chaffetz <- c("Chaffetz", "Jason", "rep", "UT", "Republican", "jasoninthehouse")
harris2 <- c("Harris", "Kamala", "sen", "CA", "Democrat", "KamalaHarris")

congress_df <- rbind(congress_df, chaffetz, harris2) %>%
  filter(twitter %in% tweet_df$username)

# check all are accounted for
length(unique(tweet_df$username)[-which(unique(tweet_df$username) %in% congress_df$twitter)]) == 0

save(congress_df, file = "../clean_data/congress_df.RData")
