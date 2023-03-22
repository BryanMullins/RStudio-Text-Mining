install.packages(c("mnormt", "psych", "SnowballC", "hunspell", 
                   "broom", "tokenizers", "janeaustenr"))
install.packages(c("slam"))
install.packages("wordcloud")
install.packages("RColorBrewer")
library(ggplot2)
library(dplyr)
library(tidytext)
library(wordcloud)
library(topicmodels)
library(tm)
library(RColorBrewer)

Q2Dataset <- read.csv("Part-2.csv", header=TRUE, encoding="UTF-8")
str(Q2Dataset)
names(Q2Dataset)



# Cleaning the data
Q2Dataset$text <- tolower(Q2Dataset$text)
Q2Dataset$text  <-  gsub("rt", "", Q2Dataset$text) # RT
Q2Dataset$text <-  gsub("https\\S*", "", Q2Dataset$text) #links
Q2Dataset$text  <-  gsub("[[:punct:]]", "", Q2Dataset$text) #punctuation
Q2Dataset$text <-  gsub("@\\S*", "", Q2Dataset$text) #mentions
Q2Dataset$text  <-  gsub("[\r\n]", "", Q2Dataset$text) #\r\n\
Q2Dataset$text  <-  gsub("amp", "", Q2Dataset$text) # amp


# Deleting stop-words
tweet_stop_words <- Q2Dataset %>%
  select(text) %>%
  unnest_tokens(word, text)
tweet_stop_words <- tweet_stop_words %>%
  anti_join(stop_words)



# a) A frequency plot of the top 25 words

tweet_stop_words %>% 
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
 
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col(show.legend = FALSE) +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count") +
  labs(x = "Unique words") +
  labs(title = "The Top 25 Most Frequent Words") +
  theme(legend.position = "none") +
  theme_bw()

# b) A word cloud of 40 most common words

set.seed(1234)
tweet_40_most_common = tweet_stop_words %>% 
  count(word, sort = TRUE) %>%
  top_n(40) %>%
  mutate(word = reorder(word, n))
wordcloud(words=tweet_40_most_common$word, freq=tweet_40_most_common$n, min.freq=1, scale=c(2.5, .5), 
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#c) Can you check he was using Social media during breakfast time (6-10 AM) 
# or not? (Hint: Plot by the hour)

Q2Dataset$Date <- as.Date(Q2Dataset$date)
Q2Dataset$Time <- format(as.POSIXct(Q2Dataset$date), format="%H:%M")

tweets_by_time = Q2Dataset %>%
  filter(Time >= "06:00" & Time <= "10:00")

tweets_by_date = tweets_by_time %>%
  group_by(Date) %>%
  count()

ggplot(tweets_by_date, aes(x = Date, y = n)) +
  geom_line() +
  scale_x_date(date_labels = "%D") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y = "Number of Tweets") +
  labs(title = "Total number of Tweets sent from 6 to 10 AM")


# d) What was the usage per month? 

Q2Dataset$Month <- format(Q2Dataset$Date, "%Y-%m")
tweets_months = Q2Dataset %>%
  group_by(Month) %>%
  count()

ggplot(tweets_months, aes(x = Month, y = n, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y = "Number of Tweets") +
  labs(title = "Number of Tweets per Month") 


# e) What were the top-15 words for source=’iPhone’ and source=’Media Studio’. 
# Any interesting about this? 
# Do think both sources were handled by one person only?

tweets_iphone = Q2Dataset %>%
  filter(source == "Twitter for iPhone")

tweets_media = Q2Dataset %>%
  filter(source == "Media Studio")

tweet_words_iphone <- tweets_iphone %>%
  select(text) %>%
  unnest_tokens(word, text)
tweet_words_iphone <- tweet_words_iphone %>%
  anti_join(stop_words)

tweet_words_iphone %>% 
  count(word, sort = TRUE) %>%
  top_n(15) 

tweet_words_media <- tweets_media %>%
  select(text) %>%
  unnest_tokens(word, text)
tweet_words_media <- tweet_words_media %>%
  anti_join(stop_words)

tweet_words_media %>% 
  count(word, sort = TRUE) %>%
  top_n(15) 


# f) f) What are the six words that he did not use in the last six months of the data 
# but were frequently used in the first six months? 

first_six = Q2Dataset %>%
  filter(Month >= "2017-01-20" & Month <= "2017-06")

last_six = Q2Dataset %>%
  filter(Month >= "2018-04" & Month <= "2018-09")

tweet_first_6 <- first_six %>%
  select(text) %>%
  unnest_tokens(word, text)
tweet_first_6 <- tweet_first_6 %>%
  anti_join(stop_words)

tweet_first_6 %>% 
  count(word, sort = TRUE) %>%
  top_n(15)

tweet_last_6 <- last_six %>%
  select(text) %>%
  unnest_tokens(word, text)
tweet_last_6 <- tweet_last_6 %>%
  anti_join(stop_words)

tweet_last_6 %>% 
  count(word, sort = TRUE) %>%
  top_n(15)









