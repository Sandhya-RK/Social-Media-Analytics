require(topicmodels)

dtm <- convert(dfmat_corp_twitter, to = "topicmodels")
lda <- LDA(dtm, k = 6, control=list(seed=12))

terms(lda, 8) %>% utf8::utf8_print()

topics(lda)[1:4]

topicAssignment = 
  data.table(
    index = lda %>% 
      topics %>% 
      names %>% 
      gsub("text","", .) 
    %>% as.integer,
    topic = lda %>% topics
  )
topicAssignment %>% head(4)

final.df$Topic = NA # creates a new col ‘topic’, assign it to NA
final.df$Topic[topicAssignment$index] = topicAssignment$topic
final.df$Topic = final.df$Topic %>% as.factor
ggplot(final.df, aes(x=created_at, y=Topic, col=Topic)) +
  geom_jitter(aes(size = retweet_count)) +
  ggtitle(paste0("Each dot is a tweet matching '",query,"'")) +
  scale_y_discrete() +
  scale_x_datetime(name = "") + 
  scale_color_discrete(guide = FALSE) + 
  scale_size_continuous(name="Retweets")

final.df[,list(Total.Retweets = sum(retweet_count)),by=Topic] %>% 
  ggplot(aes(x = Topic, y = Total.Retweets)) + 
  geom_col()

final.df[!is.na(Topic),
          list(
            TotalTweets = .N, 
            TotalReactions=sum(retweet_count, na.rm = TRUE) + 
              sum(favorite_count, na.rm = TRUE)+
              sum(reply_count, na.rm = TRUE)+
              sum(quote_count, na.rm = TRUE),
            Reach = sum(followers_count)/10000
          ), 
          by = Topic] %>% 
  melt(id.vars = "Topic") %>% 
  ggplot(aes(x = Topic, y = value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_discrete(name= "", breaks=c("TotalTweets","TotalReactions","Reach"), labels = c("Tweets","Reactions","Reach in 10,000s")) + 
  scale_y_continuous(name = "Count")

ggplot(final.df[Topic == 6], aes(x = followers_count)) + geom_histogram(binwidth = 10) + xlim(c(0,300))

ggplot(final.df[Topic == 6], aes(x = account_created_at)) +
  geom_histogram()

#plot the most common words in each topic
library(tidytext)
tweet_topics <- tidy(lda, matrix = "beta") %>% as.data.table

tweet_topics[order(-beta),.SD[1:3],by = topic][order(topic)]

library(tidytext)
tweet_topics[order(-beta),.SD[1:10],by = topic] %>% 
  ggplot(aes(x = reorder_within(term,beta,topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_x_reordered() + 
  coord_flip() + 
  theme_minimal()

