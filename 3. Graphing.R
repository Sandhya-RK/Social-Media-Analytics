library(quanteda)
#Graphing
library(ggplot2)

#Scatter-plot
ggplot(final.df, aes(x=created_at, y=(friends_count+1))) +
  geom_point() +
  scale_x_datetime(name = "Time") +
  scale_y_log10(name = "Potentia Reach", breaks = c(10,100,1000,10000) ) +
  theme_minimal()

#Histogram
ggplot(final.df, aes(x=created_at)) +
  geom_histogram(aes(y=..count..), #make histogram
                 binwidth=60, #each bar contains number of tweets during 60 s
                 colour="blue", #colour of frame of bars
                 fill="blue", #fill colour for bars
                 alpha=0.8) + # bars are semi transparant
  ggtitle(paste0("Activity ",number.of.tweets," tweets")) + #title
  scale_y_continuous(name="Number of Tweets per minute") + 
  scale_x_datetime(name = "Time") +
  theme_minimal(base_family="Times New Roman")

#Graph
ggplot(final.df, aes(
  x=created_at, 
  y=(friends_count+1), 
  size = favorite_count + reply_count + quote_count + retweet_count )
) +
  geom_point(aes(size = retweet_count), alpha = 0.5) +
  ggtitle(paste0("Each dot is a tweet matching '",query,"'")) +
  scale_y_log10(name="Potential Reach",breaks = c(10,100,1000,10000) ) +
  scale_x_datetime(name = "Time") +
  scale_size_continuous(name="Retweets") +
  theme_minimal()

#Tokens
tok_tweets <- final.df$text %>% 
  gsub("#","", . ) %>% 
  corpus() %>% 
  tokens(what="word",
         remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_symbols=TRUE,
         remove_separators=TRUE,
         remove_url=TRUE)
head(tok_tweets,n=2)

tok_tweets <- tokens_remove(tok_tweets,stopwords(language = "en"))
head(tok_tweets,n=2)

#Word Frequencies
words.to.remove <- c(stopwords("english"),'Money Heist','Money','Heist','Professor','Nairobi')
dfmat_corp_twitter <- final.df$text %>% corpus() %>% 
  dfm(remove = words.to.remove,
      what = "word",
      stem = TRUE, 
      remove_punct = TRUE,
      remove_url=TRUE)

dfFreq <- textstat_frequency(dfmat_corp_twitter) %>% as.data.table
ggplot(dfFreq[1:20,], aes(x=feature, y=frequency)) + 
  geom_col() +
  coord_flip() +
  theme_minimal()

dfFreq[1:7,]

ggplot(dfFreq[1:20,], aes(x=reorder(feature, -rank), y=frequency)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Stemmed word", y = "Count") +
  theme_minimal(base_family="Times New Roman")

textplot_wordcloud(dfmat_corp_twitter, min_count = 6, random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

dfFreq_long_top20 = dfFreq[rank <= 20] %>% 
  melt(id.vars = c("feature","group","rank"),
       measure.vars = c("frequency","docfreq")
  )

ggplot(dfFreq_long_top20, aes(x=reorder(feature,-rank), y=value, fill = variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete() + 
  labs(x = "", y = "Occurances", fill = "") +
  coord_flip() +
  theme_minimal()



