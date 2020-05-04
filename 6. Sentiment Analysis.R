library(sentimentr)
df <- final.df[,.(created_at,text,Topic)]

df$roundTime <- as.POSIXct(cut(df$created_at, breaks = "5 mins"))

df$roundTime <- df$created_at %>% # select created_at column
  cut(breaks = "5 mins") %>%     # cut every 5 min and group
  as.POSIXct      

df$text[1]
df$text[1] %>% get_sentences 
df$text[1] %>% get_sentences %>% sentiment
df$text[1] %>% get_sentences %>% sentiment_by

sentiment_by_tweet = 
  df[,
     list(text %>% get_sentences %>% sentiment_by(),
          Topic)]

sentiment_by_tweet

sentiment_by_Topic = 
  sentiment_by_tweet[, list(Tweets = .N,
                            ave_sentiment = mean(ave_sentiment),
                            sd_sentiment = sd(ave_sentiment),
                            Total_word_count = sum(word_count)),
                     by = Topic]
sentiment_by_Topic

t.test(sentiment_by_tweet[Topic ==1,ave_sentiment], sentiment_by_tweet[Topic ==6,ave_sentiment])

mean(sentiment_by_tweet$ave_sentiment)

df$polarity_score = sentiment_by_tweet$ave_sentiment
ggplot(df,aes(x=roundTime, y=polarity_score, fill=roundTime)) + 
  geom_boxplot()

df$roundTime <- as.factor(df$roundTime)

ggplot(df,aes(x=created_at, y=polarity_score,col=roundTime)) + 
  geom_point(size=0.4, alpha=0.9) + 
  theme(legend.position="none")

#Frequency analysis of negative and positive terms for each topic

final.df[Topic == 1,.(text)] %>% 
  head #show the first 6 lines

final.df[Topic == 1,.(text)] %>% 
  get_sentences() %>% #extract all sentences
  head  # show the first 6


final.df[Topic == 1,.(text)] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  head 

final.df[Topic == 1,.(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE)

final.df[,list(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,positive] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

final.df[Topic == 1,.(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,positive] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

final.df[Topic == 1,.(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

topics= unique(final.df$Topic)
topics
topics = topics[!is.na(topics)]
topics

max_terms = 10

for (i in topics) {
  neg <- final.df %>% subset(Topic == i) %>% 
    .[,text] %>% unlist() %>% 
    extract_sentiment_terms() %>% .[,negative] %>% unlist
  
  pos <- final.df %>% subset(Topic == i) %>% 
    .[,text] %>% unlist() %>% 
    extract_sentiment_terms() %>% .[,positive] %>% unlist
  
  pos <- sort(table(pos), decreasing = TRUE)
  # this is the same thing if you want to use pipes:
  #pos %>% table %>% sort(decreasing = TRUE)
  
  neg <- sort(table(neg), decreasing = TRUE)
  
  print(paste("Topic",i))
  print(pos[1:min(max_terms,length(pos))])
  
  print(neg[1:min(max_terms,length(neg))])
  print("------------------------------------------------")
}