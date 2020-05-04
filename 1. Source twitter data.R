#Sourcing twitter data

library(rtweet)
save(final.df, file="tweets_project.RData")
load(file="tweets_project.RData")
query = "Money Heist"
number.of.tweets = 5000
final.df=NULL
df=NULL
max_id = NULL
for(i in 1:3){
  df<-search_tweets(
    query,
    n = number.of.tweets,
    type = "recent",
    include_rts = FALSE, #No retweets, only original tweets!
    geocode = NULL,
    max_id = max_id,
    parse = TRUE,
    token = NULL,
    retryonratelimit = FALSE,
    verbose = TRUE,
    lang = "en",
    tweet_mode = "extended" # get 240 character tweets in full
  )
  final.df = unique(rbind(df, final.df))
  max_id = tail(final.df$status_id,1)
  rm(df)
  #Sys.sleep(900)
}
