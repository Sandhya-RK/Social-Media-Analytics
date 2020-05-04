library(data.table)
setDT(final.df) 
final.df[1:3, c(screen_name, source)]
final.df[1:3, .(screen_name, source)]

#Subseting data tables

final.df[country == "Ireland" | country == "United Kingdom"]
final.df[(country == "Ireland" | country == "United Kingdom") & source == "Instagram"]
final.df[retweet_count>10, (retweet_count+favorite_count)]
final.df[,.(.N), by = .(country)] [order(-N)]
final.df[,.(TotalTweets = .N, 
            total_reactions=sum(retweet_count, na.rm = TRUE) + 
              sum(favorite_count, na.rm = TRUE)+
              sum(reply_count, na.rm = TRUE)+
              sum(quote_count, na.rm = TRUE)), 
         by = .(country)] [order(-total_reactions)]

final.df[,.(.N), by = .(country, verified)] 

library(magrittr)
final.df[, chunk:= created_at %>% cut(breaks = "5 min") %>% as.factor ]
