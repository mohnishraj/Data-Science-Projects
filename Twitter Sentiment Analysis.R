library(twitteR)
 library(ROAuth)

consumer_key <- 'xxxxxxxxxxxxxxx'
consumer_secret <- 'xxxxxxxxxxxxxxxxxxxxxxxx'
access_token <- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
access_secret <-'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

install.packages("plyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("httr")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("tm")
install.packages("sentiment")
install.packages("bitops")
install.packages("RCurl")
install.packages("syuzhet")
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(sentiment)
library(bitops)
library(RCurl)
library(syuzhet)

some_tweets = searchTwitter("demonetisation",n=200000,since = "2017-12-01",lang="en")
length.some_tweets <-length(some_tweets)
length.some_tweets
some_tweets.df <-ldply(some_tweets,function(t)t$toDataFrame())
write.csv(some_tweets.df,"demonetisation.csv")
some_txt = sapply(some_tweets, function(x)x$getText())

some_txt1=gsub("(RT|via)((?:\\b\\w*@\\w+)+)"," ",some_txt)
some_txt2=gsub("http[^[:blank:]]+"," ",some_txt1)
some_txt3=gsub("@\\w+"," ",some_txt2)
some_txt4=gsub("[[:punct:]]"," ",some_txt3)
some_txt5=gsub("[^[:alnum:]]"," ",some_txt4)
write.csv(some_txt5,"demonetisation.csv")
some_txt6 <-Corpus(VectorSource(some_txt5))

some_txt6<- tm_map(some_txt6,removePunctuation)
some_txt6<- tm_map(some_txt6,content_transformer(tolower))
some_txt6<- tm_map(some_txt6,removeWords,stopwords("english"))
 some_txt6<- tm_map(some_txt6,stripWhitespace)
 
 pal <- brewer.pal(9,"Pastel1") 
 wordcloud(some_txt6,min.freq = 5,max.words = Inf,width=1000,height=1000,random.order = FALSE,color= pal)
 
 mysentiment <- get_nrc_sentiment(some_txt5)
sentimentscores <- data.frame(colSums(mysentiment))
sentimentscores
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

ggplot(data = sentimentscores,aes(x=sentiment,y=Score)) + geom_bar(aes(fill=sentiment),stat = "identity")+theme(legend.position = "none")+ xlab("sentiment") + ylab("Score") +ggtitle("ARR25CHENNAI- SENTIMENT ANALYSIS")


some_tweetsarr.df$timeonly <- as.numeric(some_tweetsarr.df$created - trunc(some_tweetsarr.df$created, "days"))

class(some_tweetsarr.df$timeonly) <- "POSIXct"
some_tweets.df$timeonly <- with_tz(some_tweets.df$timeonly, "Asia/Colombo")

ggplot(data = some_tweetsarr.df, aes(x = some_tweets.df$created)) +
  geom_histogram(aes(fill = ..count..), bins = 60) +
  xlab("Time") + ylab("Number of tweets") + labs(title = "Distribution of Tweets over time") + scale_x_datetime(breaks = date_breaks("1 hour"),
  labels = date_format("%H:00")) + 
  scale_fill_gradient(low = "orange", high = "darkblue")

ggplot(data = some_tweetsarr.df, aes(x = some_tweetsarr.df$created)) +
  geom_histogram(aes(fill = ..count..), bins=60) +
  xlab("Time") + ylab("Number of tweets") + labs(title = "Demonetisation-Distribution of Tweets over entire duration") +
  scale_x_datetime(breaks = date_breaks("1 hour"))
  scale_fill_gradient(low = "lightblue", high = "dark green")


ggplot(some_tweetsarr.df, aes(x= some_tweets.df$created)) + 
  geom_density(aes(fill = isRetweet), alpha = .5) +
  scale_fill_discrete(guide = 'none') +
  scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%H:00")) +    
  xlab('All tweets') + labs(title = "Demonetisation - Distribution of Tweets over time of day")

some_tweetsarr.df$created <- ymd_hms(some_tweets.df$created)


some_tweetsarr.df$statusSource <- gsub("http[^[:blank:]]+", " ", some_tweetsarr.df$statusSource)
some_tweetsarr.df$statusSource <- gsub("[[:punct:]]", " ", some_tweetsarr.df$statusSource)
some_tweetsarr.df$statusSource <- gsub(" a ", " ", some_tweetsarr.df$statusSource)
some_tweetsarr.df$statusSource <- gsub("[ \t]{2,}", " ", some_tweetsarr.df$statusSource)
some_tweetsarr.df$statusSource <- gsub("^\\s+|\\s+$", " ", some_tweetsarr.df$statusSource)
some_tweetsarr.df$statusSource <- gsub(" a href rel nofollow ", " ", some_tweetsarr.df$statusSource)
some_tweetsarr.df$statusSource <- gsub(" href rel nofollow ", " ", some_tweetsarr.df$statusSource)

tweetsBySource <- some_tweets.df %>%
  group_by(statusSource) %>%
  summarize(freqSrc=n()) %>%
  arrange(desc(freqSrc))

tweetsBySource.Top <- tweetsBySource[order(-tweetsBySource$freqSrc),]
tweetsBySource.Top10 <- tweetsBySource.Top[1:10,]

ggplot(tweetsBySource.Top10, aes(sort(tweetsBySource.Top10$statusSource,decreasing = T),tweetsBySource.Top10$freqSrc)) + 
  geom_bar(stat = "identity", width=0.5) + coord_flip() +
  geom_text(aes(label=tweetsBySource.Top10$freqSrc), hjust = -0.3, size =3, color = "black") +
  ylab("Number of Tweets") + xlab("Twitter handle") + labs(title = "Top 10 Sources that contribute to maximum posts") +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=16, colour = "black"))