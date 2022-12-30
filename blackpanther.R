#load all the required packages to work with
library(twitteR) 
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud2)
library(plyr)
library(stringr)
library(plotly)
library(dplyr)  
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)
library(data.table)
library(sentimentr)
library(pdftools)
library(textdata)
library(ggplot2)
library(RColorBrewer)


#set up API keys
api_key <- "NJHpoKti2SEY2651ZyePuVRkZ"
api_secret <- "9dibk0VthcoZGxYMzNz0B5i6RztgMqpMGMw1GtZ63jQW6AhSgy"
access_token <- "1011668912981848065-Cgk3F2ZbQm4JtHTGdip0zoDF7qBuYb"
access_token_secret <- "n6WhPMbaxANZzyHLTHWqQmXn1crsc1FKbdzHEpGwzbfjn"
setup_twitter_oauth('NJHpoKti2SEY2651ZyePuVRkZ', '9dibk0VthcoZGxYMzNz0B5i6RztgMqpMGMw1GtZ63jQW6AhSgy',
                    '1011668912981848065-Cgk3F2ZbQm4JtHTGdip0zoDF7qBuYb', 'n6WhPMbaxANZzyHLTHWqQmXn1crsc1FKbdzHEpGwzbfjn')

#Get tweets about blackpanther
search_words <- list("blackpanther", "wakandaforever", "blackpantherwakandaforever", "blackpanther2")
getTweets <- function(search_words, n = 20000) 
search_string <- paste0("@", search_words, " OR ", "#", search_words, " OR ", search_words)
tweets <- searchTwitter(search_string, since = "2022-11-11", n = 20000, lang = "en", retryOnRateLimit = 120)

#i got tweets in 4 batches.
#also i didn't get up to the specified number of tweets i requested 
black_panther2 <- searchTwitter("blackpanther -filter:retweets",since = '2022-11-22', n = 20000, lang = "en", retryOnRateLimit = 120)
wakanda_forever2 <- searchTwitter("#wakandaforever -filter:retweets",since = '2022-11-22', n = 20000, lang = "en", retryOnRateLimit = 120)
blackpanther_wakandaforever2 <- searchTwitter("#blackpantherwakandaforever -filter:retweets",since = '2022-11-22', n = 20000, lang = "en", retryOnRateLimit = 120)
black_panther22 <- searchTwitter("blackpanther2 -filter:retweets",since = '2022-11-22', n = 20000, lang = "en", retryOnRateLimit = 120)

#put in a data frame
black_panther.df <- twListToDF(black_panther2)
wakanda_forever.df <- twListToDF(wakanda_forever2)
blackpanther_wakandaforever.df <- twListToDF(blackpanther_wakandaforever2)
black_panther2.df <- twListToDF(black_panther22)

#viewing the data frames
View(black_panther.df)
View(wakanda_forever.df)
View(blackpanther_wakandaforever.df)
View(black_panther2.df)

#merge the 4 data frames together
Blackpantherdata <- do.call("rbind", list(black_panther.df, wakanda_forever.df, blackpanther_wakandaforever.df, black_panther2.df))
NROW(Blackpantherdata)
View(Blackpantherdata)


#save the data sets to .csv files
write.csv(Blackpantherdata,"C:\\Users\\user\\Documents\\Data Analysis\\Datasets\\Blackpantherdata.csv", row.names = FALSE)

tweets <- Blackpantherdata


#Data Cleansing
#Here is the order of cleaning tweets text.
# First step in this is to remove URLS as it might get difficult to remove URLs after removing punctuation, numbers etc, as it disturbs the url text which prevents removing URLS. 
#We'll remove strings between "<" and ">". This is to remove smileys and other encoded text. 
#Remove retweet entities.
#Remove quotes and apostrophe.
#Remove @people text. 
#Remove punctuation. This removes basic english punctuation.
#Remove single letters like 's', 'a'
#Remove unnecessary spaces.
#Remove leading and trailing white spaces.
#remove numbers.
#remove links.


# Remove URLs
tweets$text = gsub("http[^[:space:]]*", "",tweets$text)
# Remove retweet entities 
tweets$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)"," ",tweets$text)
# Remove quotes
tweets$text = gsub("'s|'s|[...]", "", tweets$text)
# Remove at people 
tweets$text = gsub("@\\w+", " ", tweets$text)
# Remove punctuation 
tweets$text = gsub("[[:punct:]]", " ", tweets$text)
# Remove single letters.
tweets$text = gsub(" *\\b[[:alpha:]]{1}\\b *", "", tweets$text)
# Remove unnecessary spaces
tweets$text = gsub("[ \t]{2,}", " ", tweets$text)
# Remove leading and trailing white spaces 
tweets$text = gsub("^\\s+|\\s+$", "", tweets$text)
# remove numbers
tweets$text = gsub("[[:digit:]]", "", tweets$text)
# convert to lower case
tweets$text = tolower(tweets$text)
# remove links http
tweets$text = gsub("http\\w+", "", tweets$text)
#remove emoticons
tweets$text <- sapply(tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
#remove stop words
tweets <- anti_join.data.frame(tweets, stopwords("en"))


View(tweets)

#Check for duplicate tweets.
tweets.text = data.frame(tweets$text, tweets$id) 
grouped_data <- aggregate(tweets.text, by=list(tweets$text, tweets$id), FUN=length);
colnames(grouped_data) = c("Text","id","TweetCount_1","Tweet_Count_2")
grouped_data = arrange(grouped_data, desc(TweetCount_1))
head(subset(grouped_data, grouped_data$TweetCount_1 > 1))

#i noticed there was duplicate tweets by same id. I don't want to include such tweets as it affects the strength of sentiment, for example, a tweet was repeated 28 times If the tweet by that id has any keyword affecting the sentiment, it strengthens the senitment as the same keyword is repeated 28 times. But ideally it should be considered only once so that the sentiment won't be affected.
#To solve this duplicate tweets problem, i considered unique tweets in the combination of Text and id.
# Extract unique tweets in combination of Text and id
tweets = data.table(tweets)
tweets = unique(tweets, by = c("text","id"), fromLast=TRUE)
nrow(tweets)


#for wordcloud
#creating a corpus
corpus <- SimpleCorpus(VectorSource(Blackcleaned$cleaned))
# And lets see what we have
view(corpus)

#cleaning the corpus
# 1. Stripping any extra white space:
Corpus <- tm_map(Corpus, stripWhitespace)
# 2. Transforming everything to lowercase
Corpus <- tm_map(Corpus, content_transformer(tolower))
# 3. Removing numbers 
Corpus <- tm_map(Corpus, removeNumbers)
# 4. Removing punctuation
Corpus <- tm_map(Corpus, removePunctuation)
# 5. Removing stop words
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))
#remove blackpanther words
text_corpus <- tm_map(Corpus, removeWords, c("blackpanther","marvel","wakandaforever","forever","wakanda","panther","black", "movie"))

#create a document term matrix
dtm <- TermDocumentMatrix(BLACKPANTHER) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

#generate wordcloud
set.seed(1234) 
wordcloud2(data=df, size=1.6, color='random-dark', shape='circle')


#Sentiment analysis
#downloaded the three general purpose lexicons
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran")

tweetstext <- tweets %>% 
  select(text, screenName)

View(tweetstext)

#performing the tidy operation such that each row contains a single word.
tidy_tweets <- tweetstext%>%
  group_by(screenName) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
View(tidy_tweets)


#making use of the “bing” lexicon implement filter() over the words that are positive.
positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")
tidy_tweets %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)

#using the spread() function to segregate the data into separate columns of positive and negative sentiments. 
#then use the mutate() function to calculate the total sentiment, that is, the difference between positive and negative sentiment.
library(tidyr)
bing <- get_sentiments("bing")
tweets_sentiment <- tidy_tweets %>%
  inner_join(bing) %>%
  count(screenName , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

View(tweets_sentiment)

#i wanted to visualize the words present based on their corresponding positive and negative scores.
ggplot(tweets_sentiment, aes(index, sentiment, fill = screenName)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~screenName, ncol = 2, scales = "free_x")

#counting the most common positive and negative words that are present in the tweets
counting_words <- tidy_tweets %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)

View(counting_words)

#i also wanted to create a word cloud that showed the most occuring negative and positive word
library(reshape2)
library(wordcloud)
tidy_tweets %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)















