###################################################
#ONE:
#  1) Extract tweets for any user (try choosing a user who has more tweets)
#  2) Perform sentimental analysis on the tweets extracted from the above

install.packages("twitteR")
install.packages("ROAuth")
library(twitteR)
library(ROAuth)
cred <- OAuthFactory$new(consumerKey='y35MQKmupT92QN3zVnnNtovzn', # Consumer Key (API Key)
                         consumerSecret='ThSZxjsy0hDCJWIgUvir2AaxnsPHLFO51zDIZZjXflTdDkEfZy', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")
install.packages("base64enc")
library(base64enc)
install.packages("httpuv")
library(httpuv)
setup_twitter_oauth("y35MQKmupT92QN3zVnnNtovzn", # Consumer Key (API Key)
                    "ThSZxjsy0hDCJWIgUvir2AaxnsPHLFO51zDIZZjXflTdDkEfZy", #Consumer Secret (API Secret)
                    "1233645453930942464-23MwznsISI6wK6CrLBwFKQX18suXw2",  # Access Token
                    "BBeVtpsylosLKWpYffUWSDZ8JoYZDQ2LtVJVhhjD52Oe6")  #Access Token Secret
#registerTwitterOAuth(cred)

Tweets <- userTimeline('rajeshvanka', n = 100,includeRts = T)
tweetsDF <- twListToDF(Tweets)
write.csv(TweetsDF, "RajeshTweets.csv")
getwd()

# CLEANING TWEETS

tweetsDF$text=gsub("&amp", "", tweetsDF$text)
tweetsDF$text = gsub("&amp", "", tweetsDF$text)
tweetsDF$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetsDF$text)
tweetsDF$text = gsub("@\\w+", "", tweetsDF$text)
tweetsDF$text = gsub("[[:punct:]]", "", tweetsDF$text)
tweetsDF$text = gsub("[[:digit:]]", "", tweetsDF$text)
tweetsDF$text = gsub("http\\w+", "", tweetsDF$text)
tweetsDF$text = gsub("[ \t]{2,}", "", tweetsDF$text)
tweetsDF$text = gsub("^\\s+|\\s+$", "", tweetsDF$text)

tweetsDF$text <- iconv(tweetsDF$text, "UTF-8", "ASCII", sub="")
install.packages("Rcurl")
library(RCurl)
install.packages("httr")
library(httr)
install.packages("tm")
library(tm)
install.packages("worldcloud")
library(wordcloud)
install.packages("syuzhet")
library(syuzhet)

# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(tweetsDF$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])


wordcloud_tweet = c(
  paste(tweetsDF$text[emotions$anger > 0], collapse=" "),
  paste(tweetsDF$text[emotions$anticipation > 0], collapse=" "),
  paste(tweetsDF$text[emotions$disgust > 0], collapse=" "),
  paste(tweetsDF$text[emotions$fear > 0], collapse=" "),
  paste(tweetsDF$text[emotions$joy > 0], collapse=" "),
  paste(tweetsDF$text[emotions$sadness > 0], collapse=" "),
  paste(tweetsDF$text[emotions$surprise > 0], collapse=" "),
  paste(tweetsDF$text[emotions$trust > 0], collapse=" ")
)

# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

# create document term matrix

tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

##################################################################################
#TWO:
#  1) Extract reviews of any product from ecommerce website like snapdeal and amazon
#  2) Perform sentimental analysis


install.packages("rvest")
library(rvest)
install.packages("XML")
library(XML)
install.packages("magrittr")
library(magrittr)
# Amazon Reviews #############################

aurl <- "https://www.amazon.com/product-reviews/B07T7MM64L/ref=acr_search_hist_5?ie=UTF8&filterByStar=five_star&reviewerType=all_reviews#reviews-filter-bar"
amazon_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="")))
  rev <- murl %>%
    html_nodes(".cr-original-review-content") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}

write.table(amazon_reviews,"miMobilereviews.txt",row.names=FALSE)
write.csv(amazon_reviews,file = "miMobilereviews.csv")
getwd()
library(syuzhet)
library(plotly)
library(tm)
ratingsReview  = readLines("C:/Users/RADHIKA/Documents/miMobilereviews.txt")
rview <- get_sentences(ratingsReview)

syuzhet <- get_sentiment(rview, method="syuzhet")
bing <- get_sentiment(rview, method="bing")
afinn <- get_sentiment(rview, method="afinn")
nrc <- get_sentiment(rview, method="nrc")
sentiments <- data.frame(syuzhet, bing, afinn, nrc)

#anger", "anticipation", "disgust", "fear", "joy", "sadness", 
#"surprise", "trust", "negative", "positive."
emotions <- get_nrc_sentiment(ratingsReview)
head(emotions)
emo_bar = colSums(emotions)
barplot(emo_bar)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))

plot(syuzhet, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")

# To extract the sentence with the most negative emotional valence
negative <- rview[which.min(syuzhet)]

# and to extract the most positive sentence
positive <- rview[which.max(syuzhet)]

###########################################################################################
#THREE:
#  1) Extract movie reviews for any movie from IMDB and perform sentimental analysis
#  2) Extract anything you choose from the internet and do some research on how we extract using R
#       Programming and perform sentimental analysis.
#

#IMDB review Extraction

imbdurl<- "https://www.imdb.com/title/tt8367814/reviews?ref_=tt_sa_3"
imbd_reviews<-NULL
for(i in 0:6){
  url<-read_html(as.character(paste(imbdurl,i*a,sep="")))
  imbdr<-url %>%
    html_nodes("#rw5373246 div+ p") %>%
    html_text() 
  imbd_reviews<-c(imbd_reviews,imbdr)
}
imdbReview  = readLines("C:/Users/RADHIKA/Documents/moviereviews.txt")
mview <- get_sentences(imdbReview)

syuzhet <- get_sentiment(mview, method="syuzhet")
bing <- get_sentiment(mview, method="bing")
afinn <- get_sentiment(mview, method="afinn")
nrc <- get_sentiment(mview, method="nrc")
sentiments <- data.frame(syuzhet, bing, afinn, nrc)

#anger", "anticipation", "disgust", "fear", "joy", "sadness", 
#"surprise", "trust", "negative", "positive."
emotions <- get_nrc_sentiment(imdbReview)
head(emotions)
emo_bar = colSums(emotions)
barplot(emo_bar)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))

plot(syuzhet, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")

# To extract the sentence with the most negative emotional valence
negative <- mview[which.min(syuzhet)]

# and to extract the most positive sentence
positive <- mview[which.max(syuzhet)]


######################################################################################
install.packages("NLP")
install.packages("deployr")
library(NLP)
textpath = readLines("C:/Users/RADHIKA/Desktop/Artificial_intelligence.txt")
head(textpath)
length(textpath)

text<-paste(readLines("C:/Users/RADHIKA/Desktop/Artificial_intelligence.txt"),collapse = " ")
text2<-gsub(pattern="\\W",replace = " ",text)#remove punctuations
gsub(pattern = "\\d",replace=" ",text2)#remove digists
text2<-tolower(text2)#lowercase
text2
library(tm)
stopwords()
removeWords(text2,stopwords())#remove stopwords
gsub(pattern = "\\b[A-z]\\b(1)",replace=" ",text2)
stripWhitespace(text2)#remove spaces
install.packages("stringr")
library(stringr)
library(wordcloud)
textbagofwords<-str_split(text2,pattern="\\s+")
class(textbagofwords)#list
textbagofwords<-unlist(textbagofwords)
class(textbagofwords)#character
textbagofwords
str(textbagofwords)
library(plyr)
library(twitteR)
library(syuzhet)
library(plotly)
######Positive and Negative wordclouds########
setwd("D:/ExcelR Data/Assignments/Text Mining")
getwd()
poswords <- scan("positive-words.txt",what="character",comment.char=";")
negwords <- scan("negative-words.txt",what="character",comment.char=";")


match(textbagofwords,poswords)
is.na(match(textbagofwords,poswords))
sum(!is.na(match(textbagofwords,poswords)))#32
sum(!is.na(match(textbagofwords,negwords)))#5
score<-sum(!is.na(match(textbagofwords,poswords)))-sum(!is.na(match(textbagofwords,negwords)))
score#27
wordcloud(textbagofwords)
wordcloud(textbagofwords,min.freq = 4)
wordcloud(textbagofwords,min.freq = 4,random.order = FALSE,scale = c(3,0,5),color=rainbow(3))
wordcloud(poswords)
wordcloud(poswords,min.freq = 4,random.order = FALSE,scale=c(3,0,5),color=rainbow(3))
wordcloud(negwords)
wordcloud(negwords,min.freq = 4,random.order = FALSE,scale=c(3,0,5),color=rainbow(3))

##################################################################################


















































































































































































































































































































































































