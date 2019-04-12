setwd("~/文档/15.071x/Unit 7/Assignment 7")

#Problem 1.1
tweets = read.csv("tweets.csv",stringsAsFactors = FALSE)
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus,tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords,stopwords("english"))
dtm = DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(dtm))
ncol(allTweets)

#Problem 1.2
## It will be easier to read and understand the word cloud if it includes full words instead of just the word stems correct

#Problem 2.1
install.packages("wordcloud")
library(wordcloud)
?wordcloud
## colnames

#Problem 2.2
## colSums

#Problem 2.3
wordcloud(colnames(allTweets),colSums(allTweets),scale = c(2,0.25))
## apple

#Problem 2.4
corpus = VCorpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus, removeWords,c("apple",stopwords("english")))
freq = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(freq))
wordcloud(colnames(allTweets),colSums(allTweets),scale = c(2,0.25))
## iphone

#Problem 3.1
negativetweets = subset(tweets, Avg <= -1)
corpusNT = VCorpus(VectorSource(negativetweets$Tweet))
corpusNT = tm_map(corpusNT,tolower)
corpusNT = tm_map(corpusNT,PlainTextDocument)
corpusNT = tm_map(corpusNT,removePunctuation)
corpusNT = tm_map(corpusNT,removeWords,c("apple",stopwords("english")))
freqNT = DocumentTermMatrix(corpusNT)
allNegTweets = as.data.frame(as.matrix(freqNT))
wordcloud(colnames(allNegTweets),colSums(allNegTweets),scale = c(2,0.25))
## Word Cloud C

#Problem 3.2
## Word Cloud A

#Problem 3.3
wordcloud(colnames(allTweets),colSums(allTweets),scale = c(2,0.25),random.order = FALSE)
## Word Cloud B/D

#Problem 3.4
wordcloud(colnames(allNegTweets),colSums(allNegTweets),scale = c(2,0.25))
wordcloud(colnames(allNegTweets),colSums(allNegTweets),scale = c(2,0.25),rot.per = 0.5)
## Word Cloud A

#Problem 3.5
## Word Cloud D

#Problem 4.1
?brewer.pal
## YlOrRd correct

#Problem 4.2
display.brewer.all()
## Greys

#Problem 4.3
## brewer.pal(9, "Blues")[c(-1, -2, -3, -4)]
## brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]









