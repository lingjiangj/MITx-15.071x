setwd("~/文档/15.071x/Unit5/Assignment5")

# Problem 1.1
wiki <- read.csv("wiki.csv",stringsAsFactors = FALSE)
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)

#Problem 1.2
library(tm)
corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded,removeWords,stopwords("english"))
corpusAdded <- tm_map(corpusAdded,stemDocument)
dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

#Problem 1.3
sparseAdded <- removeSparseTerms(dtmAdded,0.997)
sparseAdded

#Problem 1.4
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A",colnames(wordsAdded))
corpusRemoved = VCorpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
dtmRemoved
ncol(wordsRemoved)

#Problem 1.5
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
library(caTools)
set.seed(123)
split <- sample.split(wikiWords$Vandal,SplitRatio = 0.7)
wikiTrain <- subset(wikiWords,split == TRUE)
wikiTest <- subset(wikiWords, split == FALSE)
table(wikiTest$Vandal)
table(wikiTest$Vandal)[1]/nrow(wikiTest)

#Problem 1.6
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal~., data=wikiTrain, method="class")
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")
table(wikiTest$Vandal, testPredictCART)
(618+12)/(618+533+12) 

#Problem 1.7    
prp(wikiCART)

#Problem 2.1
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

#Problem 2.2
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
wikiCART2 = rpart(Vandal~., data = wikiTrain2,method = "class")
testPredictCART2 = predict(wikiCART2,newdata = wikiTest2,type = "class")
table(wikiTest2$Vandal,testPredictCART2)
(609+57)/nrow(wikiTest2)

#Problem 2.3
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)

#Problem 2.4
wikiTrain3 <- subset(wikiWords2,split == TRUE)
wikiTest3 <- subset(wikiWords2,split == FALSE)
wikiCART3 <- rpart(Vandal~., data = wikiTrain3, method = "class")
testPredictCART3 <- predict(wikiCART3, newdata = wikiTest3,type = "class")
table(wikiTest3$Vandal,testPredictCART3)
(514+248)/nrow(wikiTest3)

#Problem 3.1
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 <- subset(wikiWords3,split == TRUE)
wikiTest4 <- subset(wikiWords3,split == FALSE)
wikiCART4 <- rpart(Vandal~., data = wikiTrain4,method = "class")
testPredictCART4 <- predict(wikiCART4,newdata = wikiTest4,type = "class")
table(wikiTest4$Vandal, testPredictCART4)
(595+241)/nrow(wikiTest4)

#Problem 3.2
prp(wikiCART4)













































































































