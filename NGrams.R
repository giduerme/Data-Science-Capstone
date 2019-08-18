# Load the needed library packages
library(tm)
library(RWeka)
library(data.table)
library(dplyr)

# Load the three provided datasets
blogs <- readLines("en_US.blogs.txt", encoding = 'UTF-8')
news <- readLines("en_US.news.txt", encoding = 'UTF-8')
twitter <- readLines("en_US.twitter.txt", encoding = 'UTF-8')

# Set a seed for reproducibility purposes
set.seed(2019)

# Set a sample size
sample_size <- 0.03
blogs_sample <- sample(blogs, length(blogs) * sample_size)
news_sample <- sample(news, length(news) * sample_size)
twitter_sample <- sample(twitter, length(twitter) * sample_size)

# Concatenate the three sample datasets into one dataset
data_sample <- c(blogs_sample, news_sample, twitter_sample)

# Filter all non English characters
data_sample <- iconv(data_sample, "UTF-8", "ASCII", sub="")

# Create a corpora (corpus) out of the sample dataset
corpus <- VCorpus(VectorSource(data_sample))

# Undergo data cleaning
corpus <- tm_map(corpus, tolower) 
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

# Creation of the n gram tokens using a tokenize function
UnigramToken <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramToken <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramToken <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramToken <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

Unigram <- TermDocumentMatrix(corpus, control = list(tokenize = UnigramToken))
Bigram <- TermDocumentMatrix(corpus, control = list(tokenize = BigramToken))
Trigram <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramToken))
Quadgram <- TermDocumentMatrix(corpus, control = list(tokenize = QuadgramToken))

# Creation of the Rds File
UniFreq <- findFreqTerms(Unigram,lowfreq = 15)
UniFreqList <- sort(rowSums(as.matrix(Unigram[UniFreq,])), decreasing = TRUE)
UniFreqList <- data.table(word=names(UniFreqList), frequency=UniFreqList)
save(UniFreqList, file = "UnigramList.RData")

BiFreq <- findFreqTerms(Bigram,lowfreq = 25)
BiFreqList <- sort(rowSums(as.matrix(Bigram[BiFreq,])), decreasing = TRUE)
BiFreqList <- data.table(word=names(BiFreqList), frequency=BiFreqList)
save(BiFreqList, file = "BigramList.RData")

TriFreq <- findFreqTerms(Trigram,lowfreq = 10)
TriFreqList <- sort(rowSums(as.matrix(Trigram[TriFreq,])), decreasing = TRUE)
TriFreqList <- data.table(word=names(TriFreqList), frequency=TriFreqList)
save(TriFreqList, file = "TrigramList.RData")

QuadFreq <- findFreqTerms(Quadgram,lowfreq = 5)
QuadFreqList <- sort(rowSums(as.matrix(Quadgram[QuadFreq,])), decreasing = TRUE)
QuadFreqList <- data.table(word=names(QuadFreqList), frequency=QuadFreqList)
save(QuadFreqList, file = "QuadgramList.RData")


