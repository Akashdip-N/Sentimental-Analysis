# Amazon Alexa Reviews

library(tm) 
library(wordcloud)
library(syuzhet)
library(RColorBrewer)
library(tidytext)
library(ggplot2)
library(ggthemes)
library(SnowballC)

# importing data to R
reviews <- read.csv("amazon_alexa.csv")

ggplot(data = reviews, aes(x=rating))+ geom_bar(fill = "sienna") + theme_economist()

# Checking the structure of file
str(reviews)

is.na(reviews)

summary(reviews)

# This function uses the base package function iconv to translate labels into a specific encoding
corpus <- iconv(reviews$verified_reviews)
corpus <- Corpus(VectorSource(corpus))

# To see the corpus for the first 5 reviews
inspect(corpus[1:5])

# data cleaning done in corpus
corpus <- tm_map(corpus, tolower) # converting to lower case every aplhabet
corpus <- tm_map(corpus, removePunctuation) # removing punctuation
corpus <- tm_map(corpus, removeNumbers) # removing numbers from the data
corpus <- tm_map(corpus, stripWhitespace) # removing whitespace from the data
corpus <- tm_map(corpus, removeWords, c('didnt','its','got','cant','etc','im','ive','can'))
inspect(corpus[1:5])

# here we are removing the stop words
stopwords()
stopwords("english")
stopwords <- c(stopwords("english"))
stopwords

wordcloud(corpus)

# creating the final variable
reviews_final <- corpus

# Creating term document matrix
tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d,10)
termFreq <- rowSums(as.matrix(tdm))
tdm

termfreqsubset <- subset(termFreq, termFreq >= 50)
termfreqsubset

# Bar plot for most frequent words
w <- rowSums(tdm)
w <- subset(w, w >= 200) # here we are creating a bar plot where the words count is more than 200
barplot(w, las = 2, col = "blue", main = "Most Frequent Words", ylab = "Word Frequency")

# Word Cloud
w <- sort(rowSums(tdm), decreasing = T)
set.seed(1234)
wordcloud(
    words = names(w),
    freq = w,
    max.words = 200,
    random.order = FALSE,
    min.freq = 5,
    colors = brewer.pal(8,"Dark2"),
    scale = c(3, 0.5)
    )

# Obtaining sentiment scores
sentiment_data <- iconv(reviews$verified_reviews)
s <- get_nrc_sentiment(sentiment_data) # gives us scores based on few sentiments
head(s)

# calculate review wise score
s$score <- s$positive - s$negative
head(s)

# checking product sentiment
# check overall sentiment of the product
review_score <- colSums(s[,])
print(review_score)

# Bar Plot for product sentiment
barplot(colSums(s), las=2, col=rainbow(10),ylab='Count',main='Sentiment')

# Sentiment Analysis
pos.words <- scan(file='positive-words.txt', what='character')
neg.words <- scan(file = 'negative-words.txt', what='character')

