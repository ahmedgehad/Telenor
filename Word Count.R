library(tm)
library(wordcloud)
#Get Word Count in the original Data
AirplaneCrashesSince1908 <- read.csv("3-Airplane_Crashes_Since_1908.txt", stringsAsFactors = FALSE)
summary_text <- paste(AirplaneCrashesSince1908$Summary, collapse=" ")
summary_source <- VectorSource(summary_text)
corpus <- Corpus(summary_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
stopwords("english")
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency, 100)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])