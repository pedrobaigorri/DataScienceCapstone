library(ngram)
str <- "A B A C A B B"
ng <- ngram(str)
get.ngrams(ng)


ng <- ngram(str, n = 3, sep = " ")
get.ngrams(ng)


ng <- ngram_asweka(str, min=2, max=4)
ng


words <- c("a", "b", "c")
words
wordcount(words)
str <- concatenate(words, collapse="")
str
wordcount(str)

library(ngram)
str <- "A B A C A B B"
ng <- ngram(str)
get.phrasetable(ng)

ng <- ngram(dataSample)
ngram2 <- get.phrasetable(ng)

head(ngram2[order(-ngram2$freq),])

us_twitter2 <- enc2utf8(us_twitter)
ng <- ngram(us_twitter2)
ngram2 <- get.phrasetable(ng)
head(ngram2[order(-ngram2$freq),])

head(df_twitter[order(df_twitter$length),], 1)$length


library(corpus)
corpus <- as_corpus_text(us_twitter)
text_filter(corpus)$drop_punct <- TRUE # ignore punctuation
term_stats(corpus, ngrams = 2:3)





term_stats("A rose is a rose is a rose.")
# remove punctuation and English stop words
term_stats("A rose is a rose is a rose.",
           text_filter(drop_symbol = TRUE, drop = stopwords_en))
# unigrams, bigrams, and trigrams
term_stats("A rose is a rose is a rose.", ngrams = 1:3)
# also include the type information
term_stats("A rose is a rose is a rose.", ngrams = 1:3, types = TRUE)

corpus <- as_corpus_text("A rose is a rose is a rose.")
text_filter(corpus)$drop_punct <- TRUE # ignore punctuation
term_stats(corpus, ngrams = 1:3)

summary(corpus)




clean_text <- function(text)
{ 
    # Helper function to preprocess corpus 
    text <- tm_map(text, content_transformer(tolower)) 
    text <- tm_map(text, removeNumbers) 
    text <- tm_map(text, removePunctuation) 
    text <- tm_map(text, stripWhitespace) 
    text <- tm_map(text, removeWords, stopwords("english")) 
    text <- tm_map(text, removeWords, profanities) return(text) 
}

text <- "I AM hoohoh hola 3. ?"
corpus <- as_corpus_text(text)
text_filter(corpus) <- NULL
term_stats(corpus)


library(tm)

corp <- Corpus(VectorSource(text))
corp2 <- tm_map(corp, removeWords, stopwords("english"))
corp3 <- tm_map(corp, removeNumbers)

DocumentTermMatrix(corp)


setwd("D:/GIT_REPOSITORY/DataScienceCapstone")
source(".//tfTools.R")

dataset <- importDataSet()


