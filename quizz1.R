###############################################################################
#
#  File with the code to solve the Quizz1 of the Capstone Project
#
#  Author: Pedro A. Alonso Baigorri
#
#  Date: 20/06/2018
#
###############################################################################

setwd("D:/GIT_REPOSITORY/DataScienceCapstone")
source(".//tfTools.R")

# Sample for testing
fileSample <- "./Data/final/en_US/sample.txt"
dataSample <- readFile(fileSample)
df_sample <- processFile(dataSample)
length(grep("new", df_sample$line))


# reading the rest of the files

file <- "./Data/final/en_US/en_US.twitter.txt"
us_twitter <- readFile(file)
df_twitter <- processFile(us_twitter)

file <- "./Data/final/en_US/en_US.blogs.txt"
us_blogs <- readFile(file)
df_blogs <- processFile(us_blogs)


file <- "./Data/final/en_US/en_US.news.txt"
us_news <- readFile(file)
df_news <- processFile(us_news)

# QUESTION 2: The en_US.twitter.txt has how many lines of text?

length(us_twitter)

# QUESTION 3: What is the length of the longest line seen in any of the three en_US data sets?

head(df_blogs[order(-df_blogs$length),], 1)$length
head(df_news[order(-df_news$length),], 1)$length
head(df_twitter[order(-df_twitter$length),], 1)$length

# QUESTION 4 In the en_US twitter data set, if you divide the number of lines 
# where the word "love" (all lowercase) occurs by the number of lines the word
# "hate" (all lowercase) occurs, about what do you get?

length(grep("love", df_twitter$line))/length(grep("hate", df_twitter$line))

# QUESTION 5: The one tweet in the en_US twitter data set that matches the word 
# "biostats" says what?

df_twitter[grep ("biostats", df_twitter$line),]


# QUESTION 6: How many tweets have the exact characters 

sentence <- "A computer once beat me at chess, but it was no match for me at kickboxing"

df_twitter[grep (sentence, df_twitter$line),]
