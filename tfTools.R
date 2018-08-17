###############################################################################
#
#  File with functions for capstone project. It contains some functions to 
#  manipulate text files from R
#
#  Author: Pedro A. Alonso Baigorri
#
#  Date: 20/06/2018
#
###############################################################################

setwd("D:/GIT_REPOSITORY/DataScienceCapstone")

library(stringr)

###
#
# removeLastWord - 
#
# This function gives a list with potential predictions from a sentence
#
removeLastWord <- function(sentence)
{
    nWords <- getNumberOfWords(sentence)
    
    if (nWords > 1)
    {
        output <- word(sentence, start = 1, end = nWords - 1)
    }
    else{
        
        output <- sentence
    }
    
    return (output)
}

###
#
# textPredictor - 
#
# This function gives a list with potential predictions from a sentence
#

textPredictor <- function(sentence, nGrams)
{
    # calculate the maximum number of N-Grams to use depending on the nwords
    # of the sentence
    nWords <- getNumberOfWords(sentence)    

    m <- nWords + 1
    
    if (m > 5){
        m <- 5
    }
    
    wProbs <- c(0.1, 0.2, 0.4, 0.8)
    
     # loop to call the predictionFromNGrams for each length of ngram
    for (i in 2:m){
        
        prediction <- getPredictionFromNGram(sentence, nGrams, i)
        
        # setting probability weights
        prediction$wProb <- prediction$probability * wProbs[i - 1]
        
        if (i < 3){
            prediction2 <- prediction
        }
        else{
            prediction2 <- rbind(prediction2, prediction)
            
        }

    }
    

    # Agreggating the results per prediction in order to increase the probability
    # of predictions provided by different n-grams
    output <- aggregate(wProb ~ prediction, data = prediction2, FUN = sum)
    
    
    # ordering results by the probability
    output <- output[order(-output$wProb),]
    
    return (output[complete.cases(output),])
    
 
}

###
#
# getNumberofWords - 
#

getNumberOfWords <- function(sentence)
{

    return (str_count(sentence, boundary("word")))
    
}



###
#
# getPredictionFromNGram - 
#
# This function gives a list with potential predictions of the next word from 
# a given sentence with its probability within the current ngrams length
#
# Parameters:
#
# - sentence
# - nGrams from train
# - nGramLen
#
# Output:
#
# - a dataframe with a list of potential words with the following fields:
#   * prediction
#   * probability
#   * nGramLen

getPredictionFromNGram <- function(sentence, nGrams, nGramLen)
{
    #subsetting the ngrams with the given nGramLen
    sGrams <- nGrams[!is.na(nGrams[, nGramLen + 1]),]
    
    # keeping only the rows with the NGramLen selected
    if (nGramLen != 5)
        sGrams <- sGrams[is.na(sGrams[, nGramLen + 2]),]
    
    # get the first nGramLen - 1 words from term
    # sGrams$cutTerm <- word(sGrams$term, start = 1, end = nGramLen - 1)
    
    # getting the last nGramLen - 1 words from sentence
    nWords <- str_count(sentence, boundary("word"))
    cutSentence <- word(sentence, start = nWords - nGramLen + 2, end = nWords )
   
    # find rows that comparison is ok
    pGrams <- sGrams[str_detect(sGrams$cutTerm, cutSentence), ]
    pGrams <- pGrams[, c(nGramLen + 1, 7)]
    pGrams <- pGrams[complete.cases(pGrams), ]
    
    # calculation of probabilitty
    totalprob <- 0
    totalprob <- sum(pGrams$count)
    pGrams$prob <- pGrams$count/totalprob
    
    # returning the dataframe with the output
    
    if (totalprob > 0)
    {
        output <- pGrams[, c(1, 3)]
        output$nGramLen <- nGramLen
        colnames(output) <- c("prediction", "probability", "nGramLen")    
    }
    else
    {
        output <- data.frame(prediction = "", probability = 0, nGramLen = nGramLen)
    }
        
    return(output)
    
}



###
#
# getNGramsTrain - function to create the ngrams from the text for
# training
#
getNGramsTrain <- function(inputData, mincount)
{
    bkFile <- "trainNGrams.RData"
    
    output <- getNGrams(inputData, mincount, bkFile, 1)
    
    return (output)
}
 

###
#
# getNGramsTest - function to create the ngrams from the text
# for testing
#
getNGramsTest <- function(inputData, mincount)
{
    bkFile <- "testNGrams.RData"
    
    output <- getNGrams(inputData, mincount, bkFile, 0)
    
    return(output)
}   

###
#
# getNGrams - function to create the ngrams from the text
# for testing or training
#
getNGrams <- function(inputData, mincount, bkFile, train_test)
{
    
    require(corpus)
    
    if (!file.exists(bkFile))
    {
        #twitter
        t <- inputData$twitter
        
        my_corpus <- as_corpus_text(t[train_test])
        
        text_filter(my_corpus)$drop_symbol = TRUE
        text_filter(my_corpus)$drop_number = TRUE
        text_filter(my_corpus)$drop_punct = TRUE
        
        nGramsTwitter <- term_stats(my_corpus, ngrams = 2:5, min_count = mincount, types = TRUE)
        
        #blogs
        t <- inputData$blogs

        my_corpus <- as_corpus_text(t[train_test])
        
        text_filter(my_corpus)$drop_symbol = TRUE
        text_filter(my_corpus)$drop_number = TRUE
        text_filter(my_corpus)$drop_punct = TRUE
        
        nGramsBlogs <- term_stats(my_corpus, ngrams = 2:5, min_count = mincount, types = TRUE)
        
        #news
        t <- inputData$news

        my_corpus <- as_corpus_text(t[train_test])
        
        text_filter(my_corpus)$drop_symbol = TRUE
        text_filter(my_corpus)$drop_number = TRUE
        text_filter(my_corpus)$drop_punct = TRUE
        
        nGramsNews <- term_stats(my_corpus, ngrams = 2:5, min_count = mincount, types = TRUE)
        
        nGrams <- rbind(nGramsTwitter, nGramsBlogs, nGramsNews)
        
        
        # removing last word
        nGrams$cutTerm <- removeLastWord(nGrams$term)
        
        save(nGrams, file = bkFile)
        
    }else load(bkFile)
    
    return (nGrams)
        
}


###
#
# importTrainingTestCorpora function to import the dataset into train/test of R 
# objects with a sample of data 
#
importTrainingTestCorpora <- function(trainSample)
{
    dataset <- importDataSet()
    set.seed(1000)

    
    #twitter
    rFileFull <- dataset$twitter
    my_sample <- sample(length(rFileFull), round(length(rFileFull)*trainSample))
    rFileTrain <- rFileFull[my_sample]
    rFileTest <- rFileFull[-my_sample]
    twitterList <- list("train" = rFileTrain, "test" = rFileTest)
    
    #blogs
    rFileFull <- dataset$blogs
    my_sample <- sample(length(rFileFull), round(length(rFileFull)*trainSample))
    rFileTrain <- rFileFull[my_sample]
    rFileTest <- rFileFull[-my_sample]
    blogsList <- list("train" = rFileTrain, "test" = rFileTest)
    
    #news
    rFileFull <- dataset$blogs
    my_sample <- sample(length(rFileFull), round(length(rFileFull)*trainSample))
    rFileTrain <- rFileFull[my_sample]
    rFileTest <- rFileFull[-my_sample]
    newsList <- list("train" = rFileTrain, "test" = rFileTest)
    
    outputList <- list("twitter" = twitterList, "blogs" = blogsList, "news" = newsList)
    return (outputList)
}

###
#
# getCleanedCorpus function get sample of the datasets, convert them into
# a corpus and cleaned them.
#
# - dataset = the list with the text files
# - p_sample = the % of samples
#
getCleanedCorpus <- function(dataset, p_sample)
{
    set.seed(1000)
    
    # sample the datasets
    tw_sampled <- dataset$twitter[sample(length(dataset$twitter), round(length(dataset$twitter)*p_sample))]
    bl_sampled <- dataset$blogs[sample(length(dataset$blogs), round(length(dataset$blogs)*p_sample))]
    nw_sampled <- dataset$news[sample(length(dataset$news), round(length(dataset$news)*p_sample))]
    
    #create the corpus list
    require(corpus)
    
    corpusList = list("twitter", "blogs", "news")
    
    corpusList$twitter <- as_corpus_text(tw_sampled)
    corpusList$blogs <- as_corpus_text(bl_sampled)
    corpusList$news <- as_corpus_text(nw_sampled)
    
    # clean and filter
    #text_filter(corpusList$twitter)$drop_symbol = TRUE
    #text_filter(corpusList$twitter)$drop_number = TRUE
    #text_filter(corpusList$twitter)$drop_punct = TRUE
    
    #text_filter(corpusList$blogs)$drop_symbol = TRUE
    #text_filter(corpusList$blogs)$drop_number = TRUE
    #text_filter(corpusList$blogs)$drop_punct = TRUE
    
    #text_filter(corpusList$news)$drop_symbol = TRUE
    #text_filter(corpusList$news)$drop_number = TRUE
    #text_filter(corpusList$news)$drop_punct = TRUE
    
    return (corpusList)
}

###
#
# importDataSet function to import the dataset into a list of R objects
#
importDataSet <- function()
{
    bkFile <- "corpora.RData"
    
    file1 <- "./Data/final/en_US/sample1.txt"
    file2 <- "./Data/final/en_US/sample2.txt"
    file3 <- "./Data/final/en_US/sample2.txt"
    
    file1 <- "./Data/final/en_US/en_US.twitter.txt"
    file2 <- "./Data/final/en_US/en_US.blogs.txt"
    file3 <- "./Data/final/en_US/en_US.news.txt"
    
    if (!file.exists(bkFile))
    {
        rFile1 <- readFile(file1)
        rFile2 <- readFile(file2)
        rFile3 <- readFile(file3)
    
        save(rFile1, rFile2, rFile3, file = bkFile)
        
    }else load(bkFile)
    
    newList <- list("twitter" = rFile1, "blogs" = rFile2, "news" = rFile3)
    return (newList)
}



####
# 
# readFile: function to read a full text file 
#
readFile <- function(filePath)
{
    file <- readLines(filePath)
    return (file)
}

####
#
# processFile: convert the file into a dataframe with some
# metada associated
# 
processFile <- function(vectorLines)
{
    characters <- sapply(vectorLines, nchar, simplify = TRUE, USE.NAMES = FALSE)
    
    df <- data.frame("line" = vectorLines, "length" = characters)
    
    return (df)
}
