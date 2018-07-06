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
    text_filter(corpusList$twitter)$drop_symbol = TRUE
    text_filter(corpusList$twitter)$drop_number = TRUE
    text_filter(corpusList$twitter)$drop_punct = TRUE
    
    text_filter(corpusList$blogs)$drop_symbol = TRUE
    text_filter(corpusList$blogs)$drop_number = TRUE
    text_filter(corpusList$blogs)$drop_punct = TRUE
    
    text_filter(corpusList$news)$drop_symbol = TRUE
    text_filter(corpusList$news)$drop_number = TRUE
    text_filter(corpusList$news)$drop_punct = TRUE
    
    return (corpusList)
}

###
#
# importDataSet function to import the dataset into a list of R objects
#
importDataSet <- function()
{
    #file1 <- "./Data/final/en_US/sample1.txt"
    #file2 <- "./Data/final/en_US/sample2.txt"
    #file3 <- "./Data/final/en_US/sample2.txt"
    
    file1 <- "~/Data/final/en_US/en_US.twitter.txt"
    file2 <- "~/Data/final/en_US/en_US.blogs.txt"
    file3 <- "~/Data/final/en_US/en_US.news.txt"
    
    if (!file.exists("dataset.RData"))
    {
        rFile1 <- readFile(file1)
        rFile2 <- readFile(file2)
        rFile3 <- readFile(file3)
    
        save(rFile1, rFile2, rFile3, file = "dataset.RData")
        
    }else load("dataset.RData")
    
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
