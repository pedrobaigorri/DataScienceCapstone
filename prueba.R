###
#
# getNGrams - function to create the ngrams from the text
#
getNGrams <- function(dataset, bkFile)
{
    
    require(corpus)
    
    if (!file.exists(bkFile))
    {
        my_corpus <- as_corpus_text(dataset)
        
        text_filter(my_corpus)$drop_symbol = TRUE
        text_filter(my_corpus)$drop_number = TRUE
        text_filter(my_corpus)$drop_punct = TRUE
        
        nGrams <- term_stats(my_corpus, ngrams = 2:5, min_count = 2, types = TRUE)
        
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
    file1 <- "./Data/final/en_US/sample1.txt"
    file2 <- "./Data/final/en_US/sample2.txt"
    file3 <- "./Data/final/en_US/sample2.txt"
    
    file1 <- "./Data/final/en_US/en_US.twitter.txt"
    file2 <- "./Data/final/en_US/en_US.blogs.txt"
    file3 <- "./Data/final/en_US/en_US.news.txt"
    
    
    bkFile <- "datasetTrainTest.RData"
    
    if (!file.exists(bkFile))
    {
        rFile1 <- readFile(file1)
        rFile2 <- readFile(file2)
        rFile3 <- readFile(file3)
        
        set.seed(1000)

        for (i in c("file1", "file2", "file3"))
        {
            
            
            
        }
        fileList <- list("file1", "file2", )
                
        my_sample <- sample(length(rFileFull), round(length(rFileFull)*trainSample))
        rFileTrain <- rFileFull[my_sample]
        rFileTest <- rFileFull[-my_sample]
        
        save(rFileTrain, rFileTest, file = bkFile)
        
    }else load(bkFile)
    
    newList <- list("train" = rFileTrain, "test" = rFileTest)
    return (newList)
}