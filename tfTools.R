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
