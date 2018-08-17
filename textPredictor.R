###############################################################################
#
#  File with the code to create the text predictor for the Data Science Capstone
#  project. 
#
#  Author: Pedro A. Alonso Baigorri
#
#  Date: 20/06/2018
#
###############################################################################

setwd("D:/GIT_REPOSITORY/DataScienceCapstone")

source("tfTools.R")
source("runQuizzs.R")

# Reading the files with a 50% of sampling
myData <- importTrainingTestCorpora(0.5)

# Obtaining the nGrams
nGrams <- getNGramsTrain(myData, 2)

sentence <- "day off from"

# running the prediction
prediction <- textPredictor(sentence, nGrams)

# running Quizz2
quizz2(sentence, nGrams)
