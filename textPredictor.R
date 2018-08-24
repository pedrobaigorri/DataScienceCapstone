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

sampling <- 0.5
console.log("Reading the files with a 0.5 of sampling") 
myData <- importTrainingTestCorpora(sampling)

console.log("Obtaining the nGrams")
nGrams <- getNGramsTrain(myData, 2, 2, 5)

sentence <- "day off from"

console.log("running the prediction") 
prediction <- textPredictor(sentence, nGrams)
head(prediction, 10)

console.log("running Quizz2")
answers2 <- quizz2(sentence, nGrams)

console.log("running Quizz3")
answers3 <- quizz3(sentence, nGrams)
