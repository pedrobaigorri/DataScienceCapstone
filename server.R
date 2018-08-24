#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Author Pedro A. Alonso Baigorri

library(shiny)
library(ggplot2)
source("tfTools.R")


#reading and preparing data

console.log("Obtaining the nGrams")
load("Model/trainNGrams.RData")
console.log("nGrams obtained with %d rows", nrow(nGrams))

# function to predict the ranking through the model
predictText <- function (input, nGrams)
{
    
    sentence <- input$sentence
    
    if (sentence == "")
    {
        return(NULL)
    }
    
    console.log("in the function")
    console.log("sentence: %s", sentence)
    n <- input$max_results
    
    p <- textPredictor(sentence, nGrams)
    
    console.log("sentence: %s", sentence)
    return (head(p[,c("prediction", "index")], n))
    
}

# shiny server function
shinyServer(function(input, output) {


    # in the prediction tab this functions shows a text with the result of the prediction
    output$result <- renderTable({
        
        console.log("before predictText")
        console.log("sentence: %s", input$sentence)
        p <- predictText(input, nGrams)
        
        console.log("Predicted %d results", nrow(p))
        console.log("Predictions: %s, id: %d", p$prediction, p$index)
        
        if (is.null(p)) return (data.frame("prediction"=""))
        
        return (p)
        
    })
    

})
