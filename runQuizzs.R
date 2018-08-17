###############################################################################
#
#  File with the code to run the Quizzs
#
#  Author: Pedro A. Alonso Baigorri
#
#  Date: 17/08/2018
#
###############################################################################


quizz2 <- function(sentence, nGrams)
{
    
    print("Question 1")
    sentence <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)
    
    print("Question 2")
    sentence <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)
    
    print("Question 3")
    sentence <- "Hey sunshine, can you follow me and make me the"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)
    
    print("Question 4")
    sentence <- "Very early observations on the Bills game: Offense still struggling but the"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)
    
    print("Question 5")
    sentence <- "Go on a romantic date at the"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)
    
    print("Question 6")
    sentence <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)
    
    print("Question 7")
    sentence <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)
    
    print("Question 8")
    sentence <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)
    
    print("Question 9")
    sentence <- "Be grateful for the good times and keep the faith during the"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)
    
    print("Question 10")
    sentence <- "If this isn't the cutest thing you've ever seen, then you must be"
    s <- textPredictor(sentence, nGrams)
    head(s, 10)    
    
}

