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
    s1 <- textPredictor(sentence, nGrams)
    print(head(s1, 20))
    
    print("Question 2")
    sentence <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
    s2 <- textPredictor(sentence, nGrams)
    print(head(s2, 20))
    
    print("Question 3")
    sentence <- "Hey sunshine, can you follow me and make me the"
    s3<- textPredictor(sentence, nGrams)
    print(head(s3, 20))
    
    print("Question 4")
    sentence <- "Very early observations on the Bills game: Offense still struggling but the"
    s4 <- textPredictor(sentence, nGrams)
    print(head(s4, 20))
    
    print("Question 5")
    sentence <- "Go on a romantic date at the"
    s5 <- textPredictor(sentence, nGrams)
    print(head(s5, 20))
    
    print("Question 6")
    sentence <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
    s6 <- textPredictor(sentence, nGrams)
    print(head(s6, 20))
    
    print("Question 7")
    sentence <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
    s7 <- textPredictor(sentence, nGrams)
    print(head(s7, 20))
    
    print("Question 8")
    sentence <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
    s8 <- textPredictor(sentence, nGrams)
    print(head(s8, 20))
    
    print("Question 9")
    sentence <- "Be grateful for the good times and keep the faith during the"
    s9 <- textPredictor(sentence, nGrams)
    print(head(s9, 20))
    
    print("Question 10")
    sentence <- "If this isn't the cutest thing you've ever seen, then you must be"
    s10 <- textPredictor(sentence, nGrams)
    print(head(s10, 20))
    
    output = list("s1" = s1,
                  "s2" = s2,
                  "s3" = s3,
                  "s4" = s4,
                  "s5" = s5,
                  "s6" = s6,
                  "s7" = s7,
                  "s8" = s8,
                  "s9" = s9,
                  "s10" = s10)
    
    return(output)
    
}


quizz3 <- function(sentence, nGrams)
{
    
    print("Question 1")
    sentence <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
    s1 <- textPredictor(sentence, nGrams)
    print(head(s1, 20))
    
    print("Question 2")
    sentence <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
    s2 <- textPredictor(sentence, nGrams)
    print(head(s2, 20))
    
    print("Question 3")
    sentence <- "I'd give anything to see arctic monkeys this"
    s3<- textPredictor(sentence, nGrams)
    print(head(s3, 20))
    
    print("Question 4")
    sentence <- "Talking to your mom has the same effect as a hug and helps reduce your"
    s4 <- textPredictor(sentence, nGrams)
    print(head(s4, 20))
    
    print("Question 5")
    sentence <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
    s5 <- textPredictor(sentence, nGrams)
    print(head(s5, 20))
    
    print("Question 6")
    sentence <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
    s6 <- textPredictor(sentence, nGrams)
    print(head(s6, 20))
    
    print("Question 7")
    sentence <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
    s7 <- textPredictor(sentence, nGrams)
    print(head(s7, 20))
    
    print("Question 8")
    sentence <- "Every inch of you is perfect from the bottom to the"
    s8 <- textPredictor(sentence, nGrams)
    print(head(s8, 20))
    
    print("Question 9")
    sentence <- "I'm thankful my childhood was filled with imagination and bruises from playing"
    s9 <- textPredictor(sentence, nGrams)
    print(head(s9, 20))
    
    print("Question 10")
    sentence <- "I like how the same people are in almost all of Adam Sandler's"
    s10 <- textPredictor(sentence, nGrams)
    print(head(s10, 20))
    
    output = list("s1" = s1,
                  "s2" = s2,
                  "s3" = s3,
                  "s4" = s4,
                  "s5" = s5,
                  "s6" = s6,
                  "s7" = s7,
                  "s8" = s8,
                  "s9" = s9,
                  "s10" = s10)
    
    return(output)
    
}
