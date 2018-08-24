#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#
# Author Pedro A. Alonso Baigorri
#

library(shiny)
library(markdown)

shinyUI(navbarPage("Welcome to my Impressive Text Predictor!",
           
           tabPanel("Text Predictor",
                    sidebarLayout(
                        
                        
                        sidebarPanel(
                        
                            p("Input your sentence. I will predict the next word for you:"),
                            textAreaInput("sentence", "Text", ""),
                            
                            br(),
                            
                            p("Select the maximum number of results to show:"),
                            sliderInput("max_results", "Number of Results:", min = 1, max = 20, value = 5),
                            
                            br(),
                            submitButton(text = "Predict!", icon = NULL, width = NULL)
                                            
                        ),
                        mainPanel(
    
                            h3("Your prediction is:"),
                            h3(tableOutput("result"))
                            
    
                        )
                    )
           ),
           
           

           
           tabPanel("Documentation", includeMarkdown("about.md"))
           
           
)
)
