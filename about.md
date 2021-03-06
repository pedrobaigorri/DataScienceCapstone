# Text Predictor App - Data Science Capstone Project

- author: Pedro A. Alonso Baigorri
- date: 23/08/2018


## The Purpose

The objective of this project is to apply data science in the area of natural language processing to create a
prototype of a Text Predictor application similar to what the mobile text editors are using when they suggest
some text to introduce based on previous words.

The final deliverable is the web application, where users will be able to introduce a sentence and it will return a list with the most likely next word.

The design of the application has been done to maximize the following principles:

- **Accuracy:** the prediction should be real and precise.
- **Performance:** calculation of the prediction should be fast with a minimum delay.
- **Usability:** final application should be very easy to use.

## The Corpora Dataset

To create this application we are going to use a dataset ("Corpora") that includes texts collected from twitter,
blogs and news data sources that can be obtained from:
https://web-beta.archive.org/web/20160930083655/http://www.corpora.heliohost.org/aboutcorpus.html

For this app I used the english version of the corpora that includes the following files:

- en_US.twitter.txt: 2.360.148 lines
- en_US.blogs.txt: 899.288 lines
- en_US.news.txt: 77.259 lines

The preparation activites before to build the predictor included some cleaning tasks as:

- transform to lowercase.
- remove punctuations, numbers and symbols.

More information about code and libraries used to these data preparation tasks can be found on: https://rpubs.com/paab/402394

## The Algorithm

The algorithm implemented is a variation of the **Katz's back-off model** based on counting the occurrences
of different potential n-grams, where n can be from 5 to 2.

A weight to the different outputs it's applied to correct the probability of the n-grams, where a high size of the n-grams is assigned ad higher probability.

## The Application

- Users can introduce the sentence that will be the input the predictor.
- It's possible also to select the maximum number of results that provide the predictor.
- The application can be accesible from: https://paab.shinyapps.io/TextPredictor2/
- It can be used also from a **mobile phone** without loss of usability.





