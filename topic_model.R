###Topic Modelling tutorial
###needs some changes and adjustments to our corpus 
#source: https://rpubs.com/chrisbail/nlp_and_topic_models

#set working directory of the project
setwd("C:/Users/lezuz/OneDrive/Documents/VU/Marketing Data Case/scripts")

###load our corpus into environmet
load("CORPUS_GILEAD.rda")
str(corpusFirm) #get structure and data types of the data object
head(unique(corpusFirm$date))

library(tm)
library(stopwords)
library(topicmodels)
library(dplyr)
library(tidyverse) # R is better when it is tidy
library(stringr)  # for string manipulation

#corpus reformating
corpus1=corpusFirm #make a clone
corpus1$text=iconv(corpus1$text,"latin1","ASCII",sub="")
gilead_corpus=tm::Corpus(VectorSource(as.vector(corpus1$text)))

#pre-processing
#gilead_corpus=tm_map(gilead_corpus, content_transformer(removePunctuation))
gilead_corpus=tm_map(gilead_corpus,  content_transformer(tolower)) 
gilead_corpus=tm_map(gilead_corpus , content_transformer(stripWhitespace))

#stop words
stoplist=stopwords('en')
stoplist[(length(stoplist)+1):(length(stoplist)+3)]=c("www","https","http")
gilead_corpus=tm_map(gilead_corpus , content_transformer(removeWords), stoplist)

#stemming
#gilead_corpus=tm_map(gilead_corpus , content_transformer(stemDocument), language = "english")

#document term matrix, frequency of terms that occur in collection of documents
#rows-documents, columns-terms
#NPL can be performed at this step by "control" argument
#uses Boost tokenizer
DTM=DocumentTermMatrix(gilead_corpus, control = list(#stemming = TRUE,
                                                      minWordLength = 2, removeNumbers = TRUE, 
                                                      removePunctuation = TRUE))
str(DTM)

#load topic model function
source('~/Marketing-Data-Case/topic_model_fc.R')
model=topic_model(DTM,remove="tf-idf",k=10)
save.image(file='Environment.RData')
