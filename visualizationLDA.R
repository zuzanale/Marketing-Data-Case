###############################################################
##### ###visualization of LDA model ##########################
##############################################################

#load results from LDA topic modelling
load(Environment.RData)

#instal for loading JSON file in explorer, if it doesnt work
#install.packages('servr') 

topicmodels_json_ldavis=function(fitted, corpus, doc_term){
  ## Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  
  ## Find required quantities
  phi=posterior(fitted)$terms %>% as.matrix
  theta=posterior(fitted)$topics %>% as.matrix
  vocab=colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus$text)) {
    temp=paste(corpus[i]$text, collapse = ' ')
    #vector of sums of words for each text in corpus
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency=slam::col_sums(doc_term) 
  freq_matrix=data.frame(ST = names(temp_frequency),
                            Freq = temp_frequency)
  rm(temp_frequency)
  
  ## Convert to json
  json_lda=LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

#supply fitted model, corpus and document term matrix
gilead.json=topicmodels_json_ldavis(model$DTM_model, corpusFirm, model$gilead_DTM)
serVis(gilead.json)
