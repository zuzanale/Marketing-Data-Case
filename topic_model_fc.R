topic_model=function(gilead_DT,remove="sparse", P=0.99,k=10,iter=2000, seed = 0622,
                     T=20)
{               
  # function for fitting topic model using LDA algorithm
  # gilead_DT - corpus converted into document term matrix
  # remove - "no" sparse term removal; "sparse" removal of sparse terms 
  #           with removeSparseTerms() function, with parameter "P", 
  #           default is 0.99; otherwise terms removed by tf-idf importancy alhorithm  
  # P - parameter for removeSparseTerms() function
  # k - number of topics, default 10
  # iter - number of iterations LDA() function for model fitting
  # seeds - parameter for LDA() function for model fitting
  # T - Top T terms for each topic, default Top 20
  # 
  # function returns fitted model
  
  library(tm)
  library(stopwords)
  library(topicmodels)
  library(dplyr)
  
  if(remove=="no"){
    gilead_DTM=gilead_DTM
  }
  else if(remove=="sparse"){
    #remove sparse terms
    gilead_DTM=removeSparseTerms(gilead_DTM ,P)
  }
  else{
    
    #reduce the number of terms based on tf-idf importancy
    term_tfidf=tapply(gilead_DTM$v/slam::row_sums(gilead_DTM)[gilead_DTM$i], gilead_DTM$j, mean) *
      log2(tm::nDocs(gilead_DTM)/slam::col_sums(gilead_DTM > 0))
    
    #median is 0.022781
    ## we remove documents with tfidf smaller than median; >= to the.022781
    gilead_DTM=gilead_DTM[,term_tfidf >= summary(term_tfidf)[3]] 
  }
  
  #model fitting
  DTM_model=topicmodels::LDA(gilead_DTM,k, 
                             method = "Gibbs", control = list(iter=iter, seed = seed))
  
  
  #most likely topic for each document
  DTM_topics=topicmodels::topics(DTM_model, 1)
  
  ## top T terms by topic - this should be a variable in function
  DTM_terms=as.data.frame(topicmodels::terms(DTM_model,T),stringsAsFactors =F)
  #DTM_terms[1:5] #see first 5 topics
  
  # Creates a dataframe to store  the most likely topic
  doctopics.df=as.data.frame(DTM_topics)
  doctopics.df=dplyr::transmute(doctopics.df, docID = rownames(doctopics.df), Topic = DTM_topics)
  
  # add column with rownumber of a document to the original corpus
  corpusFirm$docID=as.character(seq.int(nrow(corpusFirm)))
  
  ## Adds topic number to original corpus
  doctopics.df=dplyr::inner_join(corpusFirm, doctopics.df, by = "docID")
  return(DTM_model)
}
