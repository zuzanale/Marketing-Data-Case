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
#corpus reformating
corpus1=corpusFirm #make a clone
corpus1$text=iconv(corpus1$text,"latin1","ASCII",sub="")
gilead_corpus=tm::Corpus(VectorSource(as.vector(corpus1$text)))

#pre-processing
gilead_corpus=tm_map(gilead_corpus, content_transformer(removePunctuation))
gilead_corpus=tm_map(gilead_corpus,  content_transformer(tolower)) 
gilead_corpus=tm_map(gilead_corpus , content_transformer(stripWhitespace))

#stop words
#stop words
stoplist=stopwords('en')
stoplist[(length(stoplist)+1):(length(stoplist)+3)]=c("www","https","http")
gilead_corpus=tm_map(gilead_corpus , content_transformer(removeWords), stoplist)

#stemming
#gilead_corpus=tm_map(gilead_corpus , content_transformer(stemDocument), language = "english")

#document term matrix, frequency of terms that occur in collection of documents
#rows-documents, columns-terms
#NPL can be performed at this step by "control" argument
DTM <- DocumentTermMatrix(gilead_corpus, control = list(minWordLength = 2, removeNumbers = TRUE, 
                                                               removePunctuation = TRUE))
inspect(DTM[1:20,1:20])
str(DTM )
#remove sparse terms
sparse_DTM=removeSparseTerms(DTM , 0.990)

#reduce the number of terms based on tf-idf importancy
term_tfidf=tapply(DTM$v/slam::row_sums(DTM)[DTM$i], DTM$j, mean) *
  log2(tm::nDocs(DTM)/slam::col_sums(DTM > 0))
summary(term_tfidf)

#median is 0.022781
## we remove documents with tfidf smaller than median; >= to the.022781
reduced_DTM=DTM[,term_tfidf >= .022781]
summary(slam::col_sums(reduced_DTM))

#inspect popular words
findFreqTerms(reduced_DTM, 1000)
findFreqTerms(DTM, 3000) #this might be better to use since
#some important terms are dropped by tf-idf approach

#number of topics
k=10

#determine the number of topics by harmonic mean
system.time(DTM_model=topicmodels::LDA(reduced_DTM,k, 
                                       method = "Gibbs", control = list(iter=2000, seed = 0622)))

####explore the estimated model

#most likely topic for each document
DTM_topics=topicmodels::topics(DTM_model, 1)

## top T terms by topic - this should be a variable in function
T=20
DTM_terms=as.data.frame(topicmodels::terms(DTM_model,T),stringsAsFactors =F)
#DTM_terms[1:5] #see first 5 topics

#most popular terms by topic
#terms(DTM_model, 10)

# Creates a dataframe to store the Lesson Number and the most likely topic
doctopics.df=as.data.frame(DTM_topics)
doctopics.df=dplyr::transmute(doctopics.df, docID = rownames(doctopics.df), Topic = DTM_topics)

# add column with rownumber of a document to the original corpus
corpusFirm$docID=as.character(seq.int(nrow(corpusFirm)))

## Adds topic number to original corpus
doctopics.df=dplyr::inner_join(corpusFirm, doctopics.df, by = "docID")

#topic labelling for visualisaiton part
topicTerms <- tidyr::gather(DTM_terms, Topic)
topicTerms <- cbind(topicTerms, Rank = rep(1:T))
topTerms <- dplyr::filter(topicTerms, Rank < 4)
topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
topTerms$Topic <- as.numeric(topTerms$Topic)
topicLabel <- data.frame()
for (i in 1:k){
  z <- dplyr::filter(topTerms, Topic == i)
  l <- as.data.frame(paste(z[1,2], z[2,2], z[3,2], sep = " " ), stringsAsFactors = FALSE)
  topicLabel <- rbind(topicLabel, l)
  
}
colnames(topicLabel)=c("Label")
topicLabel # this label is based on 3 top terms for each topic

##extract theta-posterior probabilities of the topic per doc
theta=as.data.frame(topicmodels::posterior(DTM_model)$topics)
#head(theta[1:5])

##################THIS IS EXTRA CODE #####################
#### using metadata to correlate categories with thetas

#using predefined doc category to correlate with a topic
#we would have to figure out how to do grouping for document by 
#category
x=as.data.frame(row.names(theta), stringsAsFactors = FALSE)
colnames(x)=c("docID")
#x$docID=as.numeric(x$docID)
theta2= cbind(x, theta)
theta2=dplyr::left_join(theta2, Category, by = "docID") #Category
#should be a data set with exlusive category for each document id

## Returns column means grouped by category
theta.mean.by <- by(theta2[, 2:k], theta2$Category, colMeans)
theta.mean <- do.call("rbind", theta.mean.by)

#corelate topics
library(corrplot)
c <- cor(theta.mean)
corrplot(c, method = "circle")

#most diagnostic topic for each categorie
theta.mean.ratios=theta.mean
for (ii in 1:nrow(theta.mean)) {
  for (jj in 1:ncol(theta.mean)) {
    theta.mean.ratios[ii,jj]=
      theta.mean[ii,jj] / sum(theta.mean[ii,-jj])
  }
}
topics.by.ratio=apply(theta.mean.ratios, 1, function(x) sort(x, decreasing = TRUE, index.return = TRUE)$ix)

# The most diagnostic topics per category are found in the theta 1st row of the index matrix:
topics.most.diagnostic=topics.by.ratio[1,]
head(topics.most.diagnostic)

##############################################################
#save the whole environment for later work
save.image(file='Environment.RData')