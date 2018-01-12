###Topic Modelling tutorial
###needs some changes and adjustments to our corpus 
#source: https://rpubs.com/chrisbail/nlp_and_topic_models

library(tm)
library(stopwords)
#corpus reformating
corpus1=corpusFirm #make a clone
corpus1$text=iconv(corpus1$text,"latin1","ASCII",sub="")
blog_corpus=Corpus(VectorSource(as.vector(corpus1$text)))

#pre-processing
blog_corpus=tm_map(blog_corpus, content_transformer(removePunctuation))
blog_corpus=tm_map(blog_corpus,  content_transformer(tolower)) 
blog_corpus=tm_map(blog_corpus , content_transformer(stripWhitespace))

#stop words
stoplist=stopwords('en')
blog_corpus=tm_map(blog_corpus , content_transformer(removeWords), stoplist)

#stemming
blog_corpus=tm_map(blog_corpus , content_transformer(stemDocument), language = "english")

#document term matrix
Blog_DTM <- DocumentTermMatrix(blog_corpus, control = list(wordLengths = c(2, Inf)))
inspect(Blog_DTM[1:20,1:20])

#remove sparse terms
DTM <- removeSparseTerms(Blog_DTM , 0.990)

#inspect popular words
findFreqTerms(Blog_DTM, 3000) #also might need to be added into stopwords

#number of topics
k=10
library(topicmodels)
#######check the parameters
control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = T, 
                         verbose = 0, prefix = tempfile(), 
                         save = 0, 
                         keep = 50, 
                         seed = 980, 
                         nstart = 1, best = T,
                         delta = 0.1,
                         iter = 2000, 
                         burnin = 100, 
                         thin = 2000) 

#######check the parameters
my_first_topic_model <- LDA(Blog_DTM, k, method = "Gibbs", 
                            control = control_LDA_Gibbs)

#most popular terms by topic
terms(my_first_topic_model, 10)

#determine the K
#Hat tip to Achiim Edelman for this nice function.
many_models <- lapply(seq(2, 35, by = 1), 
                        function(x) {LDA(Blog_DTM, x, method = "Gibbs", 
                                         control = control_LDA_Gibbs)} )

#plot the loglikelihoods
many_models.logLik <- as.data.frame(as.matrix(lapply(many_models, logLik)))
plot(2:35, unlist(many_models.logLik ), xlab="Number of Topics", ylab="Log-Likelihood")

#which document is assigment to which topic
topic_assignments_by_docs <- topics(my_first_topic_model)

#save the whole environment for later work
save.image(file='myEnvironment.RData')
