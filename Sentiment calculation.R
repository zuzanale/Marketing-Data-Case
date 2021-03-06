rm(list=ls(all=TRUE))
library("sentometrics")

# modify this to where you saved the test corpus
setwd("C:/Users/Vedder/Documents/Marketing-Data-Case")

load("CORPUS_GILEAD.rda")
str(corpusFirm) #get structure and data types of the data object
head(unique(corpusFirm$date))

library("sentometrics") # needs to be a data frame to work!!!
library("stringr") # forregex adjustments
library("stopwords") #SnowbalC stopwords
library("quanteda")
library("tidyverse") #usually used for sentometrics analysis 
library("tidytext") 

features <- colnames(corpusFirm)[-1:-2] #throw away ids and dates

# plug the full corpus into the sento_corpus() constructor
corpus <- sentometrics::sento_corpus(corpusFirm)
str(corpus)
corpus$documents$texts[1]

###explore features from corpus
#construct document-frequency matrix, similar as DocumentTermMatrix
#from tm package
#stop words
stoplist=stopwords("en")
stoplist[(length(stoplist)+1):(length(stoplist)+3)]=c("www","https","http")

#removing the possessive ending: ???s and non alphabetical characters
corpus$documents$texts=str_replace_all(corpus$documents$texts, "(^')|('s$)|[^[:alpha:]+[:blank:]]"," ")

gilead.dfm=dfm(corpus, remove = stoplist, stem = TRUE, remove_punct = TRUE)
#View(gilead.dfm)                        

topfeatures(gilead.dfm, 30)  # 30 top words

#plot document feature matrix
set.seed(100)
textplot_wordcloud(gilead.dfm, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))


# create an additional feature based on a 
# keywords occurrence search (all the texts
# are formatted into lowercase, so make sure
# the keywords are as well)
corpus <- sentometrics::add_features(corpus, 
                                     keywords=list(pharma=c("patient", "compound", "bioscience",
                                                            "genotyp","method","trademark","formula",
                                                            "application","requirement"),
                                                   stocks=c("stock","shares","gild","price",
                                                            "trade","rate","return","net",
                                                            "bilion","call","buy"),
                                                   drug=c("hiv","study","viread","truvada",
                                                          "hepatitis","percent","pharmaceuticals",
                                                          "tenofovir","sales","regimen",
                                                          "hbv")))
sum(corpus$documents$pharma)
sum(corpus$documents$stocks)
sum(corpus$documents$drug)

data("lexicons")
data("valence")
myOwnLexicon=data.frame(w=c("success", "growth", "efficacy", "forwardlooking","respect"),
                        s=c(2, 1.5, 2, 1.5,1))

#### Create my own lexicon
### find top 30 most frequent words and exclude those that are already
# in prefedined lexicons
#tokens(corpus$documents$texts, remove_numbers = TRUE,  remove_punct = TRUE)
gilead.dfm=dfm(corpus, remove = stoplist, stem = F, remove_punct = TRUE)
#View(gilead.dfm)                        

top30=topfeatures(gilead.dfm, 30)  # 30 top words
top30=data.frame(word=names(top30),count=top30)
lexicon_LM=lexicons[c("LM_eng", "GI_eng", "HENRY_eng")]
#no joins with current lexicons
top30%>%anti_join(lexicon_LM[[1]]$x,lexicon_LM[[2]]$x,lexicon_LM[[3]]$x) 

lexiconsIn=c(list(myOwnLexicon=myOwnLexicon),lexicons[c("LM_eng", "GI_eng", "HENRY_eng")])
lexIn=sentometrics::setup_lexicons(lexiconsIn=lexiconsIn, 
                                   valenceIn=valence[["valence_eng"]], 
                                   do.split=FALSE)

# define how you want the aggregation of textual
# sentiment into time series to take place
ctr <- sentometrics::ctr_agg(howWithin="tf-idf",
                             howDocs="equal_weight",
                             howTime=c("equal_weight", "linear","almon"),
                             by="day",
                             lag=180,
                             ordersAlm = TRUE,
                             do.ignoreZeros=TRUE,
                             do.inverseAlm = TRUE,
                             do.normalizeAlm = TRUE,
                             fill="latest")

# compute all sentiment measures and plot for inspection
sentMeas <- sentometrics::sento_measures(corpus, lexicons=lexIn, ctr=ctr)
plot(sentMeas, group="features")
plot(scale(sentMeas), group="lexicons")

sentMeasC1 <- sentometrics::select_measures(sentMeas, toSelect="pharma")
plot(sentMeasC1, group="features")

sentMeasC1 <- sentometrics::select_measures(sentMeas, toSelect="press")
plot(sentMeasC1, group="features")

sentMeasC1 <- sentometrics::select_measures(sentMeas, toSelect="products")
plot(sentMeasC1, group="features")

sentMeasC1 <- sentometrics::select_measures(sentMeas, toSelect="results")
plot(sentMeasC1, group="features")

# cluster the sentiment measures into two groups
ctrMerge <- sentometrics::ctr_merge(sentMeas, features=list(clust1=c("pharma", "press","results"),
                                                            clust2=c("stocks", "web"),
                                                            clust3=c("products", "news")))
sentMeasMerged <- sentometrics::merge_measures(ctrMerge)
sentMeasMerged$features # new features
sentMeasC1 <- sentometrics::select_measures(sentMeasMerged, toSelect="clust1")
sentMeasC2 <- sentometrics::select_measures(sentMeasMerged, toSelect="clust2")
sentMeasC3 <- sentometrics::select_measures(sentMeasMerged, toSelect="clust3")

sentMeasC2$measures
sentMeasC2$sentiment
sentMeasC2$stats

sentMeasC1$measures
sentMeasC1$sentiment
sentMeasC1$stats
plot(sentMeasC1, group="features")
plot(sentMeasC2, group="features")
plot(sentMeasC3, group="features")

# make two global sentiment indices (one for each 
# cluster); set the weights according to
# which lexicons and time aggregation schemes
# are more important according to you
lexWeights <- c(0.40, 0.20, 0.20, 0.20) # do sentMeas$lexicons to see the ordering of the lexicons
globC1 <- sentometrics::to_global(sentMeasC1, lexicons=lexWeights)
globC2 <- sentometrics::to_global(sentMeasC2)
globC3 <- sentometrics::to_global(sentMeasC3)
plot(x=as.Date(row.names(globC1)), y=globC1$global, type="l", xlab="DATE", ylab="SENTIMENT", ylim=c(-0.01, 0.03))
lines(x=as.Date(row.names(globC1)), y=globC2$global, col="red")
lines(x=as.Date(row.names(globC1)), y=globC3$global, col="blue")

# make a relative (spread) index,
#spread not really relevant in case of three clusters
spread <- globC1 - globC2
colnames(spread) <- "spread"
plot(x=as.Date(row.names(spread)), y=spread$spread, type="l", xlab="DATE", ylab="SENTIMENT")
abline(h=0, col="blue")