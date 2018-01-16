setwd("C:/Users/lezuz/OneDrive/Documents/VU/Marketing Data Case/scripts")
load("CORPUS_GILEAD.rda")
str(corpusFirm) #get structure and data types of the data object
head(unique(corpusFirm$date))

library("sentometrics") # needs to be a data frame to work!!!
library(stringr) # forregex adjustments
library(stopwords) #SnowbalC stopwords
library(quanteda)
library(textstem) #package for lemmatization (overal better than stemming)
library(tidyverse) #usually used for sentometrics analysis 
library(tidytext) 

features <- colnames(corpusFirm)[-1:-2] #throw away ids and dates

# plug the full corpus into the sento_corpus() constructor
corpus <- sentometrics::sento_corpus(corpusFirm)
str(corpus)
corpus$documents$texts[1]

###explore features from corpus
#construct document-frequency matrix, similar as DocumentTermMatrix
#from tm package
#stop words
stoplist=stopwords('en')
stoplist[(length(stoplist)+1):(length(stoplist)+3)]=c("www","https","http")

#removing the possessive ending: ’s and non alphabetical characters
corpus$documents$texts=str_replace_all(corpus$documents$texts, "(^')|('s$)|[^[:alpha:]+[:blank:]]"," ")

#using lemmatization instead of stemming (better results???)
corpus$documents$texts=lemmatize_strings(corpus$documents$texts)
gilead.dfm=dfm(corpus, remove = stoplist, stem = F, remove_punct =T)
#View(gilead.dfm)                        

topfeatures(gilead.dfm, 50)  # 50 top words

#plot document feature matrix
set.seed(100)
textplot_wordcloud(gilead.dfm, min.freq = 10, random.order = FALSE,
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

#### Create my own lexicon
### find top 30 most frequent words and exclude those that are already
# in prefedined lexicons
#tokens(corpus$documents$texts, remove_numbers = TRUE,  remove_punct = TRUE)
#top50=topfeatures(gilead.dfm, 50)  # 50 top words
#top50=data.frame(word=names(top50),count=top50)
lexicon_LM=lexicons[c("LM_eng", "GI_eng", "HENRY_eng")]

#regular expression matching for corpus specific phrases 
a=str_extract(corpusFirm$text,"instead\\s*([a-zA-Z]+\\s*){1,5}safety")
a=a[!is.na(a)] #remove texts that havent match the pattern
a=unlist(base::strsplit(a, '"[:blank:]"')) #split into phrases

#reply
myLexicon=data.frame(w=c(a,c("approve","release","below average","loss of"
                        )),
                        s=c(rep(-1.5,length(a)), 2, 1,-1,-2))

#remove "chronic" from lexicons since in pharma/bioscience topics it is 
#not negative word, ???needs to be modified
lexicon_LM[lexicon_LM$LM_eng$x=="chromic"]=NULL
lexicon_LM[lexicon_LM$GI_eng$x=="chromic"]=NULL
lexicon_LM[lexicon_LM$HENRY_eng$x=="chromic"]=NULL
  
lexicon_LM[lexicon_LM$LM_eng$x=="disease"]=NULL
lexicon_LM[lexicon_LM$GI_eng$x=="disease"]=NULL
lexicon_LM[lexicon_LM$HENRY_eng$x=="disease"]=NULL

lexiconsIn=c(list(myLexicon=myLexicon),lexicons_LM=lexicons[c("LM_eng", "GI_eng", "HENRY_eng")])
lexIn=sentometrics::setup_lexicons(lexiconsIn=lexiconsIn, 
                                      valenceIn=valence[["valence_eng"]], 
                                      do.split=FALSE)

# define how you want the aggregation of textual
# sentiment into time series to take place
ctr=sentometrics::ctr_agg(howWithin="tf-idf",
                             howDocs="proportional",
                             howTime=c("equal_weight", "linear","almon"),
                             by="day",
                             lag=100,
                             do.ignoreZeros=TRUE,
                             fill="latest",ordersAlm=1:3,
                          do.normalizeAlm=TRUE)

# compute all sentiment measures and plot for inspection
sentMeas=sentometrics::sento_measures(corpus, lexicons=lexIn, ctr=ctr)
plot(sentMeas, group="features")+
  ggthemes::theme_base() +
  scale_x_date(date_breaks ="25 months")

plot(sentMeas, group="lexicons")

#for loop doesnt work for some reason????
par(mfrow=c(2,2))
for(i in 1:length(sentMeas$features)){
sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[i])
plot(sent, group="features") 
}

sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[1])
plot(sent, group="features")
sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[2])
plot(sent, group="features")

sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[3])
plot(sent, group="features")

sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[4])
plot(sent, group="features")
sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[5])
plot(sent, group="features")
sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[6])
plot(sent, group="features")
sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[7])
plot(sent, group="features")
sent=sentometrics::select_measures(sentMeas, toSelect="drug")
plot(sent, group="features")

#clusters determined by inspection of individual graphs
ctrMerge=sentometrics::ctr_merge(sentMeas,features=list(oth_press=c("other", "press"),
                                                        stock_news=c("stocks", "news"),
                                                        prod_results=c("products", "results"),
                                                        drug_pharma=c("drug","pharma")))
ctrMerged=sentometrics::merge_measures(ctrMerge)
plot(ctrMerged, group="features")

# cluster the sentiment measures into two groups
sent1=sentometrics::select_measures(ctrMerged, 
                                    toSelect=c("lexicons_LM.LM_eng","myLexicon","oth_press")
                                    )
#inspect the first cluster
sent1$measures
sent1$sentiment
sent1$stats

# cluster about drugs and health/pharma specific terms
sent2=sentometrics::select_measures(ctrMerged, 
                                    toSelect=c("lexicons_LM.LM_eng","myLexicon","drug_pharma")
)

#inspect the second cluster
sent2$measures
sent2$sentiment
sent2$stats

# cluster about stocks and shares
sent3=sentometrics::select_measures(ctrMerged, 
                                    toSelect=c("lexicons_LM.HENRY_eng","myLexicon","stock_news")
)

#inspect the second cluster
sent3$measures
sent3$sentiment
sent3$stats

# cluster about products and results
sent4=sentometrics::select_measures(ctrMerged, 
                                    toSelect=c("lexicons_LM.LM_eng","myLexicon","prod_results")
)

#inspect the second cluster
sent4$measures
sent4$sentiment
sent4$stats

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
