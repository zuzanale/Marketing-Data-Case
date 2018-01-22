#set working directories
setwd("C:/Users/lezuz/OneDrive/Documents/Marketing-Data-Case")
# clean repository
rm(list=ls(all=TRUE)) 

#load Gilead corpus
load("CORPUS_GILEAD.rda")
str(corpusFirm) #get structure and data types of the data object
head(unique(corpusFirm$date))

#load needed libraries
library(sentometrics) # needs to be a data frame to work!!!
library(stringi)
library(stringr) # forregex adjustments
library(stopwords) #SnowbalC stopwords
library(quanteda)
library(textstem) #package for lemmatization (overal better than stemming)
library(tidyverse) #usually used for sentometrics analysis 
library(tidytext) 
#library(gridExtra) #for multiple plots in one figure, will be in next release


#load self-defined function for sentiment computation
#(includes regular expression)
source('~/Marketing-Data-Case/My_compute_sentiment.R')

features=colnames(corpusFirm)[-1:-2] #throw away ids and dates

# plug the full corpus into the sento_corpus() constructor
corpus=sentometrics::sento_corpus(corpusFirm)
str(corpus)
corpus$documents$texts[1]
texts(corpus) = stri_trans_tolower(texts(corpus))

###explore features from corpus
#construct document-frequency matrix, similar as DocumentTermMatrix
#from tm package
#stop words
stoplist=stopwords::stopwords('en')
stoplist[(length(stoplist)+1):(length(stoplist)+4)]=c("www","https","http","httpwww")

#removing the possessive ending: ’s and non alphabetical characters
#this is also done in compute_sentiment() part
corpus$documents$texts=str_replace_all(corpus$documents$texts, "(^')|('s$)|[^[:alpha:]+[:blank:]]"," ")

#using lemmatization instead of stemming 
# ... or lemmatization
corpusLem = copy(corpus)
texts(corpusLem) = sapply(texts(corpus), 
                          function(i) return(textstem::lemmatize_strings(i))) 
corpusLem = add_features(corpusLem, keywords=list(increase="increase", detect="detect"))
# transfer features to original corpus (from a stemmed or lemmatized corpus)
newFeats = corpusLem$documents[c("increase", "detect")]
corpus= add_features(corpus, featuresdf=newFeats)

#corpus$documents$texts=textstem::lemmatize_strings(corpus$documents$texts)

#subset the corpus - take only relevant texts
# in this case I am taking only texts after 2006
corpus.event = quanteda::corpus_subset(corpus,format(as.Date(date),"%Y-%m") >  as.Date("2006-01-01"))

#most freqent 50 words
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
corpus=sentometrics::add_features(corpus, 
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

###Create my own lexicon
#define events matrix
timeline=data.frame(event=c("Copenhagen.acquisition","Genvoya.approval",
                   " Odefsey.approval",
                   "Descovy.approval","Epclusa.approval",
                   "Vemlidy.approval","Vosevi.approval",
                   "Yescarta.approval","SantaMonica.acquisition","Sovaldi.highprice",
                   "Sovaldi.approval","lawsuit.highprice","express.deal"),
date= as.Date(c("2015-05-01","2015-11-01","2016-03-01","2016-04-01",
                "2016-06-01",
                "2016-11-01","2017-07-01","2017-10-01",
                "2017-08-01","2014-07-11","2013-12-06","2014-12-06",
                "2016-12-14")
                ))
###########################################
#finding words to detect reputation event
##########################################
eventDates=timeline$date
corpusEvents = corpus_subset(corpus, date %in% eventDates)
tokenized = tokens(corpusEvents, what="word", 
                   remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_separators=TRUE)
tokenized = tokens_remove(tokenized, stopwords("english"))
dfmTokens = dfm(tokenized)
head(dfmTokens)

# words that appear most often in the "event" documents
topFeats = topfeatures(dfmTokens, n=100, scheme="count")

# how many times a word appears at least once in an "event" document
topFeatsDocs = sort(docfreq(dfmTokens, scheme="count"), decreasing=TRUE)


#function for texts selection, for a particular month
check_events=function(event.id,timeline,corpus){
  #the function to find documents publish the same month as specific
  # events define in timeline df happened
  #
  # event.id - index of event (rownumber) from timeline data.frame
  # timeline - data frame with each row corresponding to important event
  #           that happened to a company
  # corpus - corpus of texts after pre-processing stage

  #reformat for month-year matching events to texts in corpus
  corpus$documents$dateMonth=format(as.Date(corpus$documents$date),"%Y-%m")
  timeline$dateMonth=format(as.Date(timeline$date),"%Y-%m")
  
    #see articles for timeline$event[event.id] eg.(Vemlidy.approval)
    event.date=timeline$dateMonth[event.id]
    #see texts published same month as event occure
    document.events=corpus$documents$texts[(corpus$documents$dateMonth %in% event.date)]
    return(list(event.date,document.events))
}

#call the function to find a document
event.date=check_events(6,timeline,corpus)[[1]]
check_events(6,timeline,corpus)
length(check_events(6,timeline,corpus)[[2]]) #number of found articles

#important words
##################################################
####compute sentiment scores for those texts #####
##################################################

lexiconsIn=c(lexicons[c("LM_eng", "GI_eng", "HENRY_eng")])
lexIn=sentometrics::setup_lexicons(lexiconsIn=lexicons[c("LM_eng", "GI_eng", "HENRY_eng")], 
                                   valenceIn=valence[["valence_eng"]], 
                                   do.split=FALSE)

# define how you want the aggregation of textual
# sentiment into time series to take place
ctr=sentometrics::ctr_agg(howWithin="tf-idf",
                          howDocs="equal_weight",
                          howTime=c("equal_weight", "linear","almon"),
                          by="day",
                          #lag=100,
                          do.ignoreZeros=TRUE,
                          fill="latest",ordersAlm=1:3,
                          do.normalizeAlm=TRUE, do.inverseAlm = TRUE)

# compute all sentiment measures for selected articles
event.date=format(as.Date(timeline$date[6]),"%Y-%m")
corpus.event=quanteda::corpus_subset(corpus,format(as.Date(date),"%Y-%m") %in% event.date)
sentMeas.event=sentometrics::sento_measures(corpus.event,
                                      lexicons=lexIn, ctr=ctr)

print(sentMeas.event$measures)
print(to_global(sentMeas)) #print all components across the dimensions
print(sentMeas.event$sentiment) 

#important words
list(timeline$event[6]=timeline$event[6],terms=c("grant patent","pharmaceutically acceptable"))

#check if those words are in our lexicons
tocheck = c("grant","acceptable")
(tocheck %in% lexicons$LM_eng$x)
(tocheck %in% lexicons$GI_eng$x) # only the second one is there
(tocheck %in% lexicons$HENRY_eng$x)

#see articles for timeline$event[10] (high prices)
event=timeline$dateMonth[10]
#see texts published same month as event occure
corpus$documents$texts[(corpus$documents$dateMonth %in% event)]

#see articles for timeline$event[12] (law suit for high prices)
#call the function to find a document
check_events(12,timeline,corpus)

# compute all sentiment measures for selected articles
event.date=format(as.Date(timeline$date[12]),"%Y-%m")
corpus.event = quanteda::corpus_subset(corpus,format(as.Date(date),"%Y-%m") %in% event.date)
sentMeas=sentometrics::sento_measures(corpus.event,
                                      lexicons=lexIn, ctr=ctr)

#summarized measures based on different features and dictionaries
print(sentMeas.event$measures)
print(to_global(sentMeas)) #print all components across the dimensions
print(sentMeas.event$sentiment) 

#check if selected terms are in default dictionaries
list(timeline$event[12]=timeline$event[12],terms=c("rise cost of medicine","share sink",
                                                 "not be able to recover",
                                                 "overweight"))
#check if those words are in our lexicons
tocheck = c("rise","cost","sink","overweight")
(tocheck %in% lexicons$LM_eng$x)
(tocheck %in% lexicons$GI_eng$x) # only the second one is there
(tocheck %in% lexicons$HENRY_eng$x)

#######

#see articles for timeline$event[3] (Odefsey.approval)
#call the function to find a document
check_events(3,timeline,corpus)
check_events(4,timeline,corpus)[[2]][1]#Descovy.approval. first article
check_events(4,timeline,corpus)[[2]][2] #second article
check_events(4,timeline,corpus)[[2]][3] #third article
check_events(4,timeline,corpus)[[2]][4]  #fourth article

# compute all sentiment measures for selected articles
event.date=format(as.Date(timeline$date[4]),"%Y-%m")
corpus.event = quanteda::corpus_subset(corpus,format(as.Date(date),"%Y-%m") %in% event.date)
sentMeas=sentometrics::sento_measures(corpus.event,
                                      lexicons=lexIn, ctr=ctr)

#summarized measures based on different features and dictionaries
print(sentMeas.event$measures)
print(to_global(sentMeas)) #print all components across the dimensions
print(sentMeas.event$sentiment) #LM and HENRY negative, GI positive

######################################################
#check if selected terms are in default dictionaries
list(timeline$event[4]=timeline$event[4],terms=c("raise the risk","rather than ...development"
                                                 ))
#check if those words are in our lexicons
tocheck = c("must","risk","sink")
(tocheck %in% lexicons$LM_eng$x)
(tocheck %in% lexicons$GI_eng$x) # only the second one is there
(tocheck %in% lexicons$HENRY_eng$x)


#check if those words are in our lexicons
tocheck = c("grant","acceptable")
(tocheck %in% lexicons$LM_eng$x)
(tocheck %in% lexicons$GI_eng$x) # only the second one is there
(tocheck %in% lexicons$HENRY_eng$x)

#### Create my own lexicon
### find top 30 most frequent words and exclude those that are already
# in prefedined lexicons
#tokens(corpus$documents$texts, remove_numbers = TRUE,  remove_punct = TRUE)
#top50=topfeatures(gilead.dfm, 50)  # 50 top words
#top50=data.frame(word=names(top50),count=top50)

#regular expression matching for corpus specific phrases 
a=str_extract(corpus$documents$texts,"instead\\s*([a-zA-Z]+\\s*){1,5}safety")
a=a[!is.na(a)] #remove texts that havent match the pattern
a=unique(unlist(base::strsplit(a,'"[:blank:]"'))) #split into phrases and pick only unique phrase
b=str_extract(corpus$documents$texts,"grant+\\s+patent\\s*([a-zA-Z]+\\s*){1,4}to+\\s+gilead")
b=b[!is.na(b)] #remove texts that havent match the pattern
b=unique(unlist(base::strsplit(b,'"[:blank:]"'))) #split into phrases and pick only unique phrase
c=str_extract(corpus$documents$texts,
              "\\bsuccessfully\\b\\s*?\\b(develop|approve)(?:\\W+\\w+){0,2}?\\W+(develop|approve)\\b")
c=c[!is.na(c)] #remove texts that havent match the pattern
c=unique(unlist(base::strsplit(c,'"[:blank:]"'))) #split into phrases and pick only unique phrase
d=str_extract(corpus$documents$texts,
              "\\b(tolerate|accept)(?:\\W+\\w+){0,2}?\\W+(risk)\\b")
d=d[!is.na(d)] #remove texts that havent match the pattern
d=unique(unlist(base::strsplit(d,'"[:blank:]"'))) #split into phrases and pick only unique phrase
e=str_extract(corpus$documents$texts,
              "\\b(raise|rise)(?:\\W+\\w+){0,2}?\\W+(risk)\\b")
e=e[!is.na(e)] #remove texts that havent match the pattern
e=unique(unlist(base::strsplit(e,'"[:blank:]"'))) #split into phrases and pick only unique phrase
f=str_extract(corpus$documents$texts,
              "\\b(rather)\\b\\s*?\\b(than)(?:\\W+\\w+){0,2}?\\W+(development)\\b")
f=f[!is.na(f)] #remove texts that havent match the pattern
f=unique(unlist(base::strsplit(f,'"[:blank:]"'))) #split into phrases and pick only unique phrase

#reply
myLexicon=data.frame(w=c(c(a,b,c,d,e,f),c("approve","release","below average","loss of",
                             "develop new drug", "late stage study","rise cost of",
                             "share sink","not be able to recover", "overweight",
                             "must","high price","low return")
                        ),
                        s=c(rep(-2,length(a)),rep(2,length(b)),
                            rep(3,length(c)),rep(-3,length(d)),
                            rep(-3,length(e)),rep(-2,length(f)),
                            2, 2,-1,-2,2,1,-2,-1,-3,-2,-2,-3,-3))

#remove "chronic" from lexicons since in pharma/bioscience topics it is 
#not negative word
toDelete = c("chromic","disease")
LM_mod= lexicons$LM_eng[!(lexicons$LM_eng$x %in% toDelete)]
GI_mod= lexicons$GI_eng[!(lexicons$GI_eng$x %in% toDelete)]
HENRY_mod= lexicons$HENRY_eng[!(lexicons$HENRY_eng$x %in% toDelete)]

lex_LM = setup_lexicons(list(LM=LM_mod))
lex_GI = setup_lexicons(list(GI=GI_mod))
lex_HENRY = setup_lexicons(list(HENRY=HENRY_mod))
                         
lexiconsIn=c(list(myLexicon=myLexicon),lex_LM,lex_GI ,lex_HENRY )
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
# with self defined function including terms with multiple words
# and regular expressions
sentMeas=my_sento_measures(corpus, lexicons=lexIn,remove=stoplist,ctr=ctr)
#sentMeas=sentometrics::sento_measures(corpus, lexicons=lexIn,ctr=ctr)
plot(sentMeas, group="features")+
  scale_y_continuous(limits = c(-0.03,0.07))+
  scale_x_date(date_breaks ="25 months")

#check results based on different lexicons
plot(sentMeas, group="lexicons")

#plot for each feature individually
#par(mfrow=c(2,2))
for(i in 1:length(sentMeas$features)){
sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[i])
plot(sent,group="features")
}

#clusters determined by inspection of individual graphs
ctrMerge=sentometrics::ctr_merge(sentMeas,features=list(oth_press=c("other", "press"),
                                                        stock_news=c("stocks", "news"),
                                                        prod_results=c("products", "results"),
                                                        drug_pharma=c("drug","pharma")))
#plot the clustered features
ctrMerged=sentometrics::merge_measures(ctrMerge)

plot(ctrMerged, group="features")

# cluster the sentiment measures into two groups
sent1=sentometrics::select_measures(ctrMerged, 
                                    toSelect=c("LM","myLexicon","oth_press"),
                                    do.combine=FALSE
                                    )
#inspect the first cluster
sent1$measures
sent1$sentiment
sent1$stats

# cluster about drugs and health/pharma specific terms
sent2=sentometrics::select_measures(ctrMerged, 
                                    toSelect=c("LM","myLexicon","drug_pharma"),
                                               do.combine=FALSE)

#inspect the second cluster
sent2$measures
sent2$sentiment
sent2$stats

# cluster about stocks and shares
sent3=sentometrics::select_measures(ctrMerged, 
                                    toSelect=c("HENRY","myLexicon","stock_news"),
                                    do.combine=FALSE
)

#inspect the second cluster
sent3$measures
sent3$sentiment
sent3$stats

# cluster about products and results
sent4=sentometrics::select_measures(ctrMerged, 
                                    toSelect=c("LM","myLexicon","prod_results"),
                                    do.combine=FALSE
)

#inspect the second cluster
sent4$measures
sent4$sentiment
sent4$stats

plot(sent1, group="features")
plot(sent2, group="features")
plot(sent3, group="features")
plot(sent4, group="features")

# make two global sentiment indices (one for each 
# cluster); 
#set the weights, we want to keep them equal 
#lexWeights <- c(0.40, 0.20, 0.20, 0.20) # do sentMeas$lexicons to see the ordering of the lexicons
globC1=sentometrics::to_global(sent1)
globC2=sentometrics::to_global(sent2)
globC3=sentometrics::to_global(sent3)
globC4=sentometrics::to_global(sent4)

plot(x=as.Date(row.names(globC1)), main="Global Sentiment Measure",
     y=globC1$global, type="l", xlab="DATE", ylab="SENTIMENT", ylim=c(-0.009, 0.02))
lines(x=as.Date(row.names(globC1)), y=globC2$global, col="red")
lines(x=as.Date(row.names(globC1)), y=globC3$global, col="blue")
lines(x=as.Date(row.names(globC1)), y=globC4$global, col="darkgreen")
legend(as.Date(row.names(globC1))[4600],0.02, # places a legend at the appropriate place 
       c("press & other","stock & news","producs & results","drugs & pharma"),
       lty=c(1,1,1,1),
       lwd=c(2.5,2.5),col=c("black","red","blue","darkgreen")) # puts text in the legend

# make a relative (spread) index,
spread1=globC1 - globC2
spread2=globC3 - globC4
spread3=globC1 - globC3
spread4=globC1 - globC4
spread5=globC2 - globC3
spread6=globC2 - globC4

colnames(spread1)="spread"
plot(x=as.Date(row.names(spread1)),
     main="Press & Other vs. Stocks & News",y=spread1$spread, type="l", xlab="DATE", ylab="SENTIMENT")
abline(h=0, col="blue")

colnames(spread2)="spread"
plot(x=as.Date(row.names(spread2)), 
     main="Products & Results vs. Drug & Pharma", y=spread2$spread, type="l", xlab="DATE", ylab="SENTIMENT")
abline(h=0, col="blue")

colnames(spread3)="spread"
plot(x=as.Date(row.names(spread3)), 
     main="Press & Other vs. Drug & Pharma",y=spread3$spread, type="l", xlab="DATE", ylab="SENTIMENT")
abline(h=0, col="blue")

colnames(spread4)="spread"
plot(x=as.Date(row.names(spread4)),
     main="Press & Other vs. Products & Results",y=spread4$spread, type="l", xlab="DATE", ylab="SENTIMENT")
abline(h=0, col="blue")

colnames(spread6)="spread"
plot(x=as.Date(row.names(spread5)),
     main="Stocks & News vs. Products & Results",y=spread5$spread, type="l", xlab="DATE", ylab="SENTIMENT")
abline(h=0, col="blue")

colnames(spread6)="spread"
plot(x=as.Date(row.names(spread6)),
     main="Stocks & News vs. Drug & Pharma",y=spread6$spread, type="l", xlab="DATE", ylab="SENTIMENT")
abline(h=0, col="blue")

save.image(file='sentiments.RData')
