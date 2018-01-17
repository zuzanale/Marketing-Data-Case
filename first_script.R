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
library(gridExtra) #for multiple plots in one figure

features=colnames(corpusFirm)[-1:-2] #throw away ids and dates

# plug the full corpus into the sento_corpus() constructor
corpus=sentometrics::sento_corpus(corpusFirm)
str(corpus)
corpus$documents$texts[1]

###explore features from corpus
#construct document-frequency matrix, similar as DocumentTermMatrix
#from tm package
#stop words
stoplist=stopwords::stopwords('en')
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


###Create my own lexicon
#define events matrix
timeline=data.frame(event=c("Copenhagen.acquisition","Genvoya.approval",
                   " Odefsey.approval",
                   "Descovy.approval","Epclusa.approval",
                   "Vemlidy.approval","Vosevi.approval",
                   "Yescarta.approval","SantaMonica.acquisition","Sovaldi.highprice",
                   "Sovaldi.approval","lawsuit.highprice"),
date= as.Date(c("2015-05-01","2015-11-01","2016-03-01","2016-04-01",
                "2016-06-01",
                "2016-11-01","2017-07-01","2017-10-01",
                "2017-08-01","2014-07-11","2013-12-06","2014-12-06")
                ))

################################################################
#reformat for month-year matching events to texts in corpus
corpus$documents$dateMonth=format(as.Date(corpus$documents$date),"%Y-%m")
timeline$dateMonth=format(as.Date(timeline$date),"%Y-%m")

#see articles for timeline$event[6] (Vemlidy.approval)
event=timeline$dateMonth[6]
#see texts published same month as event occure
corpus$documents$texts[(corpus$documents$dateMonth %in% event)]

#pick texts with correspoding events on timeline
#important words
list(timeline$event[6]=timeline$event[6],terms=c("grant patent","pharmaceutically acceptable"))

#check if those words are in our lexicons
tocheck = c("grant","acceptable")
(tocheck %in% lexicons$LM_eng$x)
(tocheck %in% lexicons$GI_eng$x) # only the second one is there
(tocheck %in% lexicons$HENRY_eng$x)

#compute sentiment scores for this event

#see articles for timeline$event[12] (Vemlidy.approval)
event=timeline$dateMonth[10]
#see texts published same month as event occure
corpus$documents$texts[(corpus$documents$dateMonth %in% event)]

data("lexicons")
data("valence")

#### Create my own lexicon
### find top 30 most frequent words and exclude those that are already
# in prefedined lexicons
#tokens(corpus$documents$texts, remove_numbers = TRUE,  remove_punct = TRUE)
#top50=topfeatures(gilead.dfm, 50)  # 50 top words
#top50=data.frame(word=names(top50),count=top50)

#regular expression matching for corpus specific phrases 
a=str_extract(corpusFirm$text,"instead\\s*([a-zA-Z]+\\s*){1,5}safety")
a=a[!is.na(a)] #remove texts that havent match the pattern
a=unique(unlist(base::strsplit(a,'"[:blank:]"'))) #split into phrases and pick only unique phrase
b=str_extract(corpus,"grant+\\s+patent\\s*([a-zA-Z]+\\s*){1,4}to+\\s+gilead")
b=b[!is.na(b)] #remove texts that havent match the pattern
b=unique(unlist(base::strsplit(b,'"[:blank:]"'))) #split into phrases and pick only unique phrase

#reply
myLexicon=data.frame(w=c(c(a,b),c("approve","release","below average","loss of",
                             "develop new drug", "late stage study"
                        )),
                        s=c(rep(-1.5,length(a)), rep(-1.5,length(b)),2, 1,-1,-2,2,1.5))

#remove "chronic" from lexicons since in pharma/bioscience topics it is 
#not negative word, ???needs to be modified
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
sentMeas=sentometrics::sento_measures(corpus, lexicons=lexIn, ctr=ctr)
plot(sentMeas, group="features")+
  ggthemes::theme_base() +
  scale_x_date(date_breaks ="25 months")

plot(sentMeas, group="lexicons")

#for loop doesnt work for some reason????
#par(mfrow=c(2,2))
for(i in 1:length(sentMeas$features)){
sent=sentometrics::select_measures(sentMeas, toSelect=sentMeas$features[i])
#plot(sent, group="features") 
grid.arrange(plot(sent, group="features"), ncol=2)
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

plot(sent1, group="features")
plot(sent2, group="features")
plot(sent3, group="features")
plot(sent4, group="features")

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
