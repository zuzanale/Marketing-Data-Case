setwd("C:/Users/lezuz/OneDrive/Documents/VU/Marketing Data Case/scripts")
load("CORPUS_GILEAD.rda")
str(corpusFirm) #get structure and data types of the data object
head(unique(corpusFirm$date))

library("sentometrics") # needs to be a data frame to work!!!
#features <- colnames(corpusFirm)[-1:-2] #throw away ids and dates

# plug the full corpus into the sento_corpus() constructor
corpus <- sentometrics::sento_corpus(corpusFirm)
str(corpus)
corpus$documents$texts[1]

# create an additional feature based on a 
# keywords occurrence search (all the texts
# are formatted into lowercase, so make sure
# the keywords are as well)
corpus <- sentometrics::add_features(corpus, 
                                     keywords=list(marketing=c("marketing", "marketing campaign", "product launch")))
sum(corpus$documents$marketing)

corpusFirm$date[20]
corpusFirm$text[20]
