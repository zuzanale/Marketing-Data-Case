
####################################################################################################### helper functions

plotBinary <- function(pred, yb) { # plotting function for binary predictions
  dates <- names(yb)
  data <- data.frame(date = dates, true = yb, predicted = pred)
  colnames(data)[2:3] <- c("true", "predicted")
  data[, 2:3] <- lapply(data[, 2:3], as.character)
  melted <- melt(data, id.vars = "date")
  p <- ggplot(data = melted, aes(x = as.Date(date), y = value, color = variable)) +
    geom_point() +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_discrete(name = "Response") +
    ggthemes::theme_tufte(base_size = 12) +
    theme(legend.title = element_blank(), legend.position = "top")
  return(p)
}

attribution_cv_glmnet <- function(out, sentomeasures, dimNames, type) { # attribution calculation for a cv.glmnet() output
  measures <- sentomeasures$measures[, -1]
  refDates <- sentomeasures$measures$date
  loc <- 1:dim(measures)[1]
  cols <- colnames(measures)
  coeffs <- coef(out, s = "lambda.min")
  attribsDim <- lapply(dimNames, function(x) {
    sel <- cols[stringi::stri_detect(cols, regex = paste0("\\b", x, "\\b"))]
    cf <- coeffs[which(rownames(coeffs) %in% sel)]
    coeffsIn <- data.table(matrix(cf, nrow = length(loc), ncol = length(cf), byrow = TRUE))
    attribs <- rowSums(coeffsIn * measures[loc, sel, with = FALSE, drop = FALSE], na.rm = TRUE)
    attr <- data.table(date = refDates, attrib = attribs)
    return(attr)
  })
  names(attribsDim) <- dimNames
  attribsDim <- dcast(rbindlist(attribsDim, idcol = "name"), date ~ name, value.var = "attrib")
  attribsDim <- attribsDim[, c("date", sentomeasures[[type]]), with = FALSE]
  return(attribsDim)
}

plot_attributions_cv_glmnet <- function(attributions) { # attribution plotting for a attribution_cv_glmnet() output
  attributionsMelt <- melt(attributions, id.vars = "date", variable.factor = FALSE)
  attributionsMelt <- attributionsMelt[order(rank(variable))]
  legendPos <- ifelse(length(unique(attributionsMelt[["variable"]])) <= 12, "top", "none")
  p <- ggplot(data = attributionsMelt, aes(x = date, y = value, group = variable, color = variable)) +
    geom_area(aes(colour = variable, fill = variable), alpha = 1) +
    geom_hline(yintercept = 0, size = 0.50, linetype = "dotted") +
    scale_fill_grey(start = 0, end = 1) +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = "Attribution") +
    ggthemes::theme_tufte(base_size = 12) +
    theme(legend.title = element_blank(), legend.position = legendPos)
  return(p)
}

####################################################################################################### corpus & sentiment

library("sentometrics")
data("lexicons")
data("valence")

# setwd("C:/Users/lezuz/OneDrive/Documents/Marketing-Data-Case")

load("CORPUS_GILEAD.rda")

#load sentimets computation in sentiment_analysis.R
load('sentiments.RData')
#corpus <- sentometrics::sento_corpus(corpusFirm)
corpus=add_features(corpus, keywords = list(reputation = c("reputation")))
corpus=quanteda::corpus_subset(corpus,date>"2006-01-01")
#lexiconsIn <- lexicons[c("LM_eng", "GI_eng", "HENRY_eng")]
#lexIn <- sentometrics::setup_lexicons(lexiconsIn=lexiconsIn, valenceIn=valence[["valence_eng"]])

#these are our old control settings
#ctr=sentometrics::ctr_agg(howWithin="tf-idf",
#                          howDocs="proportional",
#                          howTime=c("equal_weight", "linear","almon"),
#                          by="day",
#                          lag=100,
#                          do.ignoreZeros=TRUE,
#                          fill="latest",ordersAlm=1:3,
#                          do.normalizeAlm=TRUE)

#setting from example code
ctr <- sentometrics::ctr_agg(howWithin="tf-idf",
        howDocs="equal_weight",
        howTime=c("equal_weight", "linear", "exponential"),
        by="month",
        lag=12,
        alphasExp=c(0.2, 0.5),
        do.ignoreZeros=FALSE,
        fill="zero")
ctr <- sentometrics::ctr_agg(howWithin="tf-idf",
                             howDocs="equal_weight",
                             howTime=c("equal_weight", "linear", "almon"),
                             by="month",
                             lag=12,
                             do.ignoreZeros=FALSE,ordersAlm=1:3,do.normalizeAlm=T,
                             fill="zero")

sentMeas=my_sento_measures(corpus, lexicons=lexIn,remove=stoplist,ctr=ctr)
plot(sentMeas, group="features")

#########################################################################
# Target variable definition
########################################################################

library("quantmod")

#choose if to use stock returns or timeline
target_var=function(sentMeas, measure=c("returns","events"), timeline=NULL
                    ) {
  # sentMeas - output from sento_measures function with computed sentimets
  # measure - select if retunrs or events should be used for target variable definition
  # timeline - timeline for specific events, only if "events"
  #           is measure s used for target var definition
  
  if(measure=="returns"){
    #to get highest changes in returns as target variables
    getSymbols("GILD", src="google") # Chipotle Mexican Grill's stock prices
    # modify dates to yyyy-mm-01 format in the sentiment measures
    monthsAll <- unlist(lapply(stringi::stri_split(index(GILD), regex = "-"), 
                               function(d) return(paste0(d[1:2], collapse = "-"))))
    index(GILD) <- as.Date(paste0(monthsAll, "-01"), format = "%Y-%m-%d")
    volume <- as.data.table(GILD)[, list(volM = sum(GILD.Volume)), by = list(index)]
    target <- volume
  
    # align sentiment and target variable
    sentMeasIn <- select_measures(sentMeas, dates=index(GILD))# here we should define which sentiments should be used
  
    dates <- sentMeasIn$measures$date
    target <- target[index %in% as.Date(dates)] # to align target and sentiment variables
    dim(target)[1] == dim(sentMeasIn$measures)[1] # TRUE
    target$index <- NULL
  
  # pr <- GILD[, 4]
  # ret <- allReturns(pr)
  # ret <- ret[index(ret) <= "2017-01-01"]
  # target <- na.omit(ret[, "monthly"])
  # months <- unlist(lapply(stringi::stri_split(index(target), regex = "-"), function(d) return(paste0(d[1:2], collapse = "-"))))
  # index(target) <- as.Date(paste0(months, "-01"), format = "%Y-%m-%d")
  # sentMeasIn <- select_measures(sentMeas, dates=index(target))
  # dates <- sentMeasIn$measures$date
  # target <- target[index(target) %in% as.Date(dates)]
  # dim(target)[1] == dim(sentMeasIn$measures)[1] # TRUE
  
    nEvents <- 20
    indx <- order(abs(target), decreasing=TRUE)[1:nEvents]
    top <- target[indx]
    topDates <- dates[indx]
    y <- rep(0, length(dates)) # no "event"
    names(y) <- dates
    y[as.Date(names(y)) %in% topDates] <- 1 # "event"
    y.returns <- as.factor(y) # target var as abnormal change in returns
    yb=y.returns # choose which of those two measure to use,
    return=list(yb,sentMeasIn)
  }else{
    if(is.null(timeline)){
      stop("Provide a timeline table with important events")
    }else{
    #get dates from our timeline as importnant reputation events
    sentMeasIn <- select_measures(sentMeas, dates=timeline$date)# here we should define which sentiments should be used
    events <- timeline$event
    events.date <-timeline$date
    y <- rep(0, length(sentMeasIn$measures$date)) # no "event"
    names(y) <-sentMeasIn$measures$date
    y[as.Date(names(y)) %in% events.date] <- 1 # "event"
    y.timeline <- as.factor(y) # target var as self-defined returns

    # choose which of those two measure to use,
    #later i can do self-defined function for this
    yb=y.timeline
    return(list(yb,sentMeasIn,events.date))
  }}
}

target=target_var(sentMeas,"returns",timeline)
yb=target[[1]]
sentMeansIn=target[[2]]

#########################################################################
# Reputation modelling
########################################################################
library("doParallel")
cl <- makeCluster(2)
registerDoParallel(cl)
#cv stands for cross-validation with caret package
ctrModel <- ctr_model(model="binomial", type="cv", h=0, trainWindow=50, 
                      testWindow=10, do.parallel=TRUE) # LASSO
out <- sento_model(sentMeasIn, y=yb, ctr=ctrModel)
stopCluster(cl)
summary(out)
pred <- predict(out$reg, newx=as.matrix(sentMeasIn$measures[, -1]), type="class")
plotBinary(pred, yb)
plotBinary(pred[yb == 1], yb[yb == 1])
TP <- sum(pred[pred == 1] == yb[pred == 1]) # true positives
TN <- sum(pred[pred == 0] == yb[pred == 0]) # true negatives
FP <- sum(pred[pred == 1] != yb[pred == 1]) # false positives
FN <- sum(pred[pred == 0] != yb[pred == 0]) # false negatives
TPR <- TP / (TP + FN)
TNR <- TN / (TN + FP)
accT <- (TP + TN) / (TP + FP + TN + FN) # total accuracy

attr <- retrieve_attributions(out, sentMeasIn)
plot_attributions(attr) # this is only about the sentiment measures, so no constant or other variables!

library("glmnet")
out2 <- cv.glmnet(x=as.matrix(sentMeasIn$measures[, -1]), y=yb, family="binomial", alpha=1, type.measure="class", nfolds=5)
plot(out2)
pred2 <- predict(out2, newx=as.matrix(sentMeasIn$measures[, -1]), type="class", s="lambda.min")
plotBinary(pred2, yb)
coef <- coef(out2, s="lambda.min")

attr2 <- attribution_cv_glmnet(out2, sentMeasIn, sentMeasIn$lexicons, "lexicons")
pred2l <- predict(out2, newx=as.matrix(sentMeasIn$measures[, -1]), type="link", s="lambda.min") # linear equation
(pred2l - coef(out2, s="lambda.min")[1, ]) - rowSums(attr2[, -1]) # very close to zero
plot_attributions_cv_glmnet(attr2)

##############################################
#merged sentimets
##############################################
#example code
#ctrMerge <- ctr_merge(sentMeasIn, 
#                      features = list(FEAT = sentMeasIn$features),
#                      time = list(TIME = sentMeasIn$time))

ctrMerge=sentometrics::ctr_merge(sentMeasIn,features=list(oth_press=c("other", "press"),
                                                        stock_news=c("stocks", "news"),
                                                        prod_results=c("products", "results"),
                                                        drug_pharma=c("drug","pharma")),
                                 time=list(TIME = sentMeasIn$time))


sentMerged=merge_measures(ctrMerge)
plot(sentMerged)

#global indexes from sentiment_analysis.R

####this part is the same code as from sentiment_analysis.R
# cluster the sentiment measures into two groups
sent1=sentometrics::select_measures(sentMerged, 
                                    toSelect=c("LM","myLexicon","oth_press"),
                                    do.combine=FALSE
)
sent2=sentometrics::select_measures(sentMerged, 
                                    toSelect=c("LM","myLexicon","drug_pharma"),
                                    do.combine=FALSE)
sent3=sentometrics::select_measures(sentMerged, 
                                    toSelect=c("HENRY","myLexicon","stock_news"),
                                    do.combine=FALSE
)
sent4=sentometrics::select_measures(sentMerged, 
                                    toSelect=c("LM","myLexicon","prod_results"),
                                    do.combine=FALSE
)

globC1=sentometrics::to_global(sent1)
globC2=sentometrics::to_global(sent2)
globC3=sentometrics::to_global(sent3)
globC4=sentometrics::to_global(sent4)
####

#this is relevant only for the timetable with events option
#globC1=globC1[as.Date(rownames(globC1)) %in% events.date,]
#globC2=globC2[as.Date(rownames(globC2)) %in% events.date,]
#globC3=globC3[as.Date(rownames(globC3)) %in% events.date,]
#globC4=globC4[as.Date(rownames(globC4)) %in% events.date,]

glob=to_global(sentMerged)

#this is data frame for stocks/returns approach
data=data.frame(yb=yb, glob1=globC1$global,glob2=globC2$global,
                glob3=globC3$global,glob4=globC4$global,
                my.lex=sentMerged$measures$`myLexicon--stock_news--TIME`,
                he=sentMerged$measures$`HENRY--stock_news--TIME`, 
                lm=sentMerged$measures$`LM--stock_news--TIME`,
                gi=sentMerged$measures$'GI--stock_news--TIME'
                )

#this is data frame for timeline 
data2=data.frame(yb=yb, glob1=globC1$global,glob2=globC2$global,
                glob3=globC3$global,glob4=globC4$global,
                my.lex1=sentMerged$measures$`myLexicon--drug_pharma--TIME`,
                my.lex2=sentMerged$measures$'myLexicon--oth_press--TIME',
                he=sentMerged$measures$`HENRY--drug_pharma--TIME`, 
                he2=sentMerged$measures$`HENRY--oth_press--TIME`,
                lm=sentMerged$measures$`LM--drug_pharma--TIME`,
                lm2=sentMerged$measures$`LM--oth_press--TIME`,
                gi=sentMerged$measures$'GI--drug_pharma--TIME',
                gi2=sentMerged$measures$'GI--oth_press--TIME'
)

########## LOGISTIC REGRESSSION #####################################################
###returns
#full regression
fullmod=glm(yb ~ ., family=binomial(link="logit"), data=data) # all the variables
summary(fullmod)

#nothing
nothing=glm(yb ~ 1, family=binomial(link="logit"),data=data)
summary(nothing)

###events
fullmod=glm(yb ~ ., family=binomial(link="logit"), data=data2) # all the variables
summary(fullmod)

#nothing
nothing=glm(yb ~ 1, family=binomial(link="logit"),data=data2)
summary(nothing)

###stepwise regression
bothways1 =step(fullmod, scope=list(lower=formula(nothing),upper=formula(fullmod)),
         direction="both")
summary(bothways1) #final model
#glm(formula = yb ~ glob1 + glob4 + my.lex1 + lm, family = binomial(link = "logit"), 
#data = data2)

#with interaction terms included
bothways2 =step(fullmod, scope=list(lower=formula(nothing),upper= as.formula(yb ~ .^2)),
               direction="both")
summary(bothways2) #final model

final_model=glm(formula = yb ~ glob1 + glob2 + glob4 + my.lex + lm + gi + 
                  my.lex:lm + glob4:lm + glob1:lm + glob2:lm + glob4:my.lex + 
                  glob2:glob4, family = binomial(link = "logit"), data = data)

plot(final_model$fitted.values, ylim=c(0, 1))
lines(as.numeric(as.character(yb)), type="p", col="red") # does it change when reputation ("stock market") event?
# par(xpd=FALSE)
abline(h=0.5)
  
final_model_v1=glm(formula = yb ~ glob1 + glob3 + glob4 + he + gi, family = binomial(link = "logit"), 
                  data = data)
summary(final_model_v1)
plot(final_model_v1$fitted.values, ylim=c(0, 1))
lines(as.numeric(as.character(yb)), type="p", col="red") # does it change when reputation ("stock market") event?
abline(h=0.5)

###regression diagnostic and evaluation
#likelihood ratio test
anova(final_model_v1, final_model, test ="Chisq")

#pseudo R^2
library(pscl)
pR2(final_model)[4]  # look for 'McFadden'
pR2(final_model_v1)[4] # look for 'McFadden'
