
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
library(dplyr)
data("lexicons")
data("valence")

# setwd("C:/Users/lezuz/OneDrive/Documents/Marketing-Data-Case")

#load("CORPUS_GILEAD.rda")

#load sentimets computation in sentiment_analysis.R
load('sentiments2.RData')

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
    sentMerged<- select_measures(ctrMerged, dates=index(GILD))
    sent1 <- select_measures(sent1,dates=index(GILD))
    sent2 <- select_measures(sent2, dates=index(GILD))
    sent3 <- select_measures(sent3, dates=index(GILD))
    sent4 <- select_measures(sent4, dates=index(GILD))
    
    dates <- sentMeasIn$measures$date
    target <- target[index %in% as.Date(dates)] # to align target and sentiment variables
    dim(target)[1] == dim(sentMeasIn$measures)[1] # TRUE
    target$index <- NULL
  
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
    sentMeasIn <- sentMeas# here we should define which sentiments should be used
    sentMerged<- ctrMerged
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

#important merge as the split point
dev_sample = select_measures(sentMeasIn, dates = "date <='2011-11-21'")
val_sample = select_measures(sentMeasIn, dates = "date > '2011-11-21'")

#pre-crisis / post-crisis approach
dev_sample = select_measures(sentMeasIn, dates = "date <='2011-07-01'")
val_sample = select_measures(sentMeasIn, dates = "date > '2011-07-01'")


globC1=sentometrics::to_global(sent1)
globC2=sentometrics::to_global(sent2)
globC3=sentometrics::to_global(sent3)
globC4=sentometrics::to_global(sent4)

glob=to_global(sentMerged)

######################################################
#this is data frame for stocks/returns approach#######
######################################################

#try with only data that are connected with stock/returns feature (cluster 3)
#dataclust3=sent3$measures[,-1]
dataclust3=sent3$measures %>% dplyr::select(starts_with("HENRY"))
#dataclust3=sent3$measures[,grepl("HENRY" , names(sent3$measures))]
dataclust3=cbind(data.frame(yb=yb, glob1=globC1$global,glob2=globC2$global,
                      glob3=globC3$global,glob4=globC4$global,dataclust3
))

#split into development and validation sample
dataclust3.dev=dataclust3[sent3$measures$date<='2011-07-01',]
dataclust3.val=dataclust3[sent3$measures$date>'2011-07-01',]

#
########## LOGISTIC REGRESSSION #####################################################
###returns

fullmod=glm(yb ~ ., family=binomial(link="logit"), data=dataclust3) # all the variables
summary(fullmod)

#nothing
nothing=glm(yb ~ 1, family=binomial(link="logit"),data=dataclust3)
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

final_model=glm(formula = yb ~ glob2 + glob3 + glob4 + HENRY..web..equal_weight + 
                  HENRY..drug_pharma..equal_weight +HENRY..drug_pharma..linear+
                  HENRY..web..linear+HENRY..detect..linear+HENRY..stock_news..equal_weight+
                  HENRY..oth_press..equal_weight +HENRY..oth_press..linear +glob1+
                  HENRY..prod_results..equal_weight+HENRY..increase..equal_weight+
                  HENRY..increase..linear +HENRY..stock_news..linear  +HENRY..prod_results..linear+
                  HENRY..detect..equal_weight
                  , family = binomial(link = "logit"), 
                data = dataclust3)


plot(final_model$fitted.values, ylim=c(0, 1),main="Reputation based on abnormal returns",
     ylab="Target variable")
lines(as.numeric(as.character(yb)), type="p", col="red") # does it change when reputation ("stock market") event?
abline(h=0.5)
legend(90,0.9, # places a legend at the appropriate place 
       c("fitted values","real values"),
       lty=c(0,0),
       lwd=c(1,1),col=c("black","red"),
       pch=c(1,1)) # puts text in the legend

# this is the best model for abnormal returns modelling with a use of cluster with financial features  
final_model_v1=glm(formula = yb ~ glob1 + glob3 + HENRY..web..equal_weight + 
                       HENRY..detect..equal_weight + 
                     HENRY..web..linear + HENRY..increase..linear + HENRY..detect..linear + 
                     HENRY..oth_press..equal_weight + HENRY..oth_press..linear + 
                     HENRY..stock_news..equal_weight + HENRY..stock_news..linear + 
                     HENRY..prod_results..equal_weight + HENRY..prod_results..linear + 
                     HENRY..drug_pharma..equal_weight  + 
                     HENRY..stock_news..linear:HENRY..prod_results..equal_weight + 
                     HENRY..web..equal_weight:HENRY..detect..equal_weight + HENRY..web..equal_weight:HENRY..prod_results..linear + 
                     HENRY..oth_press..equal_weight:HENRY..prod_results..equal_weight + 
                     HENRY..web..equal_weight:HENRY..detect..linear + glob1:HENRY..detect..linear, 
                   family = binomial(link = "logit"), data = dataclust3)

summary(final_model_v1)
plot(final_model_v1$fitted.values, ylim=c(0, 1),main="Reputation based on abnormal returns",
     ylab="Target variable")
lines(as.numeric(as.character(yb)), type="p", col="red") # does it change when reputation ("stock market") event?
abline(h=0.5)
legend(90,0.9, # places a legend at the appropriate place 
       c("fitted values","real values"),
       lty=c(0,0),
       lwd=c(1,1),col=c("black","red"),
       pch=c(1,1)) # puts text in the legend

###regression diagnostic and evaluation
anova(final_model, test="Chisq")
anova(final_model_v1, test="Chisq")

#pseudo R^2
library(pscl)
pR2(final_model)[4]  # look for 'McFadden'
pR2(final_model_v1)[4] # look for 'McFadden'

#Wald test
library(survey)
regTermTest(final_model, "glob2:glob4")#fails to reject, var should be removed
regTermTest(final_model, "glob4:my.lex")

#variable importance
library(caret)
varImp(final_model)
varImp(final_model_v1)

#prediction plus prediction evaluation
pred.link=predict(final_model,dataclust3.val, type="link")
pred.response=predict(final_model,dataclust3.val,type='response')

pred.link2=predict(final_model_v1,dataclust3.val, type="link")
pred.response2=predict(final_model_v1,dataclust3.val,type='response')

score_data=data.frame(link=pred.link, 
                         response=pred.response,
                         y=dataclust3.val$yb,
                         stringsAsFactors=FALSE)

score_data %>% 
  ggplot(aes(x=link, y=response, col=y)) + 
  scale_color_manual(values=c("black", "red")) + 
  geom_point() + 
  geom_rug() + 
  ggtitle("Both link and response scores put cases in the same order")

score_data2=data.frame(link=pred.link2, 
                      response=pred.response2,
                      y=dataclust3.val$yb,
                      stringsAsFactors=FALSE)

score_data2 %>% 
  ggplot(aes(x=link, y=response, col=y)) + 
  scale_color_manual(values=c("black", "red")) + 
  geom_point() + 
  geom_rug() + 
  ggtitle("Both link and response scores put cases in the same order")

#Accuracy computation
pred.response=ifelse(pred.response > 0.5,1,0)
misClasificError=mean(pred.response != dataclust3.val$yb)
print(paste('Accuracy',1-misClasificError)) #print prediction accuracy

pred.response2=ifelse(pred.response2 > 0.5,1,0)
misClasificError=mean(pred.response2 != dataclust3.val$yb)
print(paste('Accuracy',1-misClasificError)) #print prediction accuracy

#plotting ROC curve, plotting TPR against FPR
library(pROC)
plot(roc(dataclust3.val$yb,pred.response2, direction="<"),
     col="blue", lwd=3, main="ROC curve for extraordinary
     return change prediction")

#AUC computation
library(ROCR)
pr=prediction(pred.response2,dataclust3.val$yb)
auc=performance(pr, measure = "auc")
auc=auc@y.values[[1]]
auc #auc on validation sample for stock-returns cluster is 0.7833333

############################################################
###### Evaluations for validation sample #################
########################################################

#plotBinary(pred.response[yb == 1], yb[yb == 1])
TP=sum(pred.response[pred.response == 1] ==  dataclust3.val$yb[pred.response == 1]) # true positives
TN=sum(pred.response[pred.response == 0] ==  dataclust3.val$yb[pred.response == 0]) # true negatives
FP =sum(pred.response[pred.response == 1] !=  dataclust3.val$yb[pred.response == 1]) # false positives
FN =sum(pred.response[pred.response == 0] !=  dataclust3.val$yb[pred.response == 0]) # false negatives
TPR = TP / (TP + FN)
TNR =TN / (TN + FP)
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

#same evaluation measures for the second model
TP=sum(pred.response2[pred.response2 == 1] == dataclust3.val$yb[pred.response2 == 1]) # true positives
TN=sum(pred.response2[pred.response2 == 0] ==  dataclust3.val$yb[pred.response2 == 0]) # true negatives
FP =sum(pred.response2[pred.response2 == 1] !=dataclust3.val$yb[pred.response2 == 1]) # false positives
FN =sum(pred.response2[pred.response2 == 0] != dataclust3.val$yb[pred.response2 == 0]) # false negatives
TPR = TP / (TP + FN)
TNR =TN / (TN + FP)
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy


############################################################
###### Evaluations for full sample #################
########################################################
pred.response=predict(final_model,dataclust3,type='response')
pred.response=ifelse(pred.response > 0.5,1,0)
pred.response2=predict(final_model_v1,dataclust3,type='response')
pred.response2=ifelse(pred.response2 > 0.5,1,0)

TP=sum(pred.response[pred.response == 1] ==  dataclust3$yb[pred.response == 1]) # true positives
TN=sum(pred.response[pred.response == 0] ==  dataclust3$yb[pred.response == 0]) # true negatives
FP =sum(pred.response[pred.response == 1] !=  dataclust3$yb[pred.response == 1]) # false positives
FN =sum(pred.response[pred.response == 0] !=  dataclust3$yb[pred.response == 0]) # false negatives
TPR = TP / (TP + FN)
TNR =TN / (TN + FP)
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

#same evaluation measures for the second model
TP=sum(pred.response2[pred.response2 == 1] == dataclust3$yb[pred.response2 == 1]) # true positives
TN=sum(pred.response2[pred.response2 == 0] ==  dataclust3$yb[pred.response2 == 0]) # true negatives
FP =sum(pred.response2[pred.response2 == 1] !=dataclust3$yb[pred.response2 == 1]) # false positives
FN =sum(pred.response2[pred.response2 == 0] != dataclust3$yb[pred.response2 == 0]) # false negatives
TPR = TP / (TP + FN)
TNR =TN / (TN + FP)
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

################ SPARSE REGRESSION #########################################################
library("glmnet")
sparse2=cv.glmnet(x=as.matrix(sent3$measures[, -1]), y=yb, family="binomial", alpha=1, type.measure="class", nfolds=10)
plot(sparse2)
pred2=predict(sparse2, newx=as.matrix(sent3$measures[, -1]), type="class", s="lambda.min")
plotBinary(pred2, yb)
coef=coef(sparse2, s="lambda.min")

#prediction evaluation
attr2=attribution_cv_glmnet(sparse2, sent3, sent3$features, "features")
pred2l=predict(sparse2, newx=as.matrix(sent3$measures[, -1]), type="link", s="lambda.min") # linear equation
(pred2l - coef(sparse2, s="lambda.min")[1, ]) - rowSums(attr2[, -1]) # very close to zero
plot_attributions_cv_glmnet(attr2)

plotBinary(pred2[yb == 1], yb[yb == 1])
TP=sum(pred2[pred2 == 1] == yb[pred2 == 1]) # true positives
TN=sum(pred2[pred2 == 0] == yb[pred2 == 0]) # true negatives
FP =sum(pred2[pred2 == 1] != yb[pred2 == 1]) # false positives
FN =sum(pred2[pred2 == 0] != yb[pred2 == 0]) # false negatives
TPR = TP / (TP + FN) #recall
PR=TP/(TP+TN) #precission
TNR =TN / (TN + FP) #true negative rate
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

#####################################################################################
####################### Random forest ########################################
library(randomForest)
#returns as target variable

set=data.dev
set.val=data.val
set=dataclust3.dev # sentiments only with regard to financial data
set.val=dataclust3.val
set=dataclust.dev
set.val=dataclust.dev

#randomForest
rf = randomForest(yb ~ .,  
                  ntree = 100,
                  data = set)
plot(rf) 
print(rf)

# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")
var.imp = data.frame(importance(rf,  
                                type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
prediction = predict(rf , set)
# Create Confusion Matrix
print(  
  confusionMatrix(data=prediction,  
                  reference=set$yb))

# Create Confusion Matrix
# Predicting response variable
prediction.val=predict(rf ,set.val)

# Create Confusion Matrix
print(  
  confusionMatrix(data=prediction.val,  
                  reference=set.val$yb))

TP=sum(prediction.val[prediction.val == 1] == set.val[prediction.val == 1,1]) # true positives
TN=sum(prediction.val[prediction.val == 0] == set.val[prediction.val == 0,1]) # true negatives
FP =sum(prediction.val[prediction.val == 1] != set.val[prediction.val == 1,1]) # false positives
FN =sum(prediction.val[prediction.val == 0] != set.val[prediction.val == 0,1]) # false negatives
TPR = TP / (TP + FN) #recall
PR=TP/(TP+TN) #precission
TNR =TN / (TN + FP) #true negative rate
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

save.image(file='model1.RData')

####################################################################
######################################################################################
# timeline defined events as target variable 
#########################################################

target=target_var(sentMeas,"returns",timeline)
yb=target[[1]]
sentMeansIn=target[[2]]


#this is data frame for stocks/returns approach
#take all the measurements
data=sentMerged$measures[,-1]
data=cbind(data.frame(yb=yb, glob1=globC1$global,glob2=globC2$global,
                      glob3=globC3$global,glob4=globC4$global,data
))
#split into development and validation sample
data.dev=data[sentMerged$measures$date<='2011-07-01',]
data.val=data[sentMerged$measures$date>'2011-07-01',]

table(data$yb)/nrow(data) #full sample
table(data.dev$yb)/nrow(data.dev) #train sample
table(data.val$yb)/nrow(data.val) #test sample

#this is data frame for timeline 
#try with only data that are connected with stock/returns feature (cluster 3)
dataclust=sentometrics::select_measures(sentMerged, 
                                        toSelect=c("LM","myLexicon","prod_results",
                                                   "drug_pharma","oth_press"),
                                        do.combine=FALSE)
dataclust=dataclust$measures[,-1]
dataclust=cbind(data.frame(yb=yb, glob1=globC1$global,glob2=globC2$global,
                           glob4=globC4$global,dataclust
))
#split into development and validation sample
dataclust.dev=dataclust[sentMerged$measures$date<='2011-07-01',]
dataclust.val=dataclust[sentMerged$measures$date>'2011-07-01',]

#
########## LOGISTIC REGRESSSION #####################################################
###returns
#full regression
fullmod=glm(yb ~ ., family=binomial(link="logit"), data=data) # all the variables
summary(fullmod)

#nothing
nothing=glm(yb ~ 1, family=binomial(link="logit"),data=data)
summary(nothing)

###events
fullmod=glm(yb ~ ., family=binomial(link="logit"), data=dataclust) # all the variables
summary(fullmod)

#nothing
nothing=glm(yb ~ 1, family=binomial(link="logit"),data=dataclust)
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

final_model=glm(formula = yb ~ glob1 + glob3 +
                  LM..web..equal_weight + LM..increase..equal_weight + 
                  GI..web..equal_weight + GI..increase..equal_weight + 
                  HENRY..web..equal_weight  + 
                  myLexicon..web..linear + myLexicon..detect..linear + 
                  GI..web..linear + GI..increase..linear + GI..detect..linear + 
                  HENRY..detect..linear + myLexicon..oth_press..equal_weight + 
                  LM..oth_press..equal_weight + GI..oth_press..equal_weight + 
                  LM..oth_press..linear + GI..oth_press..linear + myLexicon..stock_news..equal_weight + 
                  LM..stock_news..equal_weight + GI..stock_news..equal_weight + 
                  HENRY..stock_news..equal_weight + LM..stock_news..linear + 
                  GI..stock_news..linear  + LM..prod_results..linear + 
                  LM..drug_pharma..equal_weight  + 
                  HENRY..drug_pharma..equal_weight, family = binomial(link = "logit"), 
                data = data)


plot(final_model$fitted.values, ylim=c(0, 1),main="Reputation based on abnormal returns",
     ylab="Target variable")
lines(as.numeric(as.character(yb)), type="p", col="red") # does it change when reputation ("stock market") event?
abline(h=0.5)
legend(90,0.9, # places a legend at the appropriate place 
       c("fitted values","real values"),
       lty=c(0,0),
       lwd=c(1,1),col=c("black","red"),
       pch=c(1,1)) # puts text in the legend

# this is the best model for abnormal returns modelling with a use of cluster with financial features  
final_model_v1=glm(formula = yb ~ glob1 + glob2 + glob4 + myLexicon..web..equal_weight + 
                     myLexicon..detect..equal_weight + LM..web..equal_weight + 
                     myLexicon..web..linear + myLexicon..detect..linear + LM..web..linear + 
                     LM..increase..linear + myLexicon..oth_press..equal_weight + 
                     LM..oth_press..equal_weight + GI..oth_press..equal_weight + 
                     HENRY..oth_press..equal_weight + myLexicon..oth_press..linear + 
                     GI..oth_press..linear + HENRY..oth_press..linear + myLexicon..stock_news..equal_weight + 
                     LM..stock_news..linear + myLexicon..prod_results..linear + 
                     LM..prod_results..linear + myLexicon..drug_pharma..equal_weight + 
                     LM..drug_pharma..equal_weight + GI..drug_pharma..equal_weight + 
                     HENRY..drug_pharma..equal_weight + GI..drug_pharma..linear, 
                   family = binomial(link = "logit"), data = dataclust)

summary(final_model_v1)
plot(final_model_v1$fitted.values, ylim=c(0, 1),main="Reputation based on abnormal returns",
     ylab="Target variable")
lines(as.numeric(as.character(yb)), type="p", col="red") # does it change when reputation ("stock market") event?
abline(h=0.5)
legend(90,0.9, # places a legend at the appropriate place 
       c("fitted values","real values"),
       lty=c(0,0),
       lwd=c(1,1),col=c("black","red"),
       pch=c(1,1)) # puts text in the legend

###regression diagnostic and evaluation
anova(final_model, test="Chisq")
anova(final_model_v1, test="Chisq")

#pseudo R^2
library(pscl)
pR2(final_model)[4]  # look for 'McFadden'
pR2(final_model_v1)[4] # look for 'McFadden'

#Wald test
library(survey)
regTermTest(final_model, "glob2:glob4")#fails to reject, var should be removed
regTermTest(final_model, "glob4:my.lex")

#variable importance
library(caret)
varImp(final_model)
varImp(final_model_v1)

#prediction plus prediction evaluation
pred.link=predict(final_model,data.val, type="link")
pred.response=predict(final_model,data.val,type='response')

pred.link2=predict(final_model_v1,dataclust.val, type="link")
pred.response2=predict(final_model_v1,dataclust.val,type='response')

score_data=data.frame(link=pred.link, 
                      response=pred.response,
                      y=data.val$yb,
                      stringsAsFactors=FALSE)

score_data %>% 
  ggplot(aes(x=link, y=response, col=y)) + 
  scale_color_manual(values=c("black", "red")) + 
  geom_point() + 
  geom_rug() + 
  ggtitle("Both link and response scores put cases in the same order")

score_data2=data.frame(link=pred.link2, 
                       response=pred.response2,
                       y=dataclust.val$yb,
                       stringsAsFactors=FALSE)

score_data2 %>% 
  ggplot(aes(x=link, y=response, col=y)) + 
  scale_color_manual(values=c("black", "red")) + 
  geom_point() + 
  geom_rug() + 
  ggtitle("Both link and response scores put cases in the same order")

#Accuracy computation
pred.response=ifelse(pred.response > 0.5,1,0)
misClasificError=mean(pred.response != data.val$yb)
print(paste('Accuracy',1-misClasificError)) #print prediction accuracy

pred.response2=ifelse(pred.response2 > 0.5,1,0)
misClasificError=mean(pred.response2 != dataclust.val$yb)
print(paste('Accuracy',1-misClasificError)) #print prediction accuracy

#plotting ROC curve, plotting TPR against FPR
library(pROC)
plot(roc(data.val$yb,pred.response, direction="<"),
     col="blue", lwd=3, main="ROC curve for extraordinary
     return change prediction")

#AUC computation
library(ROCR)
pr=prediction(pred.response,data.val$yb)
auc=performance(pr, measure = "auc")
auc=auc@y.values[[1]]
auc #0.5 = behaves like a random model, 0.8279 on data that it was trained on

############################################################
###### Evaluations for validation sample #################
########################################################

#plotBinary(pred.response[yb == 1], yb[yb == 1])
TP=sum(pred.response[pred.response == 1] ==  data.val$yb[pred.response == 1]) # true positives
TN=sum(pred.response[pred.response == 0] ==  data.val$yb[pred.response == 0]) # true negatives
FP =sum(pred.response[pred.response == 1] !=  data.val$yb[pred.response == 1]) # false positives
FN =sum(pred.response[pred.response == 0] !=  data.val$yb[pred.response == 0]) # false negatives
TPR = TP / (TP + FN)
TNR =TN / (TN + FP)
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

#same evaluation measures for the second model
TP=sum(pred.response2[pred.response2 == 1] == dataclust.val$yb[pred.response2 == 1]) # true positives
TN=sum(pred.response2[pred.response2 == 0] ==  dataclust.val$yb[pred.response2 == 0]) # true negatives
FP =sum(pred.response2[pred.response2 == 1] !=dataclust.val$yb[pred.response2 == 1]) # false positives
FN =sum(pred.response2[pred.response2 == 0] != dataclust.val$yb[pred.response2 == 0]) # false negatives
TPR = TP / (TP + FN)
TNR =TN / (TN + FP)
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy


############################################################
###### Evaluations for full sample #################
########################################################
pred.response=predict(final_model,data,type='response')
pred.response=ifelse(pred.response > 0.5,1,0)
pred.response2=predict(final_model_v1,dataclust,type='response')
pred.response2=ifelse(pred.response2 > 0.5,1,0)

TP=sum(pred.response[pred.response == 1] ==  data$yb[pred.response == 1]) # true positives
TN=sum(pred.response[pred.response == 0] ==  data$yb[pred.response == 0]) # true negatives
FP =sum(pred.response[pred.response == 1] !=  data$yb[pred.response == 1]) # false positives
FN =sum(pred.response[pred.response == 0] !=  data$yb[pred.response == 0]) # false negatives
TPR = TP / (TP + FN)
TNR =TN / (TN + FP)
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

#validated on full sample
plot(roc(data$yb,pred.response, direction="<"),
     col="blue", lwd=3, main="ROC curve for extraordinary
     return change prediction")

pred.response=predict(final_model,data,type='response')
pr=prediction(pred.response,data.val$yb)
auc=performance(pr, measure = "auc")
auc=auc@y.values[[1]]
auc #0.8279 on data that it was trained on

#same evaluation measures for the second model
TP=sum(pred.response2[pred.response2 == 1] == dataclust$yb[pred.response2 == 1]) # true positives
TN=sum(pred.response2[pred.response2 == 0] ==  dataclust$yb[pred.response2 == 0]) # true negatives
FP =sum(pred.response2[pred.response2 == 1] !=dataclust$yb[pred.response2 == 1]) # false positives
FN =sum(pred.response2[pred.response2 == 0] != dataclust$yb[pred.response2 == 0]) # false negatives
TPR = TP / (TP + FN)
TNR =TN / (TN + FP)
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

################ SPARSE REGRESSION #########################################################
library("glmnet")
sparse2=cv.glmnet(x=as.matrix(sentMerged$measures[, -1]), y=yb, family="binomial", alpha=1, type.measure="class", nfolds=10)
plot(sparse2)
pred2=predict(sparse2, newx=as.matrix(sentMerged$measures[, -1]), type="class", s="lambda.min")
plotBinary(pred2, yb)
coef=coef(sparse2, s="lambda.min")

#prediction evaluation
attr2=attribution_cv_glmnet(sparse2, sentMerged, sentMerged$features, "features")
pred2l=predict(sparse2, newx=as.matrix(sentMerged$measures[, -1]), type="link", s="lambda.min") # linear equation
(pred2l - coef(sparse2, s="lambda.min")[1, ]) - rowSums(attr2[, -1]) # very close to zero
plot_attributions_cv_glmnet(attr2)

plotBinary(pred2[yb == 1], yb[yb == 1])
TP=sum(pred2[pred2 == 1] == yb[pred2 == 1]) # true positives
TN=sum(pred2[pred2 == 0] == yb[pred2 == 0]) # true negatives
FP =sum(pred2[pred2 == 1] != yb[pred2 == 1]) # false positives
FN =sum(pred2[pred2 == 0] != yb[pred2 == 0]) # false negatives
TPR = TP / (TP + FN) #recall
PR=TP/(TP+TN) #precission
TNR =TN / (TN + FP) #true negative rate
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

#####################################################################################
####################### Random forest ########################################
library(randomForest)
#returns as target variable

set=data.dev
set.val=data.val
set=data
set2=dataclust.dev
set2.val=dataclust.dev

#randomForest
rf = randomForest(yb ~ .,  
                  ntree = 100,
                  data = set)
plot(rf) 
print(rf)

# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")
var.imp = data.frame(importance(rf,  
                                type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
prediction = predict(rf , set)
# Create Confusion Matrix
print(  
  confusionMatrix(data=prediction,  
                  reference=set$yb))

# Create Confusion Matrix
# Predicting response variable
prediction.val=predict(rf ,set.val)

# Create Confusion Matrix
print(  
  confusionMatrix(data=prediction.val,  
                  reference=set.val$yb))

#these measures are on full sample
TP=19 # true positives
TN=3882 # true negatives
FP =0 # false positives
FN =2# false negatives
TPR = TP / (TP + FN) #recall
PR=TP/(TP+TN) #precission
TNR =TN / (TN + FP) #true negative rate
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

#randomForest
rf = randomForest(yb ~ .,  
                  ntree = 100,
                  data = set2)
plot(rf) 
print(rf)

# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")
var.imp = data.frame(importance(rf,  
                                type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
prediction = predict(rf , set2)
# Create Confusion Matrix
print(  
  confusionMatrix(data=prediction,  
                  reference=set2$yb))

# Create Confusion Matrix
# Predicting response variable
prediction.val=predict(rf ,set2.val)

# Create Confusion Matrix
print(  
  confusionMatrix(data=prediction.val,  
                  reference=set2.val$yb))
#full sample
prediction = predict(rf ,dataclust)
# Create Confusion Matrix
print(  
  confusionMatrix(data=prediction,  
                  reference=dataclust$yb))

#these measures are on full sample
TP=7 # true positives
TN=3882 # true negatives
FP =0 # false positives
FN =14# false negatives
TPR = TP / (TP + FN) #recall
PR=TP/(TP+TN) #precission
TNR =TN / (TN + FP) #true negative rate
accT= (TP + TN) / (TP + FP + TN + FN) # total accuracy

save.image(file='model2.RData')
