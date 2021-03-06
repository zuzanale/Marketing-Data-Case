
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

setwd("/Users/macbookpro/documents/Master Econometrics/Marketing Data Case/R scripts")

name <- "CORPUS_GILEAD" 
load(paste0(name, ".rda")) # corpusFirm

corpus <- sentometrics::sento_corpus(corpusFirm)
corpus <- add_features(corpus, keywords = list(reputation = c("reputation", "brand")))
lexiconsIn <- lexicons[c("LM_eng", "GI_eng", "HENRY_eng")]
lexIn <- sentometrics::setup_lexicons(lexiconsIn=lexiconsIn, valenceIn=valence[["valence_eng"]])

ctr <- sentometrics::ctr_agg(howWithin="tf-idf",
                             howDocs="equal_weight",
                             howTime=c("equal_weight", "linear", "exponential"),
                             by="month",
                             lag=12,
                             alphasExp=c(0.2, 0.5),
                             do.ignoreZeros=FALSE,
                             fill="zero")

sentMeas <- sentometrics::sento_measures(corpus, lexicons=lexIn, ctr=ctr)
plot(sentMeas, group="features")

####################################################################################################### modelling

library("quantmod")
getSymbols("GILD", src="google") # GILEAD's stock prices
# modify dates to yyyy-mm-01 format in the sentiment measures
monthsAll <- unlist(lapply(stringi::stri_split(index(GILD), regex = "-"), function(d) return(paste0(d[1:2], collapse = "-"))))
index(GILD) <- as.Date(paste0(monthsAll, "-01"), format = "%Y-%m-%d")
volume <- as.data.table(GILD)[, list(volM = sum(GILD.Volume)), by = list(index)]
target <- volume
# align sentiment and target variable
sentMeasIn <- select_measures(sentMeas, dates=index(GILD))
dates <- sentMeasIn$measures$date
target <- target[index %in% as.Date(dates)] # to align target and sentiment variables
dim(target)[1] == dim(sentMeasIn$measures)[1] # TRUE
target$index <- NULL

# pr <- CMG[, 4]
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
topDates
y <- rep(0, length(dates)) # no "event"
names(y) <- dates
y[as.Date(names(y)) %in% topDates] <- 1 # "event"
yb <- as.factor(y) # your base class in logistic regression is the last one appearing in levels(yb)
yb
library("doParallel")
cl <- makeCluster(4)
registerDoParallel(cl)
ctrModel <- ctr_model(model="binomial", type="cv", h=0, alphas=1, trainWindow=60, testWindow=10, do.parallel=TRUE) # LASSO
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

ctrMerge <- ctr_merge(sentMeasIn, 
                      features = list(FEAT = sentMeasIn$features),
                      time = list(TIME = sentMeasIn$time))
sentMerged <- merge_measures(ctrMerge)
plot(sentMerged)
glob <- to_global(sentMeasIn)
data <- data.frame(yb=yb, glob=glob$global, gi=sentMerged$measures$`GI_eng--FEAT--TIME`, 
                   he=sentMerged$measures$`HENRY_eng--FEAT--TIME`, lm=sentMerged$measures$`LM_eng--FEAT--TIME`)

out3 <- glm(yb ~ gi + he + lm, family=binomial(link="logit"), data=data) # 3 regressors
summary(out3)
plot(out3$fitted.values, ylim=c(0, 1))
lines(as.numeric(as.character(yb)), type="p", col="red") # does it change when reputation ("stock market") event?
# par(xpd=FALSE)
abline(h=0.5)
  
out4 <- glm(yb ~ glob, family=binomial(link="logit"), data=data) # 1 regressor
summary(out4)
plot(out4$fitted.values, ylim=c(0, 1))
lines(as.numeric(as.character(yb)), type="p", col="red") # does it change when reputation ("stock market") event?
abline(h=0.5)

