my_compute_sentiment=function(sentocorpus, lexicons,remove = stoplist,how = get_hows()$words, dfm = NULL) 
{ #lex_terms - lexicon where instead of words we have terms with sentiment scores
  #stoplist of words we want to remove
  
  ###debug part
  # sentocorpus=corpus
  # lexicons= c(list(myLexicon=myLexicon),lexicons[c("LM_eng", "GI_eng", "HENRY_eng")] )
  # lex_terms=list(myLexicon=myLexicon)
  
  check_class(sentocorpus, "sentocorpus")
  if (length(how) > 1) 
    how <- how[1]
  if ("valence" %in% names(lexicons)) {
    cat("Modify corpus to account for valence words... ")
    quanteda::texts(sentocorpus) <- include_valence(quanteda::texts(sentocorpus), 
                                                    lexicons[["valence"]])
    cat("Done.", "\n")
  }
  lexNames <- names(lexicons)[names(lexicons) != "valence"]
  features <- names(quanteda::docvars(sentocorpus))[-1]
  cat("Compute sentiment... ")
  if (is.null(dfm)) {
    dfm <- quanteda::dfm(quanteda::tokens(sentocorpus, remove_punct = F, 
                                          remove_numbers =F, remove_symbols = F, remove_separators = TRUE),
                         remove = stoplist,
                         stem = F, 
                         verbose = FALSE)
      
    dfm.2=quanteda::dfm(quanteda::tokens(sentocorpus, what="sentence",remove_punct = F, 
                                          remove_numbers = F, remove_symbols = F, remove_separators = TRUE), 
                        remove = stoplist,
                        stem = F,  
                        verbose = FALSE)
    
    dfm=cbind(dfm, dfm.2)
  }
  else if (!quanteda::is.dfm(dfm)) 
    stop("The 'dfm' argument should pass quanteda::is.dfm(dfm).")
  if (how == "counts" || how == "proportional") {
    fdm <- quanteda::t(dfm)
  }
  else if (how == "tf-idf") {
    weights <- quanteda::tfidf(dfm, scheme_tf = "prop")
    fdmWeighted <- quanteda::t(weights)
  }
  else stop("Please select an appropriate aggregation 'how'.")
  s <- as.data.table(matrix(0, nrow = quanteda::ndoc(sentocorpus), 
                            ncol = length(lexNames)))
  names(s) <- lexNames
  allWords <- quanteda::featnames(dfm)
  wCounts <- quanteda::rowSums(dfm, na.rm = TRUE)
  for (lexicon in lexNames) {
    lexWords <- lexicons[[lexicon]]$x
    lexScores <- lexicons[[lexicon]]$y
    names(lexScores) <- lexWords
    allScores <- rep(0, length(allWords))
    polInd <- allWords %in% lexWords
    allScores[polInd] <- lexScores[allWords[polInd]]
    names(allScores) <- allWords
    if (how == "counts") {
      scores <- quanteda::rowSums(quanteda::t(fdm * allScores))
    }
    else if (how == "proportional") {
      scores <- quanteda::rowSums(quanteda::t(fdm * allScores))/wCounts
    }
    else scores <- quanteda::rowSums(quanteda::t(fdmWeighted * 
                                                   allScores))
    scores[is.na(scores)] <- 0
    s[, `:=`((lexicon), scores)]
  }
  s <- as.data.table(cbind(id = quanteda::docnames(sentocorpus), 
                           quanteda::docvars(sentocorpus), word_count = wCounts, 
                           s))
  sent <- get_features_sentiment(s, features, lexNames)
  sent <- sent[order(date)]
  cat("Done.", "\n")
  sentOut <- list(corpus = sentocorpus, sentiment = sent, features = features, 
                  lexicons = lexNames, howWithin = how)
  return(sentOut)
}

###define mine sento measures funtion 
my_sento_measures=function (sentocorpus, lexicons, ctr,remove) 
{
  check_class(sentocorpus, "sentocorpus")
  toAgg <- my_compute_sentiment(sentocorpus, lexicons,remove, how = ctr$howWithin)
  sentomeasures <- perform_agg(toAgg, ctr)
  return(sentomeasures)
}