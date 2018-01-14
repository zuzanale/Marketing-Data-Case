######## those two initial steps should have been done in master script
#set working directory of the project
setwd("C:/Users/lezuz/OneDrive/Documents/VU/Marketing Data Case/scripts")

###load our corpus into environmet
load("CORPUS_GILEAD.rda")

#instal ldatuning package
#install.packages("ldatuning")

library("ldatuning")
#library("topicmodels")

result <- FindTopicsNumber(
  gilead_DTM,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)
saveRDS(result, "nooftopics.rds")
