# Agora Marketplace
# Text Mining - 'Description' Field
# Descriptions of Market Listings

rm(list = ls())

# load data -------------------------------------------------------------------

library(Hmisc)

desk <- read.csv("~/GitHub/agora-data/data/descriptions.csv")
str(desk)
summary(desk)

describe(desk)


# Preprocess Corpus -----------------------------------------------------------

library(tm)
library(NLP)
library(openNLP)
library(qdap)

deskC <- Corpus(VectorSource(desk$Description))

deskC <- tm_map(deskC, removePunctuation)
deskC <- tm_map(deskC, removeWords, stopwords("english"))
# nice: there is metadata in the descriptions:
# datetimestamp in POSIXlt[1:1], format: "2016-07-28 04:20:16"



