# Agora Marketplace
# Text Mining - 'Description' Field
# Descriptions of Market Listings

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

# create corpus
deskC <- Corpus(VectorSource(desk$Description))

# prepro
deskC <- tm_map(deskC, removePunctuation)
deskC <- tm_map(deskC, removeWords, stopwords("english"))
# nice: there is metadata in the descriptions:
# datetimestamp in POSIXlt[1:1], format: "2016-07-28 04:20:16"
# but don't go into the summary() - crashes RStudio

deskC <- tm_map(deskC, stripWhitespace)
deskC <- tm_map(deskC, PlainTextDocument)

# DTM / TDM  ------------------------------------------------------------------

deskTM <- DocumentTermMatrix(deskC)
deskTM

deskDM <- TermDocumentMatrix(deskC)
deskDM

deskTMs <- removeSparseTerms(deskTM, sparse = 0.98)
deskTMs
# 96 terms, 67140 documents
# 1:272936

deskDMs <- removeSparseTerms(deskDM, sparse = 0.98) 

deskMatTM <- as.matrix(deskTMs)
deskMatDM <- as.matrix(deskDMs)

write.table(deskMatTM, file = "~/GitHub/agora-data/data/descriptionTermMatrix.csv",
            sep = ",", row.names = F)
write.table(deskMatDM, file = "~/GitHub/agora-data/data/termDescriptionMatrix.csv",
            sep = ",", row.names = F)

# DTM frequencies -----------------------------------------

dtmFreq <- colSums(deskMatTM)
dtmOrd <- order(dtmFreq, decreasing = T)

head(dtmFreq[dtmOrd])
#  listing  quality     will shipping   please     high 
#    12450    11522    10275     8274     7991     7629

tail(dtmFreq[dtmOrd])
#     special         thc     generic     express       grown information 
#        1508        1503        1490        1476        1453        1450

dtmDF <- data.frame(term = names(dtmFreq[dtmOrd]), frequency = dtmFreq[dtmOrd])
rownames(dtmDF) <- NULL

write.table(dtmDF, file = "~/GitHub/agora-data/data/descTM-tf.csv")

# TDM frequencies -----------------------------------------

tdmFreq <- rowSums(deskMatDM)
tdmOrd <- order(tdmFreq, decreasing = T)

head(tdmFreq[tdmOrd])
#  listing  quality     will shipping   please     high 
#    12450    11522    10275     8274     7991     7629

tail(tdmFreq[tdmOrd])
#     special         thc     generic     express       grown information 
#        1508        1503        1490        1476        1453        1450

# K so DTM and TDM match. cool i guess.

# k-means clustering ----------------------------------------------------------

library(fpc)
library(cluster)
library(NbClust)

dtm_tfidf <- weightTfIdf(deskTMs)

# scale() on DTM
deskMatScale <- as.data.frame(round(scale(deskMatTM), digits = 4))
is.na(deskMatScale)
is.nan(deskMatScale)

# m hahsler euclidean normalizing
norm_eucl <- function(deskMatTM) 
  deskMatTM / apply(deskMatTM, MARGIN=1, FUN=function(x) sum(x^2)^.5)
desk_norm <- norm_eucl(deskMatTM)
desk_norm <- na.omit(desk_norm)


desk_norm_df <- as.data.frame(desk_norm)

set.seed(64)
desk.km <- kmeans(desk_norm, centers = 4, iter.max = 10, nstart = 3)
desk.km


