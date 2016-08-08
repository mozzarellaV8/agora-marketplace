# Agora Marketplace
# Text Mining Descriptions
# June 2014 Listings

# load data -------------------------------------------------------------------

library(data.table)
agora <- fread("~/GitHub/agora-data/data/agora.csv")
str(agora)


# text mining 'names' by month ------------------------------------------------

# the 'names' variable contains short 'headline' descriptions of the product 
# being sold. these are short, descriptive titles rather than simple product
# names - hence the mining.

# June 2014 -------------------------------------------------------------------

june14 <- read.csv("~/GitHub/agora-data/data/01-june14.csv")
june14dtm <- fread("~/GitHub/agora-data/data-dtm/01-june14dtm.csv")

library(tm)

nrow(june14) #  35490
length(unique(june14$name))
# 11062 of 35490
length(unique(june14$description))
# 8142 of 35490

june14corpus <- Corpus(VectorSource(june14$name))

# preprocess ----------------------------------------------

june14corpus <- tm_map(june14corpus, removePunctuation)
june14corpus <- tm_map(june14corpus, removeWords, 
                         stopwords("english"))
june14corpus <- tm_map(june14corpus, stripWhitespace)
june14corpus <- tm_map(june14corpus, PlainTextDocument)

# Document-Term Matrix ------------------------------------

june14dtm <- DocumentTermMatrix(june14corpus)

# write out a CSV in case
june14.m <- as.matrix(june14dtm)
write.table(june14.m, file = "data-dtm/01-june14dtm.csv", 
            sep = ",", row.names = F)

# look at word frequencies
june14freq <- colSums(as.matrix(june14dtm))
june14ord <- order(june14freq , decreasing = TRUE)

june14freq[head(june14ord)]
#  mdma  gram pills  free   100  pure 
#  3126  2700  2481  2334  2277  1845

june14freq[tail(june14ord)]
#    zion    zips    zolt    zora     zzz zzzzzzz 
#       3       3       3       3       3       3

findFreqTerms(june14dtm, lowfreq = 500)

# data frame of word frequencies
june14wf <- data.frame(word = names(june14freq[june14ord]), frequency = june14freq[june14ord])
rownames(june14wf) <- NULL
# write it out in case
write.table(june14wf, file = "data-dtm/01-june14wf.csv", sep = ",", row.names = F)

# wordcloud -----------------------------------------------
library(wordcloud)
library(RColorBrewer)

redpal <- brewer.pal(6, "Reds")
set.seed(64)
par(mar = c(1, 1, 1, 1), family = "Arial Rounded MT Bold")
wordcloud(june14wf$word, june14wf$frequency, scale = c(6, 0.5), min.freq = 50,
          random.order = TRUE, random.color = FALSE,
          colors = redpal)

redpal2 <- brewer.pal(4, "Reds")
set.seed(144)
par(mar = c(0, 0, 0, 0), family = "Arial Rounded MT Bold")
wordcloud(june14wf$word, june14wf$frequency, scale = c(7, 1), min.freq = 50,
          random.order = TRUE, random.color = FALSE,
          colors = redpal2)

