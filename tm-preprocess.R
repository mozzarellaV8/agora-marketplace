# Text Mining - Preprocessing to DTM

library(tm)

load.agora <- file.path("~/GitHub/posts")
dir(load.agora)
agora <- Corpus(DirSource(load.agora))

# Preprocessing  --------------------------------------------------------------

agora <- tm_map(agora, removePunctuation)
agora <- tm_map(agora, removeNumbers)
agora <- tm_map(agora, tolower)
agora <- tm_map(agora, removeWords, stopwords("english"))
agora <- tm_map(agora, removeWords, c("forums", "quote", "thread")
                agora <- tm_map(agora, stemDocument)
                agora <- tm_map(agora, stripWhitespace)
                agora <- tm_map(agora, PlainTextDocument)
                
 # stage the DocumentTermMatrix ------------------------------------------------
dtm <- DocumentTermMatrix(agora) # tf vs. tf-idf
tdm <- TermDocumentMatrix(agora)
dtm 
                
# organize terms by frequency -------------------------------------------------
freq <- colSums(as.matrix(dtm))
ord <- order(freq, decreasing = TRUE) 
length(freq)
                
freq[head(ord)]
freq[tail(ord)]
                
head(table(freq), 7)
tail(table(freq), 7)
                
# word frequency -------------------------------------------------------------
                
findFreqTerms(dtm, lowfreq = 500)
                
# write csv of the DTM --------------------------------------------------------
                
wf <- data.frame(rank = 1:161348, words = names(freq[ord]), freq = freq[ord])
write.table(wf, file = "agora_freq_V2.csv", sep = ",", row.names = FALSE)