# Agora Marketplace
# Text Mining Descriptions
# July 2014 Listings

# July 2014 -------------------------------------------------------------------

library(data.table)
library(tm)

july14 <- fread("data/02-july14.csv")

nrow(july14) # 305894
length(unique(july14$name))
# 19469 of 305894
length(unique(july14$description))
# 14058 of 305894

# create corpus of july listings
july14corpus <- Corpus(VectorSource(july14$name))

# preprocess
july14corpus <- tm_map(july14corpus, removePunctuation)
july14corpus <- tm_map(july14corpus, removeWords, 
                       stopwords("english"))
july14corpus <- tm_map(july14corpus, stripWhitespace)
july14corpus <- tm_map(july14corpus, PlainTextDocument)



# Document-Term Matrix ------------------------------------
july14dtm <- DocumentTermMatrix(july14corpus)
inspect(july14dtm)

# not exactly
library(slam)
july14dtm2 <- slam::row_sums(july14dtm, na.rm = T)
summary(july14dtm2)
class(july14dtm2)
names(july14dtm2)[1, ]

# too large; remove sparse terms
july14dtm3 <- removeSparseTerms(july14dtm, 0.98)
inspect(july14dtm3)

# write out a CSV in case
july14.m <- as.matrix(july14dtm3)
write.table(july14.m, file = "data-dtm/01-july14dtm.csv", 
            sep = ",", row.names = F)

# look at word frequencies
july14freq <- colSums(as.matrix(july14dtm3))
july14ord <- order(july14freq , decreasing = TRUE)

head(july14freq[july14ord])
#   mdma  gram  free   100 pills  pure 
#  25075 23050 22507 17320 15133 15090

tail(july14freq[july14ord])
# speed white   500  kush   200 100mg 
#  6840  6653  6650  6521  6388  6209

# data frame of word frequencies
july14wf <- data.frame(word = names(july14freq[july14ord]), 
                       frequency = july14freq[july14ord])
rownames(july14wf) <- NULL

# wordcloud July 2014 -------------------------------------
set.seed(144)
par(mar = c(1, 1, 1, 1), family = "Arial Rounded MT Bold")
wordcloud(july14wf$word, july14wf$frequency, scale = c(6, 0.5), min.freq = 50,
          random.order = TRUE, random.color = FALSE,
          colors = redpal)