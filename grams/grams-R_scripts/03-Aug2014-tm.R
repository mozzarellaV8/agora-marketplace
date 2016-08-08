# Agora Marketplace
# Text Mining - August 2014 listings

# Aug 2014 --------------------------------------------------------------------

library(data.table)
library(tm)

aug2014 <- fread("data/03-aug2014.csv")

nrow(aug2014) # 178267
length(unique(aug2014$name))
# 20391 of 178267
length(unique(aug2014$description))
# 14605 of 178267

# create corpus of august listings
aug2014corpus <- Corpus(VectorSource(aug2014$name))

# preprocess
aug2014corpus <- tm_map(aug2014corpus, removePunctuation)
aug2014corpus <- tm_map(aug2014corpus, removeWords, 
                       stopwords("english"))
aug2014corpus <- tm_map(aug2014corpus, stripWhitespace)
aug2014corpus <- tm_map(aug2014corpus, PlainTextDocument)

# Document-Term Matrix --------------------------------------------------------

aug2014dtm <- DocumentTermMatrix(aug2014corpus)
aug2014dtm

aug2014dtm.m<- as.matrix(aug2014dtm)

# write out aug2014 DocumentTermMatrix
write.table(aug2014dtm.m, file = "data-dtm/03-aug2014dtm.csv",
            sep = ",", row.names = F)

aug2014freq <- colSums(as.matrix(aug2014dtm))
aug2014ord <- order(aug2014freq, decreasing = TRUE)

head(aug2014freq[aug2014ord])
#   free  mdma  gram   100 pills grams 
#  15009 14698 13335 10329  9291  8917 

tail(aug2014freq[aug2014ord])
# spankincrystal          syrup        thieves         wafers          whizz    whizzinator 
#              1              1              1              1              1              1

aug2014wf <- data.frame(word = names(aug2014freq[aug2014ord]),
                       frequency = aug2014freq[aug2014ord])
rownames(aug2014wf) <- NULL

# write out aug2014 wordfreq dataframe
write.table(aug2014wf, file = "data-dtm/03-aug2014wf.csv",
            sep = ",", row.names = F)

# wordcloud -------------------------------------------------------------------
library(wordcloud)
library(RColorBrewer)
library(extrafont)
fonts()

aug2014wf <- fread("data-dtm/02-aug2014wf.csv")
dtm <- fread("data-dtm/03-aug2014-dtm.csv")

redpal <- brewer.pal(5, "Reds")

par(family = "Arial Rounded MT Bold")
set.seed(144)
wordcloud(aug2014wf$word, aug2014wf$frequency, scale = c(4, 0.75),
          min.freq = 500, random.order = T, random.color = F,
          colors = redpal)

# frequency plot --------------------------------------------------------------

library(ggplot2)

wordfreq <- ggplot(subset(aug2014wf, aug2014wf$frequency > 5000), aes(x = reorder(word, frequency),
                                                                    y = frequency, 
                                                                    fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1)) +
  labs(title = "Agora August 2014 Listings: Term Frequencies > 5000",
       x = "", y = "frequency", fill = "")

wordfreq + labs(title = "AgMarketplace August 2014 Listings: Term Frequencies > 5000", 
                x = "", y = "frequency", fill = "")


wordfreq2 <- ggplot(subset(aug2014wf, aug2014wf$frequency > 2500), aes(x = reorder(word, frequency),
                                                                     y = frequency, 
                                                                     fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace August 2014 Listings: Term Frequencies > 2500",
       x = "", y = "frequency", fill = "")

wordfreq2

wordfreq3 <- ggplot(subset(aug2014wf, aug2014wf$frequency > 1000), aes(x = reorder(word, frequency),
                                                                       y = frequency, 
                                                                       fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Tahoma") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace August 2014 Listings: Term Frequencies > 1000",
       x = "", y = "frequency", fill = "")

wordfreq3

dotplot <- ggplot(subset(aug2014wf, aug2014wf$frequency > 2000), 
                  aes(x = frequency, y = reorder(word, frequency))) +
  geom_segment(aes(yend = word), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = frequency)) +
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 10, base_family = "Tahoma") +
  theme(panel.grid.major.y = element_blank()) +
  theme(plot.margin = unit(c(2, 1, 0.5, 1), "cm")) +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 11, angle = 0)) +
  labs(title = "AgMarketplace August 2014 Listings: Term Frequencies > 2000",
       x = "", y = "", fill = "")

dotplot + labs(title = "AgMarketplace August 2014 Listings: Term Frequencies > 2000", 
               x = "", y = "", fill = "")

# Hierarchical Clustering -----------------------------------------------------

library(cluster)

d <- dist(t(aug2014dtm), method = "euclidean")



