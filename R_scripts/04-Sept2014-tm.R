# Agora Marketplace
# Text Mining - September 2014 listings

# Sept 2014 --------------------------------------------------------------------

library(data.table)
library(tm)

sept2014 <- fread("data/04-sept2014.csv")

nrow(sept2014) # 311866
length(unique(sept2014$name))
# 20491 of 311866
length(unique(sept2014$description))
# 14770 of 311866

# create corpus of September listings
sept2014corpus <- Corpus(VectorSource(sept2014$name))

# preprocess
sept2014corpus <- tm_map(sept2014corpus, removePunctuation)
sept2014corpus <- tm_map(sept2014corpus, removeNumbers)
sept2014corpus <- tm_map(sept2014corpus, removeWords, 
                        stopwords("english"))
sept2014corpus <- tm_map(sept2014corpus, stripWhitespace)
sept2014corpus <- tm_map(sept2014corpus, PlainTextDocument)

# writeLines(as.character(sept2014corpus), con="sept2014cor.txt")

# Document-Term Matrix --------------------------------------------------------

sept2014dtm <- DocumentTermMatrix(sept2014corpus)
sept2014dtm

dtms <- removeSparseTerms(sept2014dtm, 0.999)
inspect(dtms[1:10, 1:10])

sept2014dtm.m<- as.matrix(dtms)

# write out sept2014 DocumentTermMatrix
write.table(sept2014dtm.m, file = "data-dtm/04-sept2014-dtm.csv",
            sep = ",", row.names = F)

sept2014freq <- colSums(as.matrix(dtms))
sept2014ord <- order(sept2014freq, decreasing = TRUE)

head(sept2014freq[sept2014ord])
#   free     mdma     gram      100     pure shipping 
#  26347    24597    21768    17079    16504    15557

tail(sept2014freq[sept2014ord])
#    999 ketama   polm   chew edible   dead 
#    316    316    316    314    314    312 

sept2014wf <- data.frame(word = names(sept2014freq[sept2014ord]),
                        frequency = sept2014freq[sept2014ord])
rownames(sept2014wf) <- NULL

# write out sept2014 wordfreq dataframe
write.table(sept2014wf, file = "data-dtm/04-sept2014-wf.csv",
            sep = ",", row.names = F)

# wordcloud -------------------------------------------------------------------
library(wordcloud)
library(RColorBrewer)
library(extrafont)
fonts()

sept2014wf <- fread("data-dtm/04-sept2014-wf.csv")
dtm <- fread("data-dtm/04-sept2014-dtm.csv")

redpal <- brewer.pal(4, "Reds")

par(family = "Arial Rounded MT Bold")
set.seed(144)
wordcloud(sept2014wf$word, sept2014wf$frequency, scale = c(4, 0.75),
          min.freq = 500, random.order = T, random.color = F,
          colors = redpal)

# frequency plot --------------------------------------------------------------

library(ggplot2)

wordfreq <- ggplot(subset(sept2014wf, sept2014wf$frequency > 5000), aes(x = reorder(word, frequency),
                                                                      y = frequency, 
                                                                      fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace September 2014 Listings: Term Frequencies > 5000",
       x = "", y = "frequency", fill = "")

wordfreq

wordfreq2 <- ggplot(subset(sept2014wf, sept2014wf$frequency > 2500), aes(x = reorder(word, frequency),
                                                                       y = frequency, 
                                                                       fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace September 2014 Listings: Term Frequencies > 2500",
       x = "", y = "", fill = "Frequency")

wordfreq2

wordfreq3 <- ggplot(subset(sept2014wf, sept2014wf$frequency > 1000), aes(x = reorder(word, frequency),
                                                                       y = frequency, 
                                                                       fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Tahoma") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace September 2014 Listings: Term Frequencies > 1000",
       x = "", y = "frequency", fill = "Frequency")

wordfreq3

dotplot <- ggplot(subset(sept2014wf, sept2014wf$frequency > 2500), 
                  aes(x = frequency, y = reorder(word, frequency))) +
  geom_segment(aes(yend = word), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = frequency)) +
  theme_minimal(base_size = 10, base_family = "Tahoma") +
  theme(panel.grid.major.y = element_blank()) +
  theme(plot.margin = unit(c(2, 1, 0.5, 1), "cm")) +
  theme(axis.text.x = element_text(size = 11, angle = 0)) +
  theme(axis.text.y = element_text(size = 9, angle = 0)) +
  labs(title = "AgMarketplace September 2014 Listings: Term Frequencies > 2500",
       x = "", y = "", fill = "Frequency")

dotplot + scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  scale_x_continuous(limits = c(0, 25000))

# Kmeans clustering -----------------------------------------------------------

library(fpc)
library(cluster)
library(NbClust)

findAssocs(sept2014dtm, "mdma", .8)
dtm_tfidf <- weightTfIdf(dtms)

m <- as.matrix(dtms)
row.names(m) <- 1:nrow(m)

# normalize matrix values for kmeans ----------------------

install.packages("biganalytics")
library(biganalytics)

norm_eucl <- function(m) m/ apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
m_norm <- na.omit(m_norm)

m_norm_df <- as.data.frame(m_norm)
write.table(m_norm_df, file = "data-dtm/m_norm_df.csv", sep = ",", row.names = FALSE)

m_norm <- as.big.matrix(m_norm)

km01 <- bigkmeans(m_norm, centers = 12, iter.max = 10, nstart = 3, dist = "euclid")
km01 <- bigkmeans(m_norm, centers = 4, iter.max = 10, nstart = 3, dist = "euclid")

kmTable <- as.data.frame(table(km01$cluster))
table(km01$cluster)
#      1      2      3      4      5      6      7      8      9     10 
#  38834   6521  19476 177862   1934   9607   3515  13492  15946  12706 

summary(km01)
#              Length  Class    Mode   
#     cluster  292538 -none- numeric
#     centers    8568 -none- numeric
#     withinss     12 -none- numeric
#     size         12 -none- numeric

unique(m_norm_df)

# clusplot ------------------------------------------------

clusplot(m, clus = km01$cluster)

library(factoextra)
library(RColorBrewer)

clusterplot <- fviz_cluster(km01, m_norm, repel = TRUE) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_color_brewer(palette = "YlOrRd", direction = -1) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "K-Means - Cluster Plot")

clusterplot




