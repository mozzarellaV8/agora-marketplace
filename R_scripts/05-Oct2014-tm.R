# Agora Marketplace
# Text Mining - October 2014 listings

# oct 2014 --------------------------------------------------------------------

library(data.table)
library(tm)

oct2014 <- fread("data/05-oct2014.csv")

nrow(oct2014) # 232987
length(unique(oct2014$name))
# 221889 of 232987
length(unique(oct2014$description))
# 15593 of 232987

# remove some high priced outliers
# removed DutchTruthTeller - description 'dontorder'
oct2014 <- subset(oct2014, oct2014$btc != 99999.0000)
plot(oct2014$btc)

# remove Optiman 'ideas' and Anonymous Land in Panama
oct2014 <- subset(oct2014, oct2014$btc < 4000.00)
plot(oct2014$btc)

# subset for under 500 BTC listings
oct2014_500 <- subset(oct2014, oct2014$btc <= 500)
plot(oct2014_500$btc)
plot(oct2014_500$usd)

# subset for under 500 USD listings
oct2014_500usd <- subset(oct2014, oct2014$usd <= 500)
plot(oct2014_500usd$usd)

# keyword text mining -------------------------------------

# create corpus of October listings
oct2014corpus <- Corpus(VectorSource(oct2014$name))

# preprocess
oct2014corpus <- tm_map(oct2014corpus, removePunctuation)
oct2014corpus <- tm_map(oct2014corpus, removeNumbers)
oct2014corpus <- tm_map(oct2014corpus, removeWords, 
                         stopwords("english"))
oct2014corpus <- tm_map(oct2014corpus, stripWhitespace)
oct2014corpus <- tm_map(oct2014corpus, PlainTextDocument)

# writeLines(as.character(oct2014corpus), con="data-tm/05-oct2014cor.txt", sep = ",")

# Document-Term Matrix --------------------------------------------------------

oct2014dtm <- DocumentTermMatrix(oct2014corpus)
oct2014dtm

dtms <- removeSparseTerms(oct2014dtm, 0.9985)
inspect(dtms[1:10, 1:10])

oct2014dtm.m<- as.matrix(dtms)

# write out oct2014 DocumentTermMatrix
write.table(oct2014dtm.m, file = "data-dtm/05-oct2014-dtm.csv",
            sep = ",", row.names = F)

oct2014freq <- colSums(as.matrix(dtms))
oct2014ord <- order(oct2014freq, decreasing = TRUE)

head(oct2014freq[oct2014ord])
#   free     mdma     gram    pills     pure shipping 
#  19579    18507    16214    12978    11753    11641

tail(oct2014freq[oct2014ord])
#  complexed   facebook nitrazepam      slims    heroine        pdf 
#        355        352        352        352        351        350 

oct2014wf <- data.frame(word = names(oct2014freq[oct2014ord]),
                         frequency = oct2014freq[oct2014ord])
rownames(oct2014wf) <- NULL

# write out oct2014 wordfreq dataframe
write.table(oct2014wf, file = "data-dtm/05-oct2014-wf.csv",
            sep = ",", row.names = F)

# wordcloud -------------------------------------------------------------------
library(wordcloud)
library(RColorBrewer)

oct2014wf <- fread("data-dtm/05-oct2014-wf.csv")
dtm <- fread("data-dtm/05-oct2014-dtm.csv")

redpal <- brewer.pal(7, "Reds")

par(family = "Arial Rounded MT Bold")
set.seed(144)
wordcloud(oct2014wf$word, oct2014wf$frequency, scale = c(4.5, 0.75),
          min.freq = 500, random.order = T, random.color = F,
          colors = redpal)

# frequency plot --------------------------------------------------------------

library(ggplot2)

wordfreq <- ggplot(subset(oct2014wf, oct2014wf$frequency > 5000), aes(x = reorder(word, frequency),
                                                                        y = frequency, 
                                                                        fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace October 2014 Listings: Term Frequencies > 5000",
       x = "", y = "frequency", fill = "")

wordfreq

wordfreq2 <- ggplot(subset(oct2014wf, oct2014wf$frequency > 2500), aes(x = reorder(word, frequency),
                                                                         y = frequency, 
                                                                         fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace October 2014 Listings: Term Frequencies > 2500",
       x = "", y = "", fill = "Frequency")

wordfreq2

wordfreq3 <- ggplot(subset(oct2014wf, oct2014wf$frequency > 1000), aes(x = reorder(word, frequency),
                                                                         y = frequency, 
                                                                         fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Tahoma") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace October 2014 Listings: Term Frequencies > 1000",
       x = "", y = "frequency", fill = "Frequency")

wordfreq3

dotplot <- ggplot(subset(oct2014wf, oct2014wf$frequency > 2500), 
                  aes(x = frequency, y = reorder(word, frequency))) +
  geom_segment(aes(yend = word), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = frequency)) +
  theme_minimal(base_size = 10, base_family = "Tahoma") +
  theme(panel.grid.major.y = element_blank()) +
  theme(plot.margin = unit(c(2, 1, 0.5, 1), "cm")) +
  theme(axis.text.x = element_text(size = 11, angle = 0)) +
  theme(axis.text.y = element_text(size = 9, angle = 0)) +
  labs(title = "AgMarketplace October 2014 Listings: Term Frequencies > 2500",
       x = "", y = "", fill = "Frequency")

dotplot + scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  scale_x_continuous(limits = c(0, 25000))

# Kmeans clustering -----------------------------------------------------------

library(fpc)
library(cluster)
library(NbClust)

# findAssocs(oct2014dtm, "mdma", .8)
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
write.table(m_norm_df, file = "data-dtm/m_norm_df-oct2014.csv", sep = ",", row.names = FALSE)

m_norm_bm <- as.big.matrix(m_norm)

kmOct2014 <- bigkmeans(m_norm_bm, centers = 12, iter.max = 10, nstart = 3, dist = "euclid")
km01 <- bigkmeans(m_norm, centers = 4, iter.max = 10, nstart = 3, dist = "euclid")

kmTable <- as.data.frame(table(kmOct2014$cluster))
colnames(kmTable) <- c("cluster", "size")
table(kmOct2014$cluster)
#      1      2      3      4      5      6      7      8      9     10     11     12 
# 130259   3498   5226    468  24200    932   2901   3049   5177  11832   4958  17686

summary(kmOct2014)
#             Length Class  Mode   
# cluster  210186 -none- numeric
# centers    5580 -none- numeric
# withinss     12 -none- numeric
# size         12 -none- numeric

unique(m_norm_bm)

# clusplot ------------------------------------------------

m_norm_bm2 <- as.matrix(m_norm_bm)
m_norm_bm2 <- as.data.frame(m_norm_bm2)
clusplot(as.numeric(m_norm_df), clus = kmOct2014$cluster)
# Error in clusplot(as.numeric(m_norm_df), clus = kmOct2014$cluster) : 
#   (list) object cannot be coerced to type 'double'

library(FactoMineR)
library(factoextra)

clusterplot <- fviz_cluster(kmOct2014, m_norm_bm, repel = TRUE)
clusterplot

plot(kmOct2014$cluster, pch = 1, col = "#00000035", cex = 1,
     main = "oct2014 listings: kmeans clusters")
plot(kmOct2014$withinss, kmOct2014$size, pch = 1, cex = 2, 
     main = "oct2014 listings: kmeans withinss vs cluster size")
plot(kmOct2014$centers, pch = 1, col = "#000000", cex = 1,
     main = "oct2014 listings: kmeans centers")
plot(kmTable$cluster, kmTable$size, xlab = "cluster",
     ylab = "size", main = "oct2014 listings: kmeans clusters v sizes")

# library fastcluster ---------------------------------------------------------

library(fastcluster)


