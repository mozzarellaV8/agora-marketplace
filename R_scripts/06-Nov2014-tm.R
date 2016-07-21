# Agora Marketplace
# Keyword Text Mining

# Agora Marketplace
# Text Mining - November 2014 listings

# nov 2014 --------------------------------------------------------------------

library(data.table)
library(tm)
library(ggplot2)

nov2014 <- fread("~/GitHub/agora-data/data/06-nov2014.csv")

nrow(nov2014) # 503534
length(unique(nov2014$name))
# 25356 of 503534
length(unique(nov2014$description))
# 18008 of 503534

# remove some high priced outliers
# removed DutchTruthTeller - description 'dontorder'
nov2014 <- subset(nov2014, nov2014$name != "dontorder")

# remove Optiman 'ideas' and Anonymous Land in Panama
nov2014 <- subset(nov2014, nov2014$name != "anonymous land in panama")
nov2014 <- subset(nov2014, nov2014$name != "have something to sell got an idea")
nov2014 <- subset(nov2014, nov2014$name != "none")
nov2014 <- subset(nov2014, nov2014$name != "nothing of interest")
nov2014 <- subset(nov2014, nov2014$name != "noting")

# subset for under 500 BTC listings
nov2014_500 <- subset(nov2014, nov2014$btc <= 500) # 503002 obs
plot(nov2014_500$btc)
plot(nov2014_500$usd, asp = 1, ylab = "price in USD",
     main = "AgMarket Nov 2014: listings under 500 BTC")

# subset for under 500 USD listings
nov2014_500usd <- subset(nov2014, nov2014$usd <= 500) # 397398 obs
plot(nov2014_500usd$usd, asp = 1, ylab = "price in USD",
     main = "AgMarket Nov 2014: listings under 500 USD",
     ylim = c(0, 500))

par(mar = c(6, 6, 6, 6))
hist(nov2014_500usd$usd, breaks = 200, xlab = "price in USD",
     main = "AgMarket Nov 2014: listings under 500 USD")
# Looks like a Pareto distribution

# plot all list prices
hist(nov2014$usd, breaks = 200, xlab = "price in USD",
     xlim = c(0, 10000), ylim = c(0, 500000),
     main = "AgMarket Nov 2014: all listings")

binsize <- diff(range(nov2014$usd))/503360
novHist <- ggplot(nov2014, aes(usd, fill = usd)) +
  geom_histogram(binwidth = binsize)
novHist

# keyword text mining -------------------------------------

# create corpus of November listings
nov2014corpus <- Corpus(VectorSource(nov2014$name))

# preprocess
nov2014corpus <- tm_map(nov2014corpus, removePunctuation)
nov2014corpus <- tm_map(nov2014corpus, removeNumbers)
nov2014corpus <- tm_map(nov2014corpus, removeWords, 
                        stopwords("english"))
nov2014corpus <- tm_map(nov2014corpus, stripWhitespace)
nov2014corpus <- tm_map(nov2014corpus, PlainTextDocument)

# writeLines(as.character(nov2014corpus), con="data-tm/06-nov2014cor.txt", sep = ",")

# Document-Term Matrix --------------------------------------------------------

nov2014dtm <- DocumentTermMatrix(nov2014corpus)
nov2014dtm

dtms <- removeSparseTerms(nov2014dtm, 0.998)
inspect(dtms[1:10, 1:10])

nov2014dtm.m<- as.matrix(dtms)

# write out nov2014 DocumentTermMatrix
write.table(nov2014dtm.m, file = "~/GitHub/agora-data/data-dtm/06-nov2014-dtm.csv",
            sep = ",", row.names = F)

nov2014freq <- colSums(as.matrix(dtms))
nov2014ord <- order(nov2014freq, decreasing = TRUE)

head(nov2014freq[nov2014ord])
#     free     mdma    pills shipping     gram     pure 
#    39237    36435    29632    25074    24777    24182 

tail(nov2014freq[nov2014ord])
#     set  bottle  sweden version  purged    rock 
#    1040    1036    1032    1025    1024    1020

nov2014wf <- data.frame(word = names(nov2014freq[nov2014ord]),
                        frequency = nov2014freq[nov2014ord])
rownames(nov2014wf) <- NULL

# write out nov2014 wordfreq dataframe
write.table(nov2014wf, file = "~/GitHub/agora-data/data-dtm/06-nov2014-wf.csv",
            sep = ",", row.names = F)

# full listing frequency ------------------------------------------------------

nov2014listings <- as.data.frame(table(nov2014$name))
colnames(nov2014listings) <- c("listing", "frequency")

# remove blank
nov2014listings$listing[nov2014listings$listing == ""] <- NA
nov2014listings <- na.omit(nov2014listings)

# write out unsorted table
write.table(nov2014listings, file = "06-nov2014-listings.csv",
            sep = ",", row.names = FALSE)

# sort listings by frequency
nov2014listings <- nov2014listings[order(nov2014listings$frequency, decreasing = T), ]
rownames(nov2014listings) <- NULL

# wordcloud -------------------------------------------------------------------
library(wordcloud)
library(RColorBrewer)

nov2014wf <- fread("data-dtm/06-nov2014-wf.csv")
dtm <- fread("data-dtm/06-nov2014-dtm.csv")

redpal <- brewer.pal(7, "Reds")

par(family = "Arial Rounded MT Bold")
set.seed(144)
wordcloud(nov2014wf$word, nov2014wf$frequency, scale = c(4.5, 0.75),
          min.freq = 500, random.order = T, random.color = F,
          colors = redpal)

# frequency plot --------------------------------------------------------------

library(ggplot2)

wordfreq <- ggplot(subset(nov2014wf, nov2014wf$frequency > 5000), aes(x = reorder(word, frequency),
                                                                      y = frequency, 
                                                                      fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace November 2014 Listings: Term Frequencies > 5000",
       x = "", y = "frequency", fill = "")

wordfreq

wordfreq2 <- ggplot(subset(nov2014wf, nov2014wf$frequency > 2500), aes(x = reorder(word, frequency),
                                                                       y = frequency, 
                                                                       fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace November 2014 Listings: Term Frequencies > 2500",
       x = "", y = "", fill = "Frequency")

wordfreq2

wordfreq3 <- ggplot(subset(nov2014wf, nov2014wf$frequency > 1000), aes(x = reorder(word, frequency),
                                                                       y = frequency, 
                                                                       fill = frequency)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 12, base_family = "Tahoma") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  labs(title = "AgMarketplace November 2014 Listings: Term Frequencies > 1000",
       x = "", y = "frequency", fill = "Frequency")

wordfreq3

dotplot <- ggplot(subset(nov2014wf, nov2014wf$frequency > 2500), 
                  aes(x = frequency, y = reorder(word, frequency))) +
  geom_segment(aes(yend = word), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = frequency)) +
  theme_minimal(base_size = 10, base_family = "Tahoma") +
  theme(panel.grid.major.y = element_blank()) +
  theme(plot.margin = unit(c(2, 1, 0.5, 1), "cm")) +
  theme(axis.text.x = element_text(size = 11, angle = 0)) +
  theme(axis.text.y = element_text(size = 9, angle = 0)) +
  labs(title = "AgMarketplace November 2014 Listings: Term Frequencies > 2500",
       x = "", y = "", fill = "Frequency")

dotplot + scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  scale_x_continuous(limits = c(0, 25000))

# Kmeans clustering -----------------------------------------------------------

library(fpc)
library(cluster)
library(NbClust)

# findAssocs(nov2014dtm, "mdma", .8)
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
write.table(m_norm_df, file = "data-dtm/m_norm_df-nov2014.csv", sep = ",", row.names = FALSE)

m_norm_bm <- as.big.matrix(m_norm)

kmnov2014 <- bigkmeans(m_norm_bm, centers = 12, iter.max = 10, nstart = 3, dist = "euclid")
km01 <- bigkmeans(m_norm, centers = 4, iter.max = 10, nstart = 3, dist = "euclid")

kmTable <- as.data.frame(table(kmnov2014$cluster))
colnames(kmTable) <- c("cluster", "size")
table(kmnov2014$cluster)
#      1      2      3      4      5      6      7      8      9     10     11     12 
# 130259   3498   5226    468  24200    932   2901   3049   5177  11832   4958  17686

summary(kmnov2014)
#             Length Class  Mode   
# cluster  210186 -none- numeric
# centers    5580 -none- numeric
# withinss     12 -none- numeric
# size         12 -none- numeric

unique(m_norm_bm)

# clusplot ------------------------------------------------

m_norm_bm2 <- as.matrix(m_norm_bm)
m_norm_bm2 <- as.data.frame(m_norm_bm2)
clusplot(as.numeric(m_norm_df), clus = kmnov2014$cluster)
# Error in clusplot(as.numeric(m_norm_df), clus = kmnov2014$cluster) : 
#   (list) object cannot be coerced to type 'double'

library(FactoMineR)
library(factoextra)

clusterplot <- fviz_cluster(kmnov2014, m_norm_bm, repel = TRUE)
clusterplot

plot(kmnov2014$cluster, pch = 1, col = "#00000035", cex = 1,
     main = "nov2014 listings: kmeans clusters")
plot(kmnov2014$withinss, kmnov2014$size, pch = 1, cex = 2, 
     main = "nov2014 listings: kmeans withinss vs cluster size")
plot(kmnov2014$centers, pch = 1, col = "#000000", cex = 1,
     main = "nov2014 listings: kmeans centers")
plot(kmTable$cluster, kmTable$size, xlab = "cluster",
     ylab = "size", main = "nov2014 listings: kmeans clusters v sizes")

# library fastcluster ---------------------------------------------------------

library(fastcluster)

d <- dist(m_norm, method = "euclidean")
hclust.vector(X, method="single", members=NULL, metric='euclidean', p=NULL)

