# Agora Marketplace Analysis
# January 2014 arules 
# cluster to determine categories
# use categories to set up arules data

# load data -------------------------------------------------------------------

library(data.table)
library(tm)
library(ggplot2)

v0114 <- read.csv("data/vendor/vendorall_0114.csv")
str(v0114)
length(unique(v0114$name))
# 3256 of 8533 listings are unique


head(v0114$name)
v0114$name[1]


# # keyword text mining -------------------------------------------------------

# create corpus of November listings
v0114corpus <- Corpus(VectorSource(v0114$name))

# preprocess
v0114corpus <- tm_map(v0114corpus, removePunctuation)
v0114corpus <- tm_map(v0114corpus, removeNumbers)
v0114corpus <- tm_map(v0114corpus, content_transformer(tolower))

# add stopwords: accepted, agora, this, quality, please, listing, the
v0114corpus <- tm_map(v0114corpus, removeWords, 
                        stopwords("english"))

v0114corpus <- tm_map(v0114corpus, stripWhitespace)
v0114corpus <- tm_map(v0114corpus, PlainTextDocument)

# Document-Term Matrix --------------------------------------------------------

v0114dtm <- DocumentTermMatrix(v0114corpus)
inspect(v0114dtm[1:10, 1:10])
v0114dtm

dtms <- removeSparseTerms(v0114dtm, 0.998)
inspect(dtms[1:10, 1:10])

v0114dtm.m<- as.matrix(dtms)

v0114freq <- colSums(as.matrix(dtms))
v0114ord <- order(v0114freq, decreasing = TRUE)

head(v0114freq[v0114ord])
# quality  please listing    mdma    high     lsd 
# 1493    1475    1448    1252    1223    1009
tail(v0114freq[v0114ord])
#     wenn      wir required     mind      end     sell 
#       90       90       89       88       86       86

v0114wf <- data.frame(word = names(v0114freq[v0114ord]),
                        frequency = v0114freq[v0114ord])
rownames(v0114wf) <- NULL

# write out v0114 wordfreq dataframe
# review this for more stopwords
write.table(v0114wf, file = "~/GitHub/agora-marketplace/data/derived/01-v0114-wf.csv",
            sep = ",", row.names = F)

v0114tdm <- TermDocumentMatrix(v0114corpus)
tdms <- removeSparseTerms(v0114tdm, 0.998)
inspect(tdms[1:10, 1:10])
inspect(tdms[100:120, 100:120])

# Kmeans clustering -----------------------------------------------------------

# euclidean normalization -----------------------------------------------------
install.packages(c("fpc", "NbClust"))
library(fpc)
library(cluster)
library(NbClust)

# this is chill
mdmaAssocs <- as.data.frame(findAssocs(dtms, "mdma", 0.15))
mdmaAssocs$terms <- rownames(mdmaAssocs)
rownames(mdmaAssocs) <- NULL

dtm_tfidf <- weightTfIdf(dtms)

m <- as.matrix(dtms)
row.names(m) <- 1:nrow(m)

# kmeans normalizing
norm_eucl <- function(m) m/ apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
m_norm <- na.omit(m_norm)

m_norm_df <- as.data.frame(m_norm)
write.table(m_norm_df, file = "data/derived/m_norm_v0114.csv", sep = ",", row.names = FALSE)

# wssplot  ------------------------------------------------
# determine number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",
       main = "Total WSS vs. Number of Clusters (15)")
}


par(mfrow = c(1, 1), mar = c(6, 6, 6, 6))
wssplot(m_norm_df)

# NbClust -------------------------------------------------
# doesnt work - tighten up sparsity, too many terms

set.seed(144)
nc <- NbClust(m_norm, min.nc = 2, max.nc = 80, method = "kmeans")

barplot(table(nc$Best.n[1,]),
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "Number of Clusters Chosen by 26 Criteria")

# K-means fit -----------------------------------------------------------------

v0114.km <- kmeans(m_norm_df, centers = 4, iter.max = 10, nstart = 4)
clustTab <- table(v0114.km$cluster)
clustTab
#     1    2    3    4 
#   568  306 1176 6453

install.packages(c("factoextra", "RColorBrewer"))
library(factoextra)
library(RColorBrewer)

clusterplot <- fviz_cluster(v0114.km, m_norm_df, repel = TRUE) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_color_brewer(palette = "YlOrRd", direction = -1) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "K-Means - Cluster Plot")

clusterplot

clusplot(m_norm_df, clus = v0114.km$cluster)







