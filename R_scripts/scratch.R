# text mining descriptions ----------------------------------------------------
library(tm)

length(unique(agora$name))
# 85448 out of 4371382 listings
length(unique(agora$description))
# 67141 out of 4371382

unique(name)
unique(description)

product_corpus <- Corpus(VectorSource(agora$name))

# writeCorpus(product_corpus, path = "corpus",
#            filenames = paste(seq_along(product_corpus), ".txt", sep = ""))


## f_viz Oct 2014 cluster

scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_color_brewer(palette = "YlOrRd", direction = -1) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "K-Means - Cluster Plot")
