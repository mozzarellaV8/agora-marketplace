# Agora Marketplace
# product variable cleanse + subset

# load data -------------------------------------------------------------------
library(qdap)
library(data.table)
library(ggplot2)

p14 <- fread("~/GitHub/agora-data/ag03-2014.csv", stringsAsFactors = F)
str(p14)

# lexical product breakdown ---------------------------------------------------

# look at products
pr <- data.frame(list = p14$list, p = p14$product)

# wordfreq by all_words
prQ <- all_words(pr$p, alphabetical = T) # 15035
prQ <- prQ[order(prQ$FREQ, decreasing = T), ]
rownames(prQ) <- NULL
# removing common stopwords
prQ <- prQ[-c(5, 9, 11, 12, 14, 17, 20, 21, 25, 27, 30, 38, 34, 42, 53, 89, 90, 94, 
              56, 67, 70, 73, 76, 77, 80, 83, 85, 110, 128, 134, 136, 162, 
              180, 190, 89, 90, 94), ]

# write.csv(prQ, file = "~/GitHub/agora-data/qdapProductWF.csv", row.names = F)

par(mar = c(6, 6, 6, 6), mfrow = c(1, 2), family = "HersheySans", bty = "l")
plot(prQ$FREQ, col = "#000000", xlab = "Agora Product Listings - 2014",
     main = "distribution of words by frequency - all_words()")
plot(log(prQ$FREQ), col = "#00000025", xlab = "Agora Product Listings - 2014",
     main = "log distribution of words by frequency")

# wordfreq by freq_terms
pFT <- freq_terms(pr$p, top = 100, stopwords = Top200Words) # 100
pFT <- pFT[order(pFT$FREQ, decreasing = T), ]
rownames(pFT) <- NULL
write.csv(pFT, file = "~/GitHub/agora-data/qdapProductWF100.csv", row.names = F)

par(mar = c(6, 6, 6, 6), mfrow = c(1, 2), family = "HersheySans", bty = "l")
plot(pFT$FREQ, col = "#000000", xlab = "Agora Product Listings - 2014",
     main = "distribution of words by frequency - freq_terms, top = 100")
plot(log(pFT$FREQ), col = "#000000", xlab = "Agora Product Listings - 2014",
     main = "log distribution of words by frequency")


# word-frequency matrix
pfm <- wfm(pr$p) # 15398
cor(t(test))

qheat(ct, low = "#FFE4C4", high = "#CD2626", grid = "grey90",
      diag.na = TRUE, by.column = NULL)

# dispersion plot
with(p14, dispersion_plot(pFT$WORD, c("mdma", "kush"), 
                          grouping.var = list(p14$subcat, p14$cat)))

