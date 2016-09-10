# Agora Marketplace Analysis
# 2014 data binding

# read and bind data ----------------------------------------------------------
pDir <- "~/GitHub/ag-p/p2014"
pList <- list.files(path = pDir, pattern = ".csv", all.files = T,
                           full.names = T, recursive = T)

p2014 <- data.frame()
system.time(
  for (i in 1:length(pList)) {
    temp <- read.csv(pList[i], stringsAsFactors = F)
    p2014 <- rbind(p2014, temp)
  }
)

#    user  system elapsed
#  60.263   2.891  64.025

# safety
write.csv(p2014, file = "~/GitHub/agora-data/ag01-2014.csv", row.names = F)

# cleanse ----------------------------------------------------------------------

library(data.table)
library(tidyr)
library(dplyr)
library(tm)

p14 <- fread("~/GitHub/agora-data/ag01-2014.csv")
str(p14)
summary(p14) # 1018109 obs. of  11 variables

p14$feedback <- stripWhitespace(p14$feedback)

p14$vendor <- gsub("%7E", "", p14$vendor) # tilde in vendorname
levels(as.factor(p14$vendor))

write.csv(p14, file = "~/GitHub/agora-data/ag01-2014.csv", row.names = F)

# category summaries --------------------------------------
which(is.na(p14$cat))
summary(is.na(p14$subcat)) # TRUE: 39236
#    Mode   FALSE    TRUE    NA's 
# logical  978873   39236       0

39236/1018109   # 0.03853811 no subcategory
978873/1018109  # 0.9614619 have subcategory

summary(is.na(p14$subsubcat))
#    Mode   FALSE    TRUE    NA's 
# logical  499830  518279       0 

518279/1018109 # 0.5090604 no sub-subcategory
499830/1018109 # 0.4909396 have sub-subcategory

# products ----------------------------
# it's own script later~
summary(as.factor(p14$product))

product <- as.data.frame(table(as.factor(p14$product)))
colnames(product) <- c("p", "freq")

product$p <- as.character(product$p)
product$p <- stripWhitespace(product$p)

pS <- unlist(product$p)
pS <- removeWords(pS, stopwords("en"))
pS <- removeWords(pS, stopwords("SMART"))

product$pS <- pS
product$pS <- removePunctuation(product$pS)
product$pS <- tolower(product$pS)

AgStopWords <- c("sample", "special","master","high", "quality","grade", "good","beautiful","FE",
                           "Listing", "Pure","HANDMADE","Plus","ultra","page","pussy","riot","social","amazing",
                           "your", "saint", "lucia", "deal", "strength", "real", "deal", "potent", "shipping",
                           "sugar","crumble","bundle","NL","Belgium","no","minimum","branded","generic",
                           "legit","lab","HQ","new", "seller","sale","tracked","full","escrow","fee","our",
                           "free","strong","intro","version","full escrow version","full escrow","fresh",
                           "best","top","shelf","gluten")

AgStopWords <- tolower(AgStopWords)
product$pS <- removeWords(pS, stopwords(AgStopWords))

write.csv(product, file = "~/GitHub/agora-data/ag01-productTest.csv", row.names = F)


