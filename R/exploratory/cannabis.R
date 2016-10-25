# Cannabis Class

# aora Marketplace Analysis
# Association Rules - aora Population
# Variable Preparation

# load data -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(qdap)

# population with: character counts, subcat agregated, all cats
a <- fread("~/GitHub/aora-data/aora-01b.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables
str(a)

# create categories  ----------------------------------------------------------

# combine sub- and subsub-categories
a$sc <- paste(a$subcat, a$subsubcat, sep = "-")
a$sc <- gsub("-NA$", "", a$sc)
a$sc <- gsub("Methylone", "RCs", a$sc)
a$sc <- factor(a$sc)
levels(a$sc)

a$all.c <- paste(a$cat, a$sc, sep = ": ")
a$all.c <- gsub("Drugs: Methylone", "Drugs: RCs", a$all.c)
a$all.c <- factor(a$all.c)
levels(a$all.c)

# character counts
a$p.chars <- nchar(as.character(a$product))
a$f.chars <- nchar(as.character(a$feedback))
summary(a$f.chars)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0.0    30.0    30.0   283.4   262.0  2979.0

hist(a$p.chars, breaks = 200)
hist(log(a$p.chars), breaks = 100)

fbc <- subset(a, a$f.chars > 30)
nrow(fbc)
nchar("Feedbacks: No feedbacks found.")
hist(fbc$f.chars, breaks = 200)

# word counts
a$p.words <- word_count(a$product, byrow = T, digit.remove = F)
summary(a$p.words)
hist(a$p.words) # normal, wow
hist(log(a$p.words))

a$f.words <- word_count(a$feedback, byrow = T, digit.remove = F)
summary(a$f.words)
hist(fbc$f.words, breaks = 200)
summary(fbc$f.words)
fbc <- subset(fbc, fbc$f.words > 6)

write.csv(a, file = "~/GitHub/agora-data/agora-02.csv", row.names = F)

# investigate the price bins ----------------------------
a1 <- subset(a, a$usd <= 150)
nrow(a1) # 1457401

1457401/nrow(a) # 0.6273894 63% of listings $150 or less
summary(a1$usd)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    9.97   35.43   46.78   75.87  150.00

nrow(subset(a, a$usd > 150 & a$usd <= 600))   # 515111
nrow(subset(a, a$usd > 600 & a$usd <= 2000))  # 230701
nrow(subset(a, a$usd > 2000 & a$usd <= 10000)) # 106747
nrow(subset(a, a$usd > 10000 & a$usd <= 60000)) # 10259
nrow(subset(a, a$usd > 60000)) # 2742

cor(a$usd, a$price)
cov(a$usd, a$price)
var(a$usd)
var(a$price)

onedollar <- subset(a, a$usd <= 1) # 104040
summary(onedollar)
dollar.cats <- as.data.frame(table(onedollar$all.c))


# subset cannabis -------------------------------------------------------------
cannabis <- subset(a, a$subcat == "Cannabis")
summary(cannabis)


