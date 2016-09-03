# Agora Marketplace
# extracting quantities of products

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(qdap)
library(data.table)
library(ggplot2)

p14 <- fread("~/GitHub/agora-data/ag06-2014.csv", stringsAsFactors = T)
str(p14)

# to double check on quanities:
prQ <- fread("~/GitHub/agora-data/02-lexical/qdapProductWF.csv")

# subset by list word/quantity ------------------------------------------------

length(unique(p14$product)) # 62873
p14$product <- as.factor(p14$product)

pTemp <- duplicated(p14$product)
p14q <- subset(p14, pTemp == F) # 62873
p14 <- p14q

# write.csv(p14q, file = "ag06-2014-unique.csv", row.names = F)

# custom orders
cust <- grepl("\\bcustom\\b", p14q$product, ignore.case = T)
customs <- subset(p14q, cust == T, ignore.case = T) # 29545 - 2080 unique
# write.csv(customs, file = "~/GitHub/agora-data/plex-customs.csv", row.names = F)

summary(customs$usd)

# product weights by id -------------------------------------------------------

# milligrams ----------------------------------------------
m <- grepl("\\bmg\\b", p14$product, ignore.case = T)
mi <- grepl("[0-9]mg\\b", p14$product, ignore.case = T)
mil <- grepl("milligram", p14$product, ignore.case = T)
mg <- subset(p14, m == T | mi == T | mil == T)
# 14583 obs by the milligram

summary(mg$usd)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#      0.0     35.7     89.4    722.0    244.5 999300.0

summary(mg)

# only listings with feedback
mg.fb <- subset(mg, mg$none == F) # 2481
mg.fbd <- subset(mg.fb, select = c(great, good, ok, poor, horrible, worst))
mg.fbds <- stack(mg.fbd)
mg.fbds$values[mg.fbds$values == T] <- 1
mg.fbds$values[mg.fbds$values == F] <- 0
colnames(mg.fbds) <- c("value", "feedback")

mg1 <- ggplot(mg.fbds, aes(value)) + xlim(0.5, 1) +
  geom_line(stat = "density", aes(colour = feedback)) +
  theme_minimal(base_size = 14, base_family = "HersheySans") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "milligram subset - feedback densities", x = "", y = "") +
  expand_limits(y = 0)
mg1

# grams ---------------------------------------------------
g <- grepl("\\bg\\b", p14$product, ignore.case = T)
gr <- grepl("[0-9]g\\b", p14$product, ignore.case = T)
gra <- grepl("\\bgram\\b", p14$product, ignore.case = T)
gram <- grepl("\\bgrams\\b", p14$product, ignore.case = T)
grams <- subset(p14, g == T | gr == T | gra == T | gram == T)
# 23973 obs by the gram.

summary(grams)

g.fb <- subset(grams, grams$none == F) # 5065
g.fbd <- subset(g.fb, select = c(great, good, ok, poor, horrible, worst))
g.fbds <- stack(g.fbd)
g.fbds$values[g.fbds$values == T] <- 1
g.fbds$values[g.fbds$values == F] <- 0
colnames(g.fbds) <- c("value", "feedback")

g1 <- ggplot(g.fbds, aes(value)) + xlim(0.5, 1) +
  geom_line(stat = "density", aes(colour = feedback)) +
  theme_minimal(base_size = 14, base_family = "HersheySans") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "grams subset - feedback densities", x = "", y = "") +
  expand_limits(y = 0)
g1

# kilograms -----------------------------------------------
k <- grepl("\\bkg\\b", p14$product, ignore.case = T)
k2 <- grepl("[0-9]kg\\b", p14$product, ignore.case = T)
ki <- grepl("\\bkilo\\b", p14$product, ignore.case = T)
kilo <- grepl("\\bkilos\\b", p14$product, ignore.case = T)
kil <- grepl("\\bkilogram\\b", p14$product, ignore.case = T)
kilos <- grepl("\\bkilograms\\b", p14$product, ignore.case = T)
kg <- subset(p14, k == T | ki == T | kil == T | kilo == T | kilos == T | k2 == T, ignore.case = T) 
# 473 obs by the kilogram

summary(kg$usd)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#     40.5   2376.0   3554.0   6988.0   6948.0 427500.0

summary(kg)
# china #1
# test kg regex
cocaine <- subset(p14, grepl("cocaine", p14$product, ignore.case = T) == T)

kg.fb <- subset(kg, kg$none == F) # 17
kg.fbd <- subset(kg.fb, select = c(great, good, ok, poor, horrible, worst))
kg.fbds <- stack(kg.fbd)
kg.fbds$values[kg.fbds$values == T] <- 1
kg.fbds$values[kg.fbds$values == F] <- 0
colnames(kg.fbds) <- c("value", "feedback")

kg1 <- ggplot(kg.fbds, aes(value)) + xlim(0.5, 1) +
  geom_line(stat = "density", aes(colour = feedback)) +
  theme_minimal(base_size = 14, base_family = "HersheySans") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "kilograms subset - feedback densities", x = "", y = "") +
  expand_limits(y = 0)
kg1

# ounces --------------------------------------------------
o <- grepl("\\boz\\b", p14$product, ignore.case = T)
ou <- grepl("\\bounce\\b", p14$product, ignore.case = T)
oun <- grepl("[0-9]oz\\b", p14$product, ignore.case = T)
oz <- subset(p14, o == T | ou == T | oun == T, ignore.case = T)
# 4259 obs by the ounce

summary(oz$usd)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#      0.01    95.53   190.80   458.20   365.60 33350.00

summary(oz)


