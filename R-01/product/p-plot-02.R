# Agora Marketplace Analysis
# 2014 Product Data
# Categories

# load data -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(extrafontdb)
# font_import()

# agora-00.csv = raw data
# agora-01.csv = cleansed locations, 1812775
# agora-b-01.csv = categories removed for plotting

agora <- fread("~/GitHub/agora-data/agora-01.csv")
agora$date <- as.Date(agora$date)

length(unique(agora$from)) # 84
length(unique(agora$product)) # 86740
summary(agora$usd)
# Min.  1st Qu.    Median       Mean     3rd Qu.         Max. 
# 0         27         90       9628        310   2215000000

agora_b <- fread("~/GitHub/agora-data/agora-b-01.csv")

# Price, Categories, Locations, Products --------------------------------------

# collapse some levels:
# consider Worldwide+Undeclared, Agora+Internet+Torland

levels(as.factor(agora$from))
agora$from <- as.character(agora$from)
agora$from[is.na(agora$from)] <- "No Info"
agora$from <- gsub("\\bBangkok\\b", "Thailand", agora$from)
agora$from <- gsub("\\bEurope\\b", "EU", agora$from)
agora$from <- gsub("\\bAgora\\b", "Agora/Internet/Torland", agora$from)
agora$from <- gsub("^Torland(.*)", "Agora/Internet/Torland", agora$from)
agora$from <- gsub("^Internet(.*)", "Agora/Internet/Torland", agora$from)
agora$from <- as.factor(agora$from)

agora$subsubcat <- as.character(agora$subsubcat)
agora$subsubcat[is.na(agora$subsubcat)] <- "No Info / Other"
agora$subsubcat <- gsub("^No\\sInfo(.*)", "No Info/Other", agora$subsubcat)
agora$subsubcat <- gsub("\\bNB\\b", "NBOMe", agora$subsubcat)
levels(as.factor(agora$subsubcat)) # 43
agora$subsubcat <- as.factor(agora$subsubcat)

levels(as.factor(agora$subcat))
agora$subcat <- as.character(agora$subcat)
agora$subcat[is.na(agora$subcat)] <- "No Info/Other"
agora$subcat <- gsub("^No\\sInfo\\s(.*)", "No Info/Other", agora$subcat)
agora$subcat <- gsub("^Drugs(.*)", "Drugs-Books/Info", agora$subcat)
levels(as.factor(agora$subcat))

# write.csv(agora, file = "~/GitHub/agora-data/agora-00.csv", row.names = F)

# chem <- subset(agora, agora$cat == "Chemicals")
# summary(chem)
# product   
# 1 Free Deepweb Website- Ask what field you want it from!                                    : 13  
# 27 000 + Commercial - Chemical -GREAT POTENTIAL - related  TARGETED emails                  : 13  
# ACETIC ANHYDRIDE ~~liquid~~  ```chemical``` ```precursor```  ```reagent```  Acetic Anhydride: 13  
# 1000mL DMF or DMSO (DMSO for testosterone transdermal absorption)                           : 12  
# 1000mL hexane, ethyl acetate, methanol                                                      : 12  
# 1g 10% Palladium on Carbon Charcoal Pd/C                                                    : 12  
# (Other)                                                                                     :177

# categorical tables ----------------------------------------------------------

# category table
c15 <- as.data.frame(table(p15$cat))
colnames(c15) <- c("category", "freq")
c15 <- c15[order(c15$freq, decreasing = T), ]
rownames(c15) <- NULL

# subcategory table
summary(p15$subcat)
sc15 <- as.data.frame(table(p15$subcat))
colnames(sc15) <- c("subcategory", "freq")
sc15 <- sc15[order(sc15$freq, decreasing = T), ]
rownames(sc15) <- NULL

# sub-subcategory (drug) table
summary(p15$subsubcat)
ssc15 <- as.data.frame(table(p15$subcat))
colnames(ssc15) <- c("drug", "freq")
ssc15 <- ssc15[order(ssc15$freq, decreasing = T), ]
rownames(ssc15) <- NULL


# plots -----------------------------------------------------------------------

# No subcategories: Chemicals, Electronics, Jewelry, Listings, Other
agora_b <- subset(agora, agora$cat != "Electronics" & agora$cat != "Jewelry"
               & agora$cat != "Listings" & agora$cat != "Other"
               & agora$cat != "Chemicals")

nrow(agora_b) # 1749748

# write.csv(agora_b, file = "~/GitHub/agora-data/agora-b-01.csv", row.names = F)
levels(as.factor(agora_b$from))
levels(as.factor(agora_b$cat))
levels(as.factor(agora_b$subcat))
levels(as.factor(agora_b$subsubcat))

from <- levels(as.factor(agora_b$from))
cat <- levels(as.factor(agora_b$cat))
sc <- levels(as.factor(agora_b$subcat))
ssc <- levels(as.factor(agora_b$subsubcat))
write(from, file = "from.txt")
write(sc, file = "sc.txt")
write(ssc, file = "ssc.txt")
write(cat, file = "cat.txt")

# font size for web
a1c <- ggplot(agora_b, aes(subcat, from, fill = cat)) + 
  geom_tile(colour = "white", size = 1) + 
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(size = 14.5, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 12.5, lineheight = 1.2)) +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Category ~ Subcategory + Location (n = 1749748)",
       y = "", x = "", fill = "category")

a1c + scale_fill_manual(values = c("red3", 
                                   "gold1", 
                                   "deepskyblue4", 
                                   "lightblue3", 
                                   "red1", 
                                   "bisque1",
                                   "bisque3",
                                   "bisque4", 
                                   "darkorange3",
                                   "firebrick4"))

# a1c2 print size  -----------------------------
a1c2 <- ggplot(agora_b, aes(x = from, y = subcat, fill = cat)) + 
  geom_tile(colour = "white", size = 1) + 
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(size = 6, margin = margin(0, -100, 20, 100))) + 
  theme(axis.text.x = element_text(size = 12, angle = 45, 
                                   hjust = 1, lineheight = 1.1)) +
  theme(axis.text.y = element_text(size = 11, lineheight = 1.1)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Category ~ Subcategory + Location (n = 1749748)",
       y = "", x = "", fill = "category")

a1c2 + scale_fill_manual(values = c("red3", 
                                   "gold1", 
                                   "deepskyblue4", 
                                   "lightblue3", 
                                   "red1", 
                                   "bisque1",
                                   "bisque3",
                                   "bisque4", 
                                   "darkorange3",
                                   "firebrick4"))

plot.new()
frame()

# a2 - subset under 1k --------------------------------------------------------

a2 <- subset(agora_b, agora_b$usd <= 1000) # 1562498

levels(as.factor(a2$from))      # 72
a2$from <- as.factor(a2$from)

levels(as.factor(a2$cat))
a2$cat <- as.factor(a2$cat)

levels(as.factor(a2$subsubcat)) # 43
a2$subsubcat <- as.factor(a2$subsubcat)

wh1k <- levels(as.factor(a2$from))
write(wh1k, file = "under1kfrom.txt")

nrow(a2) # 1562498

# class(a2)
# a2 <- as.data.frame(a2)
# convert to factors first.
# write.csv(a2, file = "~/GitHub/agora-data/a2-1k.csv", row.names = F)
# a2 <- fread("~/GitHub/agora-data/a2-1k.csv", stringsAsFactors = T)

# print version
a2p <- ggplot(a2, aes(from, subsubcat, fill = usd)) +
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5)) +
  theme(axis.text.y = element_text(size = 12.5, angle = 45, vjust = 0)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Drug + Location ~ Price (USD < 1000, n = 1562498)", 
       y = "", x = "", fill = "Price (USD)")

a2p + scale_fill_gradient2(low = "deepskyblue3", 
                           mid = "antiquewhite1",
                           high = "firebrick3",
                           midpoint = 500)

# a2-2 - websized font - 1200px wide
a2p2 <- ggplot(a2, aes(subsubcat, from, fill = usd)) +
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(size = 13.75, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 12.35)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Drug + Location ~ Price (USD < 1000, n = 1562498)", 
       y = "", x = "", fill = "Price (USD)")

a2p2 + scale_fill_gradient2(low = "deepskyblue3", 
                           mid = "bisque1",
                           high = "firebrick3",
                           midpoint = 500)

# subset over 1k  -------------------------------------------------------------

a3 <- subset(agora_b, agora_b$usd > 1000 & agora_b$usd < 10000.000) # 176816
levels(as.factor(a3$from))      # 51
levels(as.factor(a3$subsubcat)) # 39

where1k <- levels(as.factor(a3$from))
write(where1k, file = "where1k.txt")

# web version
a3p <- ggplot(a3, aes(from, subsubcat, fill = usd)) + 
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 12, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(size = 8, margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12.75, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 14)) + 
  labs(title = "Agora Marketplace: Drug + Location ~ Price (1000 < USD < 10000; n = 176816)", 
       y = "", x = "", fill = "Price (USD)") 

a3p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 5500)

# print version
a3p2 <- ggplot(a3, aes(from, subsubcat, fill = usd)) + 
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12.75, angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 12.75, angle = 45)) + 
  labs(title = "Agora Marketplace: Drug + Location ~ Price (1000 < USD < 10000; n = 176816)", 
       y = "", x = "", fill = "Price (USD)") 

a3p2 + scale_fill_gradient2(low = "deepskyblue4", 
                            mid = "antiquewhite1", 
                            high = "firebrick3",
                            midpoint = 5500)

# print V2
a3p2b <- ggplot(a3, aes(subsubcat, from, fill = usd)) + 
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12.75, angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(size = 12.75, angle = 45)) + 
  labs(title = "Agora Marketplace: Drug + Location ~ Price (1000 < USD < 10000; n = 176816)", 
       y = "", x = "", fill = "Price (USD)") 

a3p2b + scale_fill_gradient2(low = "deepskyblue4", 
                            mid = "antiquewhite1", 
                            high = "firebrick3",
                            midpoint = 5500)

# subset over 10-40k  -------------------------------------------------------------

a4 <- subset(agora, agora$usd > 10000 & agora$usd < 40000.000) # 4722

a4p <- ggplot(a4, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora Marketplace: Drug ~ Location / Price (10,000 < USD < 40,000), n = 4722", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 13,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 13)) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm"))

a4p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 25000)

# subset over 40k  ----------------------------------------------------------------

a5 <- subset(agora, agora$usd > 40000 & agora$usd < 1000000) # 1416

a5p <- ggplot(a5, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora Marketplace: Drug ~ Location / Price (40,000 < USD < 1,000,000), n = 1416", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 20, 0))) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 13,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 13)) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm"))

a5p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 520000)

# subset over 1m  ------------------------------------------------------------------

a6 <- subset(agora, agora$usd > 1000000 & agora$usd < 10020993 & 
               agora$vendor != "DutchTruthTeller") # 187

summary(a6$usd)
range(a6$usd)

a6p <- ggplot(a6, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora Marketplace: Drug ~ Location / Price (USD > 1,000,000), n = 187", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 18,
                                  margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 15,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 15)) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm"))

a6p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 4960000)

