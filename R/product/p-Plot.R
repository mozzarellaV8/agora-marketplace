# Agora Marketplace Analysis
# Association Rules - 2014 Product Data
# Cannabis Analysis - SubCategories

# load data -------------------------------------------------------------------

library(data.table)

p14 <- fread("~/GitHub/agora-data/ag07b-2014.csv", stringsAsFactors = F)
p14$date <- as.Date(p14$date)
str(p14)

length(unique(p14$from)) # 81
length(unique(p14$product)) # 62873
summary(p14$usd)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0         29         95      15890        332 2215000000

# Densities: Price, Categories, Locations, Products ---------------------------

library(ggplot2)
library(RColorBrewer)
library(vcd)
library(extrafont)
library(extrafontdb)
font_import()

# plots -----------------------------------------------------------------------
# flawed - original - complete plot
p3 <- ggplot(p14, aes(subcat, from, fill = cat)) + geom_tile() + 
  labs(title = "Agora 2014: Category / Subcategory :: Location", 
       y = "", x = expression(subcat %subset% cat), fill = "category") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 9))

p3 + scale_fill_manual(values = c("red3", "#CDAD00", "#00688B", "#9AC0CD", 
                                  "#FF7F00", "red2", "bisque3", "gray23",
                                  "bisque2", "bisque1", "darkorange3", "darkorange4",
                                  "firebrick4"))

# remove eletronics and jewelry
p14b <- subset(p14, p14$cat != "Electronics" & p14$cat != "Jewelry") # 10000591

p3b <- ggplot(p14b, aes(subcat, from, fill = cat)) + geom_tile() + 
  labs(title = "Agora 2014: Category / Subcategory :: Location", 
       y = "", x = "", fill = "category") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 9))

p3b + scale_fill_manual(values = c("red3", 
                                  "bisque4", 
                                  "#00688B", 
                                  "#9AC0CD", 
                                  "red1", 
                                  "bisque1",
                                  "black", 
                                  "bisque3", 
                                  "darkorange2", 
                                  "darkorange4",
                                  "firebrick4"))

# plot that works -------------------------------------------------------------
# clean some location levels: 'cheqdropz', 'earth planet'
# remove listings too
p14c <- subset(p14, p14$cat != "Electronics" & p14$cat != "Jewelry"
               & p14$cat != "Listings" & p14$cat != "Other") #980979

p3c <- ggplot(p14c, aes(subcat, from, fill = cat)) + 
  geom_tile(colour = "white", size = 1) + 
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(face = "bold", size = 16,
                                  margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(size = 12.4, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 10.95)) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  labs(title = "Agora 2014: Category ~ Subcategory + Location (n = 980979)", 
       y = "", x = "", fill = "category")

p3c + scale_fill_manual(values = c("red3", 
                                   "gold1", 
                                   "deepskyblue4", 
                                   "lightblue3", 
                                   "red1", 
                                   "bisque1",
                                   "bisque4", 
                                   "darkorange4",
                                   "firebrick4"))

# subset under 1k -------------------------------------------------------------

p4 <- subset(p14, p14$usd <= 1000) # 905805

p4p <- ggplot(p4, aes(subsubcat, from, fill = usd)) + 
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(face = "bold", size = 14,
                                  margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(size = 12.5, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  labs(title = "Agora 2014: Drug + Location ~ Price (USD < 1000, n = 905805)", 
       y = "", x = "", fill = "Price (USD)")

p4p + scale_fill_gradient2(low = "deepskyblue3", 
                           mid = "bisque1",
                           high = "firebrick3",
                           midpoint = 500)

# subset over 1k  -------------------------------------------------------------

p5 <- subset(p14, p14$usd > 1000 & p14$usd < 10000.000) # 105774

p5p <- ggplot(p5, aes(subsubcat, from, fill = usd)) + 
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(face = "bold", size = 14,
                                  margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 12)) + 
  labs(title = "Agora 2014: Drug + Location ~ Price (1000 < USD < 10000; n = 105774)", 
     y = "", x = "", fill = "Price (USD)") 

p5p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 5500)

# subset over 10-40k  -------------------------------------------------------------

p6 <- subset(p14, p14$usd > 10000 & p14$usd < 40000.000) # 4722

p6p <- ggplot(p6, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora 2014: Drug ~ Location / Price (10,000 < USD < 40,000), n = 4722", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 13,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 13)) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm"))

p6p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 25000)

# subset over 40k  ----------------------------------------------------------------

p7 <- subset(p14, p14$usd > 40000 & p14$usd < 1000000) # 1416

p7p <- ggplot(p7, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora 2014: Drug ~ Location / Price (40,000 < USD < 1,000,000), n = 1416", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 20, 0))) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 13,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 13)) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm"))

p7p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 520000)

# subset over 1m  ------------------------------------------------------------------

p8 <- subset(p14, p14$usd > 1000000 & p14$usd < 10020993 & 
               p14$vendor != "DutchTruthTeller") # 187

summary(p8$usd)
range(p8$usd)

p8p <- ggplot(p8, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora 2014: Drug ~ Location / Price (USD > 1,000,000), n = 187", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 18,
                                  margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 15,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 15)) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm"))

p8p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 4960000)

