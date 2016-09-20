# Agora Marketplace Analysis
# Categorical Plots

# load data -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(RColorBrewer)
library(extrafont)
library(extrafontdb)
# font_import()

# agora-00.csv = raw data
# agora-01b.csv = cleansed locations, 2322961 obs

agora <- fread("~/GitHub/agora-data/agora-01b.csv")
agora$date <- as.Date(agora$date)

length(unique(agora$from)) # 90
length(unique(agora$product)) # 107302
summary(agora$usd)
# Min.  1st Qu.    Median       Mean     3rd Qu.         Max. 
# 0         24         85       7827        294    2215000000

# Price, Categories, Locations, Products --------------------------------------

# collapse some levels:
# consider Worldwide+Undeclared, Agora+Internet+Torland

levels(as.factor(agora$from))
agora$from <- as.character(agora$from)
agora$from[is.na(agora$from)] <- "No Info"
agora$from <- gsub("\\bBangkok\\b", "Thailand", agora$from)
agora$from <- gsub("\\bEurope\\b", "EU", agora$from)
agora$from <- gsub("^England(.*)", "UK", agora$from)
agora$from <- gsub("\\bGerman\\b", "Germany", agora$from)

agora$from <- gsub("^Candy(.*)", "No Info", agora$from)
agora$from <- gsub("^Earth(.*)", "Worldwide", agora$from)
agora$from <- gsub("Agora//Internet/Torland", "Agora/Internet/Torland", agora$from)

agora$from <- gsub("\\bAgora\\b", "Agora/Internet/Torland", agora$from)
agora$from <- gsub("^Torland(.*)", "Agora/Internet/Torland", agora$from)
agora$from <- gsub("^Internet(.*)", "Agora/Internet/Torland", agora$from)
agora$from <- gsub("^bluerave(.*)", "Agora/Internet/Torland", agora$from)

levels(as.factor(agora$from))           # 85
agora$from <- as.factor(agora$from)

agora$subsubcat <- as.character(agora$subsubcat)
agora$subsubcat[is.na(agora$subsubcat)] <- "No Info / Other"
agora$subsubcat <- gsub("^No\\sInfo(.*)", "No Info/Other", agora$subsubcat)
agora$subsubcat <- gsub("\\bNB\\b", "NBOMe", agora$subsubcat)
levels(as.factor(agora$subsubcat))      # 43
agora$subsubcat <- as.factor(agora$subsubcat)

levels(as.factor(agora$subcat))
agora$subcat <- as.character(agora$subcat)
agora$subcat[is.na(agora$subcat)] <- "No Info/Other"
agora$subcat <- gsub("^No\\sInfo\\s(.*)", "No Info/Other", agora$subcat)
agora$subcat <- gsub("^Drugs(.*)", "Drugs: Books/Info", agora$subcat)
agora$subcat <- gsub("\\bEcstasy-MDMA", "Ecstasy", agora$subcat)
agora$subcat <- gsub("\\bDrugs:\\sBooks\\/Info", "eBooks", agora$subcat)
levels(as.factor(agora$subcat))         # 58
agora$subcat <- as.factor(agora$subcat)

write.csv(agora, file = "~/GitHub/agora-data/agora-01b.csv", row.names = F)

# chem <- subset(agora, agora$cat == "Chemicals")
# summary(chem)
# product   
# 1 Free Deepweb Website- Ask what field you want it from!                                    : 13  
# 27 000 + Commercial - Chemical -GREAT POTENTIAL - related  TARGETED emails                  : 13  
# ACETIC ANHYDRIDE ~~liquid~~  ```chemical``` ```precursor```  ```reagent```  Acetic Anhydride: 13  
# 1000mL DMF or DMSO (DMSO for testosterone transdermal absorption)                           : 12  
# 1000mL hexane, ethyl acetate, methanol                                                      : 12  
# 1g 10% Palladium on Carbon Charcoal Pd/C                                                    : 12  
# (Other)                                     

# candy <- subset(agora, agora$from == "Candyland")

# plots -----------------------------------------------------------------------
agora_b <- fread("~/GitHub/agora-data/agora_b.csv")

# No subcategories: Chemicals, Electronics, Jewelry, Listings, Other
agora_b <- subset(agora, agora$cat != "Electronics" & agora$cat != "Jewelry"
                  & agora$cat != "Listings" & agora$cat != "Other"
                  & agora$cat != "Chemicals")

nrow(agora_b) # 2237562

agora_b$subcat <- gsub("^Methylone", "RCs", agora_b$subcat)
agora_b$subcat <- gsub("^Other(.*)", "No Info/Other", agora_b$subcat)
agora_b$subcat <- as.factor(agora_b$subcat)

agora_b$subsubcat <- gsub("^Other(.*)", "No Info/Other", agora_b$subsubcat)

agora_b$from <- as.factor(agora_b$from)
agora_b$cat <- as.factor(agora_b$cat)
agora_b$subcat <- as.factor(agora_b$subcat)
agora_b$subsubcat <- as.factor(agora_b$subsubcat)

levels(as.factor(agora_b$from))         # 84
levels(as.factor(agora_b$cat))          # 10
levels(as.factor(agora_b$subcat))       # 54
levels(as.factor(agora_b$subsubcat))    # 42

write.csv(agora_b, file = "~/GitHub/agora-data/agora_b.csv", row.names = F)

# write(levels(as.factor(agora_b$from)), file = "from.txt")
# write(levels(as.factor(agora_b$subcat)), file = "sc.txt")
# write(levels(as.factor(agora_b$subsubcat)), file = "ssc.txt")
# write(levels(as.factor(agora_b$cat)), file = "cat.txt")

# font size for web
a1c <- ggplot(agora_b, aes(from, subcat, fill = cat)) +
  geom_tile(colour = "white", size = 1) + 
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(size = 11.5, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 12.5, lineheight = 1)) +
  theme(plot.margin = unit(c(0, 0.25, 0, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Category ~ Subcategory + Location (n = 2237562)",
       y = "", x = "", fill = "category")

a1c + scale_fill_manual(values = c("red3", "gold1", "deepskyblue4", "lightblue3", 
                                   "red1", "bisque1", "bisque3", "bisque4", 
                                   "darkorange3", "firebrick4"))

# a1c2 print size  ----------------------------------------
a1c2 <- ggplot(agora_b, aes(x = from, y = subcat, fill = cat)) + 
  geom_tile(colour = "white", size = 1) + 
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.title = element_text(size = 6, margin = margin(0, -100, 20, 100))) + 
  theme(axis.text.x = element_text(size = 11.5, angle = 45, 
                                   hjust = 1, lineheight = 1.1)) +
  theme(axis.text.y = element_text(size = 12, lineheight = 1)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Category ~ Subcategory + Location (n = 2237562)",
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

a2 <- subset(agora_b, agora_b$usd <= 1000) # 2011240
nrow(a2)

levels(as.factor(a2$from))      # 72
a2$from <- as.factor(a2$from)

levels(as.factor(a2$cat))       # 10
a2$cat <- as.factor(a2$cat)

levels(as.factor(a2$subcat))    # 54
levels(as.factor(a2$subsubcat)) # 43

a2$subsubcat <- as.character(a2$subsubcat)
a2$subsubcat[is.na(a2$subsubcat)] <- "No Info/Other"
a2$subsubcat <- gsub("^No\\sInfo(.*)", "No Info/Other", a2$subsubcat)
a2$subsubcat <- as.factor(a2$subsubcat)

wh1k <- levels(as.factor(a2$from))
write(wh1k, file = "~/GitHub/agora-data/info/under1kfrom.txt")

# class(a2)
# a2 <- as.data.frame(a2)
# convert to factors first.
# write.csv(a2, file = "~/GitHub/agora-data/a2-1k.csv", row.names = F)
# a2 <- fread("~/GitHub/agora-data/a2-1k.csv", stringsAsFactors = T)

# print version
a2p <- ggplot(a2, aes(from, subsubcat, fill = usd)) +
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = .5)) +
  theme(axis.text.y = element_text(size = 12.5, angle = 45, vjust = 0)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Drug * Location ~ Price (USD < 1000, n = 2011240)", 
       y = "", x = "", fill = "Price (USD)")

a2p + scale_fill_gradient2(low = "deepskyblue3", 
                           mid = "antiquewhite1",
                           high = "firebrick3",
                           midpoint = 500)

# a2-2 - websized font - 1200px wide
a2p2 <- ggplot(a2, aes(from, subsubcat, fill = usd)) +
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(axis.text.x = element_text(size = 12.25, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 13.25)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Drug * Location ~ Price (USD < 1000, n = 2011240)", 
       y = "", x = "", fill = "Price (USD)")

a2p2 + scale_fill_gradient2(low = "deepskyblue3", 
                            mid = "bisque1",
                            high = "firebrick3",
                            midpoint = 500)

plot.new()
frame()

# subset over 1k  -------------------------------------------------------------
# agora_b <- fread("~/GitHub/agora-data/agora_b.csv")

a3 <- subset(agora_b, agora_b$usd > 1000 & agora_b$usd < 10000.00) # 213680
nrow(a3)
levels(as.factor(a3$from))      # 52
levels(as.factor(a3$subsubcat)) # 42

where1k <- levels(as.factor(a3$from))
write(where1k, file = "where1k.txt")

a3$subsubcat <- as.character(a3$subsubcat)
a3$subsubcat[is.na(a3$subsubcat)] <- "No Info/Other"
a3$subsubcat <- as.factor(a3$subsubcat)  

# web version
a3p <- ggplot(a3, aes(from, subsubcat, fill = usd)) + 
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.title = element_text(size = 8, margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12.75, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 13.5)) + 
  labs(title = "Agora Marketplace: Drug * Location ~ Price (1000 < USD < 10000; n = 213680)", 
       y = "", x = "", fill = "Price (USD)") 

a3p + scale_fill_gradient2(low = "deepskyblue4", 
                           mid = "antiquewhite1", 
                           high = "firebrick3",
                           midpoint = 5500)

# print version
a3p2 <- ggplot(a3, aes(from, subsubcat, fill = usd)) + 
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12.75, angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 12.75, angle = 45)) + 
  labs(title = "Agora Marketplace: Drug * Location ~ Price (1000 < USD < 10000; n = 213680)", 
       y = "", x = "", fill = "Price (USD)") 

a3p2 + scale_fill_gradient2(low = "deepskyblue4", 
                            mid = "antiquewhite1", 
                            high = "firebrick3",
                            midpoint = 5500)

# subset over 10-40k  -------------------------------------------------------------

a4 <- subset(agora_b, agora_b$usd > 10000 & agora_b$usd < 100000) # 10683
nrow(a4)

a4$subsubcat <- as.character(a4$subsubcat)
a4$subsubcat[is.na(a4$subsubcat)] <- "No Info/Other"

a4$subsubcat <- factor(a4$subsubcat)
a4$from <- factor(a4$from)

levels(as.factor(a4$from)) # 25
levels(as.factor(a4$subsubcat)) # 29

summary(a4$usd)

# print
a4p <- ggplot(a4, aes(subsubcat, from, fill = usd)) + 
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12.75, angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 12.75, angle = 45)) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Drug * Location ~ Price (10,000 < USD < 100,000), n = 10683", 
       y = "", x = "", fill = "Price (USD)")

a4p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 55000)

# web
a4p2 <- ggplot(a4, aes(subsubcat, from, fill = usd)) + 
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.title = element_text(size = 8, margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12.75, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 13.5)) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Drug * Location ~ Price (10,000 < USD < 100,000), n = 10683", 
       y = "", x = "", fill = "Price (USD)")

a4p2 + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", 
                            high = "firebrick3", midpoint = 55000)

# subset over 40k  ----------------------------------------------------------------

a5 <- subset(agora_b, agora_b$usd > 100000 & agora_b$usd < 1000000) # 1212

a5$subsubcat <- as.character(a5$subsubcat)
a5$subsubcat[is.na(a5$subsubcat)] <- "No Info/Other"
a5$subsubcat <- factor(a5$subsubcat)
a5$from <- factor(a5$from)

levels(as.factor(a5$from)) # 12
levels(as.factor(a5$subsubcat)) # 15

# print
a5p <- ggplot(a5, aes(subsubcat, from, fill = usd)) +
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.title = element_text(size = 10, margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12.75, angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.text.y = element_text(size = 12.75, angle = 45)) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  labs(title = "Agora Marketplace: Drug * Location ~ Price (100,000 < USD < 1,000,000), n = 1212", 
       y = "", x = "", fill = "Price (USD)")

a5p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", 
                           high = "firebrick3", midpoint = 520000)

# web
a5p2 <- ggplot(a5, aes(subsubcat, from, fill = usd)) +
  geom_tile(colour = "white", size = 1.75) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.title = element_text(size = 8, margin = margin(0, 0, 20, 0))) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12.75, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 13.5)) + 
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Agora Marketplace: Drug * Location ~ Price (100,000 < USD < 1,000,000), n = 1212", 
       y = "", x = "", fill = "Price (USD)")

a5p2 + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", 
                           high = "firebrick3", midpoint = 520000)
