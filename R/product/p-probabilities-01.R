# Agora Marketplace Analysis
# Class/Category Probabilities

# load data -------------------------------------------------------------------

library(data.table)

p14 <- fread("~/GitHub/agora-data/ag07-2014.csv", stringsAsFactors = T)
p14$date <- as.Date(p14$date)

p14$subcat <- as.character(p14$subcat)
p14$subcat[p14$subcat == "Methylone"] <- "RCs"
p14$subcat[p14$subcat == "Disassociatives"] <- "Dissociatives"
p14$subcat[p14$subcat == "NoInfo/Other"] <- "No Info / Other"
p14$subcat <- as.factor(p14$subcat)

p14$subsubcat <- as.character(p14$subsubcat)
which(p14$subsubcat == "Ecstasy")
p14$subsubcat[p14$subsubcat == "Ecstasy"] <- "MDMA"
p14$subsubcat[p14$subsubcat == "NoInfo/Other"] <- "No Info / Other"
p14$subsubcat <- as.factor(p14$subsubcat)

write.csv(p14, file = "~/GitHub/agora-data/ag07-2014.csv", row.names = F)

# class distributions ---------------------------------------------------------

# 1018109 observations total

levels(p14$cat)       # 13 categories
levels(as.factor(p14$subcat))    # 42 subcategories
levels(as.factor(p14$subsubcat)) # 42 subsubcategories (all under drugs)

# probabilities of category, subcategory, ssc
100*(1/13) # 7.69% - 0.07692308
100*(1/42) # 2.38% - 0.02380952
100*(1/42) # 2.38% - 0.02380952

# theoretical probability of cannabis
100*(1/13)*(1/43)

# relative frequencies of each category
# category
cats <- as.data.frame(table(p14$cat))
cats$RelFreq <- cats$Freq/nrow(p14)
colnames(cats)[1] <- "Category"

# subcategory
subcats <- as.data.frame(table(p14$subcat))
subcats$RelFreq <- subcats$Freq/nrow(p14)
colnames(subcats)[1] <- "SubCategory"

# sub-subcategory (drugs)
drugs <- as.data.frame(table(na.omit(p14$subsubcat)))
drugs$RelFreq <- drugs$Freq/499830
colnames(drugs)[1] <- "Drug"

# drugs vs no info count
1018109-518279 #  499830

# 2-way no. 1
csc <- as.data.frame(table(p14$cat, p14$subcat))
csc$RelFreq <- csc$Freq/nrow(p14)
colnames(csc) <- c("Category", "SubCategory", "Freq", "RelFreq")

# 2-way no. 2
csd <- as.data.frame(table(p14$subcat, p14$subsubcat))
csd$RelFreq <- csd$Freq/nrow(p14)
colnames(csd)[1:2] <- c("SubCategory", "Drug")

# 3-way Frequency Table
allcats <- as.data.frame(table(p14$cat, p14$subcat, p14$subsubcat))
colnames(allcats) <- c("Category", "Sub", "SubSub", "Freq")
allcats$RelFreq <- allcats$Freq/nrow(p14)


# plot RelFreq/Distributions --------------------------------------------------

library(ggplot2)
library(extrafont)
library(extrafontdb)
font_import()

plot(cats$Category, cats$Freq)
plot(subcats$SubCategory, subcats$RelFreq)
plot(subcats$SubCategory, subcats$Freq)

reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

median(range(cats$RelFreq)) #  0.3490334

# category plot
c1 <- ggplot(cats, aes(reorder(Category, Freq), Freq, fill = RelFreq)) + 
  geom_bar(stat = "identity", colour = "black", size = 0.225) + 
  scale_fill_gradient2(low = "bisque2", mid = "white", high = "firebrick3",
                       midpoint = median(range(cats$RelFreq))) +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) + 
  labs(title = "Agora 2014: categories by frequency", x = "", y = "",
       fill = "relative frequency\n(n=1018109)") +
  coord_flip()

c1

# subcategory plot
sc1 <- ggplot(na.omit(subcats), aes(reorder(SubCategory, Freq), Freq, fill = RelFreq)) + 
  geom_bar(stat = "identity", colour = "black", size = 0.225) + 
  scale_fill_gradient2(low = "bisque2", mid = "white", high = "firebrick3",
                       midpoint = median(range(subcats$RelFreq))) +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) + 
  labs(title = "Agora 2014: sub-categories by frequency", x = "", y = "",
       fill = "relative frequency\n(n=1018109)") +
  coord_flip()

sc1

# drug plot
class(drugs)
drugs <- drugs[-29, ]

d1 <- ggplot(drugs, aes(reorder(Drug, Freq), Freq, fill = RelFreq)) + 
  geom_bar(stat = "identity", colour = "black", size = 0.225) + 
  scale_fill_gradient2(low = "bisque2", mid = "white", high = "firebrick3",
                       midpoint = median(range(na.omit(drugs$RelFreq)))) +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) + 
  labs(title = "Agora 2014: drug listings by frequency (NA removed)", 
       x = "", y = "", fill = "relative frequency\n(n=499830)") +
  coord_flip()

d1

# 3-way table - glitchy jitter
d3 <- ggplot(allcats, aes(SubSub, Sub, fill = Category)) + 
  geom_tile(stat = "identity", position = "jitter") + 
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) + 
  labs(title = "Agora 2014: Category ~ SubCategory ~ SubSubCategory", 
       x = "", y = "", fill = "category") +
  coord_flip()

d3 + scale_fill_manual(values = c("red3", 
                                  "bisque3", 
                                  "#00688B", 
                                  "#9AC0CD", 
                                  "red1", 
                                  "bisque1",
                                  "black", 
                                  "gold2", 
                                  "grey62", 
                                  "grey72",
                                  "gray82",
                                  "darkorange4",
                                  "firebrick4"))

# 3-way from p14
d3b <- ggplot(p14, aes(subcat, subsubcat, fill = cat)) + 
  geom_tile(stat = "identity", position = "dodge", size = 2) + 
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) + 
  labs(title = "Agora 2014: Category ~ SubCategory ~ SubSubCategory", 
       x = "", y = "", fill = "category") +
  coord_flip()

d3b + scale_fill_manual(values = c("red3", 
                                  "bisque3", 
                                  "#00688B", 
                                  "#9AC0CD", 
                                  "red1", 
                                  "bisque1",
                                  "black", 
                                  "gold2", 
                                  "grey62", 
                                  "grey72",
                                  "gray82",
                                  "darkorange4",
                                  "firebrick4"))

# two-way tables

tt1 <- ggplot(csc, aes(reorder(Category, Freq), SubCategory, fill = Freq)) + 
  geom_tile(colour = "grey50", size = 0.5) + 
  scale_fill_gradient(low = "bisque1", high = "firebrick3") +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) + 
  labs(title = "Agora 2014: categories by frequency", x = "", y = "",
       fill = "frequency")

tt1

tt2 <- ggplot(csd, aes(reorder(SubCategory, Freq), 
                       reorder(Drug, Freq), fill = Freq)) + 
  geom_tile(colour = "grey50", size = 0.5) + 
  scale_fill_gradient2(low = "antiquewhite", mid = "white",
                       high = "firebrick4", midpoint = 50000) +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) + 
  labs(title = "Agora 2014: categories by frequency", x = "", y = "",
       fill = "frequency")

tt2


tt3 <- ggplot(p14, aes(subcat, subsubcat, fill = cat)) + 
  geom_tile(colour = "white", size = 1.85) + 
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) + 
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) + 
  labs(title = "Agora 2014: Category ~ SubCategory ~ SubSubCategory", 
       x = "", y = "", fill = "category")

tt3 + scale_fill_manual(values = c("red3", 
                                   "bisque3", 
                                   "#00688B", 
                                   "#9AC0CD", 
                                   "red1", 
                                   "bisque1",
                                   "black", 
                                   "gold2", 
                                   "grey62", 
                                   "grey72",
                                   "gray82",
                                   "darkorange4",
                                   "firebrick4"))
