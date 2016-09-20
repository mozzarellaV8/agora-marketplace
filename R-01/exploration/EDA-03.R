# Agora Marketplace Analysis
# Exploratory Cannabis Analysis - SubCategories

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)
library(tm)

p14 <- fread("~/GitHub/agora-data/ag03-2014.csv", stringsAsFactors = F)
p14$date <- as.Date(p14$date)
str(p14)

p14$to <- gsub("wideutschland", "", p14$to)
p14$to <- gsub("deutschlandstination", "", p14$to)
swedeutschlandn
outsideutschland
p14$to <- (as.factor(p14$to))

write.csv(p14, file = "~/GitHub/agora-data/ag03-2014.csv", row.names = F)

# quick looks ---------------------------------------------

length(unique(p14$product)) # 62873
summary(p14$usd)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0         29         95      15890        332 2215000000
summary(p14$cat)
summary(p14$subcat)
summary(p14$subsubcat)
709176/10118109 
# 0.07008978 - percentage of market listed as "Drugs"

# subset over $1000 -----------------------------------------------------------

# Placeholders and Outliers
waitlist <- subset(p14, p14$price >= 3500)
waitlist <- waitlist[order(waitlist$price, decreasing = T), ]

waitTable <- as.data.frame(table(waitlist$subcat))
waitTable <- waitTable[order(waitTable$Freq, decreasing = T), ]
colnames(waitTable) <- c("subCategory", "Freq")
table(waitlist$cat)

# snapshotp of what's out of stock
waitT2 <- as.data.frame(table(waitlist$subsubcat))

# 1000 USD subset -------------------------------------------------------------

# subset without feedback
p14b <- subset(p14, p14$usd <= 1000) # 905805 

# subset including feedback
p14$feedback <- as.character(p14$feedback)
p14c <- subset(p14,  p14$feedback != " Feedbacks: No feedbacks found. " &
                 p14$usd <= 1000) # 442716

summary(p14c)
p14c$from[which(is.na(p14c$from))] <- "no info"
levels(as.factor(p14c$from))

# safety
write.csv(p14c, file = "~/GitHub/agora-data/ag04-2014.csv", row.names = F)
p14 <- fread("~/GitHub/agora-data/ag04-2014.csv", stringsAsFactors = T)

p14$from <- gsub("the united snakes of captivity", "usa", p14$from)
p14$from <- as.factor(p14$from)

# imputing subcats ------------------------------------------------------------

library(mice)

cats <- data.frame(cat = p14$cat, subcat = p14$subcat, subsubcat = p14$subsubcat)



# Densities: Price, Categories, Locations, Products ---------------------------

library(ggplot2)
library(RColorBrewer)
library(vcd)
library(extrafontdb)
font_import()

p1 <- ggplot(p14, aes(usd)) + theme_minimal() +
  geom_density(fill = "#CD0000", alpha = 0.2) +
  geom_line(stat = "density") + expand_limits(y = 0) +
  theme(plot.title = element_text(family= "Times", face = "bold", size = 16)) +
  labs(title = "AgMarket 2014 - List Price Density", x = "list price in USD")

p1 + theme(axis.title.x = element_text(family = "Times", face = "italic", size = 12)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 12)) +
  theme(axis.text.x = element_text(family = "Times", face = "plain", size = 12)) +
  theme(axis.text.y = element_text(family = "Times", face = "plain", size = 12)) +
  theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0))) + 
  theme(axis.title.x = element_text(margin = margin(40, 0, 0, 0))) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))

p2 <- ggplot(p14, aes(x = sqrt(usd), fill = usd)) + 
  geom_density(alpha = 0.5) +
  geom_histogram(mapping = )
xlim(-10, 100)

p2

p3 <- ggplot(p14, aes(subcat, from, fill = cat)) + geom_tile() + 
  labs(title = "Agora 2014: Category / Subcategory :: Location", 
       y = "", x = expression(subcat %subset% cat)) +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(-20, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 9))

p3 + scale_fill_manual(values = c("red3", "#998976", "#B2DFEE", "#9AC0CD", 
                                  "#B2DFEE", "red2", "bisque2", "gray23",
                                  "bisque3", "bisque4", "bisque", "bisque4",
                                  "firebrick4"))
