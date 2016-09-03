# Agora Marketplace
# Counterfeit Exploration

# load data -------------------------------------------------------------------

library(data.table)

p14 <- fread("~/GitHub/agora-data/ag07-2014.csv")
levels(as.factor(p14$cat))

cf <- subset(p14, p14$cat == "Counterfeits") # 44151 listings
length(unique(cf$product)) # 1460 listings

44150/1018109   # counterfeit/population
0.04336471*100  # multiplied by 100
# 4.336471      # 4.34% percent of listings

# counts by sub ---------------------------------------------------------------

library(ggplot2)
library(vcd)
library(RColorBrewer)

levels(as.factor(cf$vendor)) # 81
levels(as.factor(cf$from)) # 16
levels(as.factor(cf$to)) # 19 messy

# quick clean destinations
cf$to <- gsub("^wor(.*)", "worldwide", cf$to)
cf$to <- gsub("\\bww\\b", "worldwide", cf$to)
cf$to <- gsub("^usa(.*)", "usa", cf$to)
cf$to <- gsub("^anywhere(.*)", "anywhere beside usa, canada", cf$to)

levels(as.factor(cf$to)) # now 12

summary(cf$usd)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   0.00    74.67   135.40   227.90   278.60 80380.00 

# Money subcategory - counterfeit bills - throws the prices off towards high.

# stray RC listing from drugs - remove
cf <- as.data.frame(cf)
which(cf$subcat == "RCs") # 118
cf <- cf[-118, 1:17]

# frequency table
c.sub <- as.data.frame(table(cf$subcat))
colnames(c.sub) <- c("category", "count")
c.sub <- c.sub[order(c.sub$count, decreasing = T), ]
rownames(c.sub) <- NULL

# plot prices -----------------------------------------------------------------
cp1 <- ggplot(cf, aes(subcat, usd, fill = usd)) + 
  geom_point() +
  labs(title = "Counterfeits - Subcategory by Price", 
           y = "", x = "", fill = "price (USD)")

cp1 + theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  scale_y_continuous(limits = c(0, 15100))

# Majority of listings are under 2500 USD - 
# ylim set to 15100, removed 2 listings

cp1b <- ggplot(cf, aes(subcat, usd, shape = subcat, colour = usd)) + 
  geom_point(size = 4) +
  scale_shape_manual(values = c(1, 2, 3, 8, 4)) +
  scale_y_continuous(limits = c(0, 10000)) +
  labs(title = "Counterfeits - Subcategory by Price", 
       y = "", x = "", fill = "price (USD)", shape = "sub-category")
cp1b

# ylim at 2500  removes 103 listings
cf2500 <- subset(cf, cf$usd <= 2500)
cp1c <- ggplot(cf2500, aes(subcat, usd, shape = subcat, colour = usd)) + 
  geom_point(size = 4, alpha = 0.25) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  scale_y_continuous(limits = c(0, 2500)) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) + 
  labs(title = "Counterfeits - Subcategory by Price", 
       y = "", x = "", colour = "price (USD)", shape = "sub-category")
cp1c

# bar -------------------------------------------------------------------------
  
# reorder function
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

# category by location
cp2 <- ggplot(cf, aes(reorder_size(from), fill = reorder_size(subcat))) + 
  geom_bar(position = "dodge") +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  labs(title = "Agora 2014: Counterfeits by Location", 
       x = "", y = "", fill = "category") +
  scale_fill_manual(values = c("bisque2", "darkslategray4", "deepskyblue4",
                               "red2", "firebrick4")) +
  coord_flip()

cp2

# facets ----------------------------------------------------------------------

cf$month <- as.factor(cf$month)
cf2 <- melt(cf, id.vars = c("from", "month", "subcat"))

levels(cf2$variable)
cf2 <- subset(cf2, cf2$variable == "usd")
cf2$from <- as.factor(cf2$from)
cf2$subcat <- as.factor(cf2$subcat)

cf2 <- cf2[, -c(4, 5)]
levels(cf2$month)[levels(cf2$month) == "01"] <- "January"
levels(cf2$month)[levels(cf2$month) == "02"] <- "February"
levels(cf2$month)[levels(cf2$month) == "03"] <- "March"
levels(cf2$month)[levels(cf2$month) == "04"] <- "April"
levels(cf2$month)[levels(cf2$month) == "05"] <- "May"
levels(cf2$month)[levels(cf2$month) == "06"] <- "June"
levels(cf2$month)[levels(cf2$month) == "07"] <- "July"
levels(cf2$month)[levels(cf2$month) == "08"] <- "August"
levels(cf2$month)[levels(cf2$month) == "09"] <- "September"
levels(cf2$month)[levels(cf2$month) == "10"] <- "October"
levels(cf2$month)[levels(cf2$month) == "11"] <- "November"
levels(cf2$month)[levels(cf2$month) == "12"] <- "December"

# facetted by month - growth
cp32 <- ggplot(cf2, aes(reorder_size(from), fill = reorder_size(subcat))) + 
  geom_bar(position = "dodge") +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(face = "bold", margin = margin(0, 0, 20, 0))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(1.75, 2, 2, 2.5), "cm")) +
  labs(title = "Agora 2014: Counterfeits + Location ~ Month", 
       x = "", y = "", fill = "category") +
  scale_fill_manual(values = c("bisque2", "darkslategray4", "deepskyblue4",
                               "red2", "firebrick4")) +
  coord_flip() +
  facet_wrap(~ month)

cp32


# only the players
cf3 <- subset(cf2, cf2$from == "china" | cf2$from == "hong kong" | 
                cf2$from == "no info" | cf2$from == "usa" |
                cf2$from == "torland")

cf3p <- ggplot(cf3, aes(reorder_size(from), fill = reorder_size(subcat))) + 
  geom_bar(position = "dodge") +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(face = "bold", margin = margin(0, 0, 20, 0))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9.5)) +
  theme(strip.text = element_text(face = "bold")) +
  theme(plot.margin = unit(c(1.75, 2, 2, 2.5), "cm")) +
  labs(title = "Agora 2014: Counterfeits + Location ~ Month", 
       x = "", y = "", fill = "category") +
  scale_fill_manual(values = c("bisque2", "darkslategray4", "deepskyblue4",
                               "red2", "firebrick4")) +
  coord_flip() +
  facet_wrap(~ month)

cf3p


# tiles -----------------------------------------------------------------------

# category by location + destination
cp3 <- ggplot(cf, aes(to, from, fill = subcat)) + 
  geom_tile(colour = "white", size = 6) +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  labs(title = "Agora 2014: Counterfeits ~ Origin + Destination", 
       x = "Destination", y = "Origin", fill = "Category") +
  scale_fill_manual(values = c("bisque2", "red2", "deepskyblue4",
                               "darkslategray4", "firebrick4"))
cp3

# listings by month and location
cp5 <- ggplot(cf2, aes(month, reorder_size(from), 
                       fill = reorder_size(subcat))) + 
  geom_tile(colour = "white", size = 6) +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  labs(title = "Agora 2014: Counterfeits by Location + Month", 
       x = "month", y = "", fill = "category") +
  scale_fill_manual(values = c("bisque2", "darkslategray4", "deepskyblue4",
                               "red2", "firebrick4"))

cp5

# catgetory by location + vendor
cp4 <- ggplot(cf, aes(from, vendor, fill = subcat)) + geom_tile() +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  labs(title = "Agora 2014: Counterfeits ~ Location + Vendor", 
       x = "", y = "", fill = "category") +
  scale_fill_manual(values = c("bisque2", "red2", "deepskyblue4",
                               "darkslategray4", "firebrick4"))

cp4

# specific product info -------------------------------------------------------

library(qdap)

cf <- read.csv("~/GitHub/agora-data/counterfeit-brands.csv", stringsAsFactors = F)
cf2 <- cf[!duplicated(cf$product), ] # remove duplicates
cf[, 14:24] <- NULL
cf2[, 14:24] <- NULL # remove extra columns

cfu <- unique(cf$product)
cfu <- wfm(cfu)
cfu <- as.data.frame(cfu)
cfu$word <- rownames(cfu)
cfu <- cfu[order(cfu$all, decreasing = T), ]
rownames(cfu) <- NULL

# dataframe with brand counts
cfb <- cf
cfb$subsubcat <- NULL

cfb$brand <- ifelse(grepl("Rolex", cfb$product, ignore.case = T) == T, "Rolex",
                ifelse(grepl("omega", cfb$product, ignore.case = T) == T, "Omega",
                ifelse(grepl("breitling", cfb$product, ignore.case = T)==T, "Breitling", 
                ifelse(grepl("nike", cfb$product, ignore.case = T)==T, "Nike",
                ifelse(grepl("panerai", cfb$product, ignore.case = T)==T, "Panerai", 
                ifelse(grepl("tag\\sh(.*)", cfb$product, ignore.case = T)==T, "Tag Heuer", 
                ifelse(grepl("hublot", cfb$product, ignore.case = T)==T, "Hublot", 
                ifelse(grepl("louis\\svuitton", cfb$product, ignore.case = T)==T, "Louis Vuitton",
                       ifelse(grepl("\\bLV\\b", cfb$product, ignore.case = T)==T, "Louis Vuitton",       
                ifelse(grepl("chanel", cfb$product, ignore.case = T)==T, "Chanel", 
                       ifelse(grepl("\\bchannel\\b", cfb$product, ignore.case = T)==T, "Chanel", 
                ifelse(grepl("gucci", cfb$product, ignore.case = T)==T, "Gucci",
                ifelse(grepl("alexander\\smcqueen", cfb$product, ignore.case = T)==T, "Alexander McQueen",
                ifelse(grepl("victoria\\'s\\ssecret", cfb$product, ignore.case = T)==T, "Victoria's Secret",
                ifelse(grepl("patek(.*)", cfb$product, ignore.case = T)==T, "Patek Phillipe", 
                ifelse(grepl("cartier", cfb$product, ignore.case = T)==T, "Cartier",
                ifelse(grepl("bentley", cfb$product, ignore.case = T)==T, "Bentley", 
                ifelse(grepl("montblanc", cfb$product, ignore.case = T)==T, "Mont Blanc",
                       ifelse(grepl("\\bMont\\sBlanc\\b", cfb$product, ignore.case = T)==T, "Mont Blanc",
                ifelse(grepl("hermes", cfb$product, ignore.case = T)==T, "Hermes",
                ifelse(grepl("yeezy", cfb$product, ignore.case = T)==T, "Yeezy",
                ifelse(grepl("diesel", cfb$product, ignore.case = T)==T, "Diesel",
                ifelse(grepl("samsung", cfb$product, ignore.case = T)==T, "Samsung",
                ifelse(grepl("bvlgari", cfb$product, ignore.case = T)==T, "Bvlgari",
                       ifelse(grepl("bulgari", cfb$product, ignore.case = T)==T, "Bvlgari",
                ifelse(grepl("versace", cfb$product, ignore.case = T)==T, "Versace",
                       ifelse(grepl("Oakley", cfb$product, ignore.case = T)==T, "Oakley",       
                       ifelse(grepl("Armani", cfb$product, ignore.case = T)==T, "Armani",
                       ifelse(grepl("Burberry", cfb$product, ignore.case = T)==T, "Burberry",
                       ifelse(grepl("\\bRayBan\\b", cfb$product, ignore.case = T)==T, "Ray Ban", 
                              ifelse(grepl("\\bRay·Ban\\b", cfb$product, ignore.case = T)==T, "Ray Ban", 
                       ifelse(grepl("\\bMarc\\sJacobs\\b", cfb$product, ignore.case = T)==T, "Marc Jacobs",
                       ifelse(grepl("\\bMichael\\sKors\\b", cfb$product, ignore.case = T)==T, "Michael Kors",
                       ifelse(grepl("\\bprada\\b", cfb$product, ignore.case = T)==T, "Prada",
                       ifelse(grepl("\\bJORDAN\\b", cfb$product, ignore.case = T)==T, "Jordan",
                       ifelse(grepl("\\bLouboutin\\b", cfb$product, ignore.case = T)==T, "Christian Louboutin", 
                       ifelse(grepl("\\bMiuMiu\\b", cfb$product, ignore.case = T)==T, "MiuMiu",
                       ifelse(grepl("\\bdsquared\\b", cfb$product, ignore.case = T)==T, "DSquared",
                       ifelse(grepl("\\bHugo\\sBoss\\b", cfb$product, ignore.case = T)==T, "Hugo Boss",
                       ifelse(grepl("\\bPOLO\\b", cfb$product, ignore.case = T)==T, "Polo",
                       ifelse(grepl("\\bCanada\\sGoose\\b", cfb$product, ignore.case = T)==T, "Canada Goose",
                       ifelse(grepl("\\bAudemars\\b", cfb$product, ignore.case = T)==T, "Audemars Piguet",
                       ifelse(grepl("\\bJaeger-LeCoultre\\b", cfb$product, ignore.case = T)==T, "Jaeger-LeCoultre",
                       ifelse(grepl("\\bVacheron\\b", cfb$product, ignore.case = T)==T, "Vacheron Constantin",
                       ifelse(grepl("\\bIWC\\b", cfb$product, ignore.case = T)==T, "IWC Schaffhausen",
                       ifelse(grepl("\\bChopard\\b", cfb$product, ignore.case = T)==T, "Chopard",
                              ifelse(grepl("gabbana", cfb$product, ignore.case = T)==T, "Dolce & Gabbana",
                                     ifelse(grepl("Gabanna", cfb$product, ignore.case = T)==T, "Dolce & Gabbana",
                       ifelse(grepl("\\bD&G\\b", cfb$product, ignore.case = T)==T, "Dolce & Gabbana", "No Info")))))))))))))))))))))))))))))))))))))))))))))))))

write.csv(cfb, file = "~/GitHub/agora-data/cfb-brands.csv", row.names = F)
# cfb <- read.csv("~/GitHub/agora-data/cfb-brands.csv")

cfb$brand <- as.factor(cfb$brand) # 43-44 levels
levels(cfb$brand)
table(cfb$brand)

# examine No Info -------------------------------------------------------------
#
# some have been labelled 
# look into binding those to the main cfb frame
#
cfni <- subset(cfb, cfb$brand == "No Info") # 17043 > 9114 > 8269 > 7922 > 7668
levels(cfni$subcat)
table(cfni$subcat)

# money subcat subset
cfmoney <- subset(cfni, cfni$subcat == "Money")
cfmoney$brand <- as.character(cfmoney$brand)
cfmoney$brand[cfmoney$brand == "No Info"] <- "bank notes, buillon, accounts"

# electronics subcat subset
cfe <- subset(cfni, cfni$subcat == "Electronics")

cfe$brand <- ifelse(grepl("\\bDr. Dre\\b", cfe$product, ignore.case = T)==T, "Beats by Dre", 
             ifelse(grepl("usb(.*)", cfe$product, ignore.case = T)==T, "USB Privacy/Security",
             ifelse(grepl("wifi", cfe$product, ignore.case = T)==T, "wifi jammer",
             ifelse(grepl("phone", cfe$product, ignore.case=T)==T, "phone product",
             ifelse(grepl("\\bsim\\b", cfe$product, ignore.case=T)==T, "phone product",
             ifelse(grepl("landline(.*)", cfe$product, ignore.case=T)==T, "phone product",
             ifelse(grepl("\\bemp\\spulse\\b", cfe$product, ignore.case = T)==T, "EMP Pulse Generator", "accounts/electronics/misc")))))))

cfe$brand <- as.factor(cfe$brand)
levels(cfe$brand)
table(cfe$brand)

# clothing cfni
cfc <- subset(cfni, cfni$subcat == "Clothing")
# cavalli, amani, Zanotti, Parajumpers, Paul&Shark, PHILIPP PLEIN, 

# accessories cfni
cfa <- subset(cfni, cfni$subcat == "Accessories")
# REALISTIC SILICONE MASK, Tag Remover, Spy, Stoplok Pegboard, Amazon


# brand plots -----------------------------------------------------------------

cfb1 <- ggplot(cfb, aes(reorder_size(brand), fill = reorder_size(subcat))) + 
  geom_bar(position = "dodge") +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  labs(title = "Agora 2014: Counterfeits by Brand", 
       x = "", y = "", fill = "category") +
  scale_fill_manual(values = c("bisque2", "darkslategray4", "deepskyblue4",
                               "red2", "firebrick4")) +
  coord_flip()

cfb1

# brand + location
cfb2 <- ggplot(cfb, aes(from, brand, fill = subcat)) + 
  geom_tile(colour = "white", size = 3.5) +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(face = "bold", 
                                  margin = margin(0, 0, 20, 0))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  labs(title = "Agora 2014: Counterfeits ~ Brand + Location", 
       x = "", y = "", fill = "category") +
  scale_fill_manual(values = c("bisque2", "red2", "deepskyblue4",
                               "darkslategray4", "firebrick4"))

cfb2


