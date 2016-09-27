

library(data.table)
library(ggplot2)


# prepped data load ----------------------------------------------------------
ag <- fread("~/GitHub/agora-data/06-arules/ag-arules-20k.csv", stringsAsFactors = T)

par(mfrow = c(1, 1))
vendors <- as.data.frame(table(ag$v3))
colnames(vendors) <- c("v3", "num.listings")
vendors <- vendors[order(vendors$num.listings, decreasing = T), ]
rownames(vendors) <- NULL

tv <- subset(vendors, vendors$num.listings > 10000)
tv$v3 <- factor(tv$v3)


# sample distribution - price ~ category --------------------------------------
ag1k <- subset(ag, ag$usd < 500 & ag$from == "USA")
ag500<- ag1k[sample(nrow(ag1k), 2000), ]
ag500 <- subset(ag500, ag500$cat != "Electronics" & ag500$cat != "Jewelry"
                & ag500$cat != "Listings" & ag500$cat != "Other"
                & ag500$cat != "Chemicals")

ggplot(ag500, aes(cat, usd)) + 
  geom_dotplot(aes(cat, usd, fill = cat), color = "white",
               binwidth = 3.25, binaxis = "y", stackdir = "center") +
  scale_fill_manual(values = c("red3", "gold1", "deepskyblue4", "lightblue3", 
                               "red1",  "bisque1", "bisque3", "bisque4",  
                               "darkorange3", "firebrick4")) +
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.y = element_text(family = "Times New Roman", face = "italic"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  labs(title = "sample distribution of prices ~ category • price < $500 • n = 2000",
       x = "", y = "price USD") +
  guides(fill = F, color = F) 

ag100 <- subset(ag500, ag500$usd < 100)
ggplot(ag100, aes(cat, usd)) + 
  geom_dotplot(aes(cat, usd, fill = cat), color = "white",
               binwidth = 1, binaxis = "y", stackdir = "center") +
  scale_fill_manual(values = c("red3", "gold1", "deepskyblue4", "lightblue3", 
                               "red1",  "bisque1", "bisque3", "bisque4",  
                               "darkorange3", "firebrick4")) +
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.y = element_text(family = "Times New Roman", face = "italic"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  labs(title = "sample distribution of prices ~ category • price < $100 • n = 2000",
       x = "", y = "price USD") +
  guides(fill = F, color = F) 

agNoDrugs <- subset(ag500, ag500$cat != "Drugs" & ag500$usd < 100)

ggplot(agNoDrugs, aes(cat, usd)) + 
  geom_dotplot(aes(cat, usd, fill = cat), color = "white",
               binwidth = 1.5, binaxis = "y", stackdir = "center") +
  scale_fill_manual(values = c("red3", "gold1", "deepskyblue4", 
                               "red1",  "bisque1", "bisque3", "bisque4",  
                               "darkorange3", "firebrick4")) +
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.y = element_text(family = "Times New Roman", face = "italic"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  labs(title = "sample distribution of prices ~ category (Drugs removed) • price < $100 • n = 2000",
       x = "", y = "price USD") +
  guides(fill = F, color = F) 
