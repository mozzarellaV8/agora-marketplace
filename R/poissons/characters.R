# character counts: product listings

library(data.table)
library(sandwich)
library(ggplot2)
library(dplyr)
library(broom)

library(extrafont)
library(extrafontdb)

# load data -------------------------------------------------------------------
# agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
# agora$date <- as.Date(agora$date)
# agora$usd <- round(agora$usd, 2)

# ag <- subset(agora, select = c("year", "month", "day", "date", "product",
#                                "price", "usd", "cat", "subcat", "subsubcat",
#                                "fb", "feedback", "from"))

# tranform dates
# ag$j <- ag$date
# ag$j <- julian(ag$j)

# ag <- as.data.frame(ag)
# ag <- ag[c("year", "month", "day", "j", "date", "product",
#            "price", "usd", "cat", "subcat", "subsubcat",
#            "fb", "feedback", "from", "list")]

# 00 - which product listings have numbers? -------------------
# ag$has.num<- grepl("[0-9]+", ag$product)

# ag.n <- subset(ag, ag$pnum == T) # 1821074
# ag.c <- subset(ag, ag$pnum == F) # 501887


# 00 - length of product description ------------------------------------------
# ag$product <- as.character(ag$product)
# ag$cc <- nchar(as.character(ag$product))

# load data -------------------------------------------------------------------
agc <- fread("~/GitHub/agora-data/agora-02.csv", stringsAsFactors = T)
agc$date <- as.Date(agc$date)

summary(agc$has.num)
#    Mode   FALSE    TRUE    NA's 
# logical  501887 1821074       0 

summary(agc$cc)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.00   25.00   36.00   38.81   49.00  287.00      18

par(mfrow = c(2, 1), mar = c(5, 5, 5, 5), las = 1, family = "GillSans")
hist(agc$cc, breaks = 50, main = "character count: Product Listings",
     xlab = "number of characters", ylab = "")
hist(log(agc$cc), breaks = 100, 
     main = "(log) character count: Product Listings",
     xlab = "(log) number of characters", ylab = "")

# do character counts vary over time?
plot(agc$j, agc$cc)
# do character counts vary over category?
plot(agc$cat, agc$cc)
# vary over subcategory?
plot(agc$cc, agc$subcat)
# vary by drug?
plot(agc$cc, agc$subsubcat)

par(mfrow = c(1, 1), mar = c(5, 5, 5, 5), las = 1, family = "GillSans")
plot(agc$has.num)


# charcount over time ---------------------------------------------------------

# prep
dates <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "month")
dw <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "week")

# plot
p.cd <- ggplot(agc, aes(cc, date)) + 
  geom_hex(aes(cc, date, color = cc), bins = 80) +
  
  scale_y_date(breaks = dw, labels = dw) +
  scale_fill_gradient2(low = "deepskyblue4", mid = "antiquewhite",
                       high = "firebrick3", midpoint = 4000) +
  
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.25, 0.5), "cm"),
        panel.grid.major = element_line(colour = "gray82"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(20, 0, 0, 0))) +
  labs(title = "Product Listings: Character Count by Date",
       x = "number of characters", y = "", fill = "frequency")

p.cd

# vertical
p.cdv <- ggplot(agc, aes(date, cc)) + 
  geom_hex(aes(date, cc, color = cc), bins = 80) +
  
  scale_x_date(breaks = dw, labels = dw) +
  scale_fill_gradient2(low = "deepskyblue4", mid = "antiquewhite",
                       high = "firebrick3", midpoint = 4000) +
  
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.25, 0.5), "cm"),
        panel.grid.major = element_line(colour = "gray82"),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1,
                                   vjust = 0.5),
        axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5)) +
  labs(title = "Product Listings: Character Count by Date",
       x = "", y = "", fill = "")

p.cdv

# charcount by location -------------------------------------------------------

p.cl <- ggplot(agc, aes(cc, from)) + 
  
  geom_tile(aes(cc, from, fill = cc),  color = "white") +
  
  scale_fill_gradient2(low = "deepskyblue4", mid = "bisque2",
                       high = "firebrick3", midpoint = 150) +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.25, 0.5), "cm"),
        panel.grid.major = element_line(colour = "gray82"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12.5),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(20, 0, 0, 0))) +
  labs(title = "Product Listings: Character Count by Location",
       x = "number of characters", y = "", color = "character\ncount")

p.cl

# charcount by category -------------------------------------------------------

p.nchar0 <- ggplot(agc, aes(cc, cat)) + 
  geom_tile(aes(cc, cat, fill = cc),  color = "white") +
  
  scale_fill_gradient2(low = "deepskyblue4", mid = "bisque2",
                       high = "firebrick3", midpoint = 150) +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.y = element_text(size = 12.75),
        axis.text.x = element_text(size = 12.75),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(20, 0, 0, 0))) +
  labs(title = "Product Listings: Character Count by Category",
       x = "number of characters", y = "", color = "")

p.nchar0

# charcount by subcategory ----------------------------------------------------

# prep
# agc$subcat <- as.character(agc$subcat)
# agc$subcat[agc$subcat == "No Info/Other"] <- "Other"
# agc$subcat <- factor(agc$subcat)
# levels(agc$subcat)

# plot
p.nchar1 <- ggplot(agc, aes(cc, subcat)) + 
  geom_tile(aes(cc, subcat, fill = cc),  color = "white") +
  
  scale_fill_gradient2(low = "deepskyblue4", mid = "bisque2",
                       high = "firebrick3", midpoint = 150) +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12.75),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(20, 0, 0, 0))) +
  labs(title = "Product Listings: Character Count by Sub-Category",
       x = "number of characters", y = "", color = "")

p.nchar1

# charcount by sub-subcategory ------------------------------------------------

# agc$subsubcat <- factor(agc$subsubcat)
# levels(agc$subsubcat)

p.nchar2 <- ggplot(agc, aes(cc, subsubcat)) + 
  geom_tile(aes(cc, subsubcat, fill = cc),  color = "white") +
  
  scale_fill_gradient2(low = " deepskyblue4", mid = "bisque2",
                       high = "firebrick3", midpoint = 150) +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text.y = element_text(size = 12.5),
        axis.text.x = element_text(size = 12.75),
        axis.title.y = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(family = "Times New Roman",
                                    face = "italic",
                                    margin = margin(20, 0, 0, 0))) +
  labs(title = "Product Listings: Character Count by Sub-Subcategory",
       x = "number of characters", y = "", color = "")

p.nchar2

plot.new()
frame()

# charcount by stacked category -----------------------------------------------

# combine sub- and subsub-categories
# agc$sc <- paste(agc$subcat, agc$subsubcat, sep = "-")
# agc$sc <- gsub("-NA$", "", agc$sc)
# agc$sc <- gsub("Methylone", "RCs", agc$sc)
# agc$sc <- factor(agc$sc)
# levels(agc$sc)


# plot
p.nchar3 <- ggplot(agc, aes(sc, cc)) + 
  geom_tile(aes(sc, cc, fill = cc),  color = "white", na.rm = T) +
  
  scale_fill_gradient2(low = "deepskyblue", mid = "antiquewhite1",
                       high = "firebrick3", midpoint = 125) +
  scale_y_continuous(breaks = seq(0, 300, 50)) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
        axis.text.y = element_text(size = 12.75, angle = 90, hjust = 0.5),
        axis.text.x = element_text(size = 11.25, angle = 90, hjust = 1,
                                   vjust = 0.5),
        legend.text = element_text(size = 8, angle = 90, vjust = 0.5,
                                   margin = margin(c(40, 40, 40, 40))),
        legend.key = element_blank()) +
  labs(title = "Product Listings: Character Count by Subcategories",
       x = "", y = "", fill = "")

p.nchar3

p.nchar3b <- ggplot(agc, aes(cc, sc)) + 
  geom_tile(aes(cc, sc, fill = cc),  color = "white", na.rm = T) +
  
  scale_fill_gradient2(low = "deepskyblue4", mid = "antiquewhite2",
                       high = "firebrick4", midpoint = 125) +
  scale_x_continuous(breaks = seq(0, 300, 25)) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.25, 0.25, 0.5, 0.25), "cm"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 11.25)) +
  labs(title = "Product Listings: Character Count by Subcategories",
       x = "", y = "", fill = "")

p.nchar3b

plot.new()
frame()

# charcount by all categories -------------------------------------------------
# agc$all.c <- paste(agc$cat, agc$sc, sep = ": ")
# agc$all.c <- gsub("Drugs: Methylone", "Drugs: RCs", agc$all.c)
# agc$all.c <- factor(agc$all.c)
levels(agc$all.c)

# horizontal 
p.nchar4 <- ggplot(agc, aes(cc, all.c)) + 
  geom_tile(aes(cc, all.c, fill = cc),  color = "white", na.rm = T) +
  scale_fill_gradient2(low = " deepskyblue4", mid = "bisque2",
                       high = "firebrick3", midpoint = 150) +
  scale_x_continuous(breaks = seq(0, 300, 25)) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), 
    axis.text.y = element_text(size = 9.25, lineheight = 1.2, hjust = 0),
    axis.text.x = element_text(size = 12.75),
    axis.title.x = element_text(family = "Times New Roman", face = "italic", margin = margin(20, 0, 0, 0)),
    legend.text = element_text(size = 8),
    legend.position = c(1, 1), legend.justification = c(1, 1),
    legend.key = element_blank()) +
  
  labs(title = "Product Listings: Character Count by all Categories",
       x = "number of characters", y = "", fill = "character\ncount")

p.nchar4

# vertical 
p.nchar5 <- ggplot(agc, aes(all.c, cc)) + 
  geom_tile(aes(all.c, cc, fill = cc),  color = "white", na.rm = T) +
  scale_fill_gradient2(low = "deepskyblue4", mid = "antiquewhite2",
                       high = "firebrick3", midpoint = 125) +
  scale_y_continuous(breaks = seq(0, 300, 25)) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
        axis.text.y = element_text(size = 12.75, angle = 90, vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        legend.text = element_text(size = 10, angle = 90, hjust = 1,
                                   margin = margin(c(0, 40, 40, 0))),
        legend.position = "bottom",
        legend.key = element_blank()) +
  labs(title = "Product Listings: Character Count by all Categories",
       y = "", x = "", fill = "")
  
p.nchar5

plot.new()
frame()

# horizontal w/ 45-degree x-axis
p.nchar6 <- ggplot(agc, aes(all.c, cc)) + 
  geom_tile(aes(all.c, cc, fill = cc), color = "white", na.rm = T) +
  scale_y_continuous(breaks = seq(0, 300, 25)) +
  scale_fill_gradient2(
    low = " deepskyblue4", mid = "antiquewhite2",
    high = "firebrick4", midpoint = 125) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
        axis.text.y = element_text(size = 12.75, angle = 90, vjust = 1),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        legend.text = element_text(size = 10, angle = 90, hjust = 1,
                                   margin = margin(c(0, 40, 40, 0))),
        legend.position = "top",
        legend.key = element_blank()) +
  labs(title = "Product Listings: Character Count by all Categories",
       y = "", x = "", fill = "")

p.nchar6

write.csv(agc, file = "~/GitHub/agora-data/agora-02.csv", row.names = F)
