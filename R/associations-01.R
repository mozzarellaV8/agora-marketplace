# Agora Marketplace Analysis
# arules - 2014 data

# load data -------------------------------------------------------------------

# clear workspace and set directory
rm(list = ls())

getwd()
setwd("~/GitHub/agora-marketplace")

# read and bind data
pDir <- "~/GitHub/agora-marketplace/data/product"
product.list <- list.files(path = pDir, pattern = ".csv", all.files = T,
                           full.names = T, recursive = T)

p2014 <- data.frame()

system.time(
  for (i in 1:length(product.list)) {
    temp <- read.csv(product.list[i])
    p2014 <- rbind(p2014, temp)
  }
)

#    user  system elapsed
# 314.713  13.411 330.067

# safety
write.csv(p2014, file = "agora-2014.csv", row.names = F)

str(p2014)
# 772632 obs. of  11 variables

p2014$date <- as.Date(p2014$date)
summary(p2014)
quantile(p2014$price)

# need to remove duplicates in the to/from fields

# explore ---------------------------------------------------------------------

library(ggplot2)

plot(p2014$price)
# outliers at 3.5 million, most of population well under 1000 btc
par(mar = c(6, 6, 6, 6), family = "FranklinGothicSSK")
plot(p2014$date, p2014$price, main = "Agora 2014 - product prices in BTC")

options(scipen=999)

p1 <- ggplot(p2014, aes(date, price), colour = price) + 
  geom_point(size = 2, shape = 17) +
  theme_minimal(base_size = 12, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Product Prices in BTC", x = "", y = "")
  
p1

# subeset for listings with feedback

library(tm)
p2014$feedback <- as.factor(p2014$feedback)
p2014fb <- subset(p2014, p2014$feedback != "Feedbacks: No feedbacks found.")


