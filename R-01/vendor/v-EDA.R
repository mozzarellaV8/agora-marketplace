# Agora Vendors

# load data ------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(tidyr)

v14 <- fread("~/GitHub/agora-data//v14-02.csv", stringsAsFactors = T)
v14$fulldate <- v14$date
v14 <- separate(v14, fulldate, into = c("year", "month", "day"), sepp  = "-")

write.csv(v14, file = "~/GitHub/agora-data//v14-02a.csv")

# product-location contingency table
product <- as.data.frame(table(v14$product, v14$from))
product <- na.omit(product)
product <- subset(product, product$Freq > 0)

write.csv(product, file = "v-CT-ProductLocation-01.csv", row.names = F)
