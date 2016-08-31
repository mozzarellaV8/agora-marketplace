# Agora Marketplace Analysis
# Table creation

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)
library(tm)

p14 <- fread("~/GitHub/agora-data/ag05-2014.csv", stringsAsFactors = T)
p14$date <- as.Date(p14$date)
str(p14)

# tables ----------------------------------------------------------------------

# when?
tDate <- as.data.frame(table(p14$date))
colnames(tDate) <- c("date", "freq")

# who? most prolific vendors
tv <- as.data.frame(table(p14$vendor))

# what? most frequently listed products
tp <- as.data.frame(table(p14$product))

# how much?
tBTC <- as.data.frame(table(p14$price))
tUSD <- as.data.frame(table(p14$usd))

# where from?
tFrom <- as.data.frame(table(p14$from))

# what category?
tc <- as.data.frame(table(p14$cat))
tsc <- as.data.frame(table(p14$subcat))
tssc <- as.data.frame(table(p14$subsubcat))

# cross tablulation
cl <- xtabs(~cat + from + subsubcat, data = p14)
print(cl)
summary(cl)
