# Agora Marketplace Analysis
# Class/Category Probabilities 02 

# load data -------------------------------------------------------------------

library(data.table)

p14 <- fread("~/GitHub/agora-data/ag07-2014.csv", stringsAsFactors = T)
p14$date <- as.Date(p14$date)
