# Agora Marketplace Analysis
# 2014 data binding

# load data -------------------------------------------------------------------

# clear workspace and set directory
rm(list = ls())
getwd()
setwd("~/GitHub/agora-marketplace")

# read and bind data ----------------------------------------------------------
pDir <- "~/GitHub/agora-marketplace/data/product"
product.list <- list.files(path = pDir, pattern = ".csv", all.files = T,
                           full.names = T, recursive = T)

p2014 <- data.frame()

system.time(
  for (i in 1:length(product.list)) {
    temp <- read.csv(product.list[i], stringsAsFactors = F)
    p2014 <- rbind(p2014, temp)
  }
)

#    user  system elapsed
# 314.713  13.411 330.067
#  65.966   1.959  68.202

tail(p2014$date) 
# 2014-11-12: didn't extract subcategories

# safety
write.csv(p2014, file = "~/GitHub/agora-data/agora-2014.csv", row.names = F)

str(p2014)
summary(p2014) # 772632 obs. of  11 variables