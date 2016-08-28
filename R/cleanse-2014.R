# Agora Marketplace Analysis
# 2014 data binding

# read and bind data ----------------------------------------------------------
pDir <- "~/GitHub/ag-p/p2014"
pList <- list.files(path = pDir, pattern = ".csv", all.files = T,
                           full.names = T, recursive = T)

p2014 <- data.frame()

system.time(
  for (i in 1:length(pList)) {
    temp <- read.csv(pList[i], stringsAsFactors = F)
    p2014 <- rbind(p2014, temp)
  }
)

#    user  system elapsed
#  60.263   2.891  64.025

# safety
write.csv(p2014, file = "~/GitHub/agora-data/ag01-2014.csv", row.names = F)

# cleanse ----------------------------------------------------------------------

library(data.table)
library(tidyr)
library(dplyr)
library(tm)

p14 <- fread("~/GitHub/agora-data/ag01-2014.csv")
str(p14)
summary(p14) # 1018109 obs. of  11 variables

p14$feedback <- stripWhitespace(p14$feedback)
