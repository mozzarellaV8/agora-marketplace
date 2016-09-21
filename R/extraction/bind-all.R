# Agora Marketplace Analysis
# bind 2014+2015 data into one.

library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)

library(qdap)
library(zoo)
library(tm)

pDir <- "~/GitHub/agora-marketplace"
setwd(pDir)

# bind 2014 + 2015 ------------------------------------------------------------
p14 <- fread("~/GitHub/agora-data/ag14-01.csv", stringsAsFactors = T)
p15 <- fread("~/GitHub/agora-data/ag15-04.csv", stringsAsFactors = T)

# bind 2014 + 2015 ------------------------------------------------------------  
  
levels(as.factor(p15$from)) # 71 levels
levels(as.factor(p14$from)) # 76 levels

agora <- rbind(p14, p15)
nrow(agora) # 2322961 obs of 18 variables

levels(as.factor(agora$from)) # 92 levels
levels(as.factor(agora$to))   # 2180 levels - very messy
agora$to <- stripWhitespace(as.character(agora$to))

write.csv(agora, file = "~/GitHub/agora-data/agora-01.csv", row.names = F)
agora <- fread("~/GitHub/agora-data/agora-01.csv", stringsAsFactors = T)
