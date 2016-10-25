# Bootstrap Validation
# NB model

library(MASS)
library(data.table)
library(ggplot2)
library(broom)
library(extrafont)
library(extrafontdb)

# population
ag <- fread("~/GitHub/agora-data/agora-02.csv", stringsAsFactors = T)
ag$date <- as.Date(ag$date)

# fotified negative binomial model
nb01a <- fread("~/GitHub/agora-marketplace/data/validation/dailyNB-01.csv")
nb01a$date <- as.Date(nb01a$date)


