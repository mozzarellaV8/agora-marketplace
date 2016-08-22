# Agora Marketplace Analysis
# January 2014 Sample
# explore categories

# load data -------------------------------------------------------------------
getwd()
setwd("~/GitHub/agora-marketplace")

p14ct <- read.csv("~/GitHub/agora-marketplace/data/product/products-2014-01.csv")
str(p14ct)

p14ct$date <- as.Date(p14ct$date)
p14ct$cat <- as.character(p14ct$cat)
p14ct$subcat <- as.character(p14ct$subcat)
p14ct$subsubcat <- as.character(p14ct$subsubcat)

# create binary columans ------------------------------------------------------

# high level / main categories
levels(as.factor(p14ctct$cat))

p14ctct$drugs <- ifelse(p14ct$cat == "Drugs", 1, 0)
p14ct$counterfeits <- ifelse(p14ct$cat == "Counterfeits", 1, 0)
p14ct$data <- ifelse(p14ct$cat == "Data", 1, 0)
p14ct$dp <- ifelse(p14ct$cat == "Drug paraphernalia", 1, 0)
p14ct$info <- ifelse(p14ct$cat == "Information", 1, 0)
p14ct$forgeries <- ifelse(p14ct$cat == "Forgeries", 1, 0)
p14ct$Listings <- ifelse(p14ct$cat == "Listings", 1, 0)
p14ct$services <- ifelse(p14$cat == "Services", 1, 0)
p14ct$tobacco <- ifelse(p14$cat == "Tobacco", 1, 0)
p14ct$weapons <- ifelse(p14$cat == "Weapons", 1, 0)


p14ct$guides
p14ct$books


p14ct$benzos
p14ct$cannabis
p14ct$weed
p14ct$edibles
p14ct$synthetics
p14ct$hash
p14ct$ecstasy
p14ct$mda
p14ct$mdma
p14ct$methlone
p14ct$pills
p14ct$opioids
p14ct$other
p14ct$prescription
p14ct$psychedelics
p14ct$mescaline
p14ct$x2c
p14ct$xs5meo
p14ct$dmt
p14ct$lsd
p14ct$mushrooms
p14ct$nbomes
p14ct$salvia
p14ct$spores
p14ct$RCs
p14ct$steroids
p14ct$stimulants
p14ct$cocaine
p14ct$meth
p14ct$speed
p14ct$disassociatives
p14ct$gbl
p14ct$ghb
p14ct$ketamine
p14ct$mxe

p14ct$weightloss

p14ct$nonlethal
p14ct$explosives
p14ct$ammo
p14ct$melee
p14ct$nonlethal
p14ct$fireworks

p14ct$chemicals

