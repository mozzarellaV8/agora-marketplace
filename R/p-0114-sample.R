# Agora Marketplace Analysis
# January 2014 Sample
# explore categories

# load data -------------------------------------------------------------------
getwd()
setwd("~/GitHub/agora-marketplace")

p14 <- read.csv("~/GitHub/agora-marketplace/data/product/products-2014-01.csv")
str(p14)

p14$date <- as.Date(p14$date)

# feedback as transaction - how many?
p14$feedback <- as.character(p14$feedback)
fb <- subset(p14, p14$feedback != "\n    Feedbacks:\n    No feedbacks found.\n")
# number of listings drops from 7986 listings to 1430. 

levels(fb$cat)
#  [1] "Counterfeits"       "Data"               "Drug paraphernalia" "Drugs"              "Forgeries"         
#  [6] "Information"        "Listings"           "Services"           "Tobacco"            "Weapons"

levels(fb$subcat)
# [1] "Accessories"         "Accounts"            "Ammunition"          "Benzos"              "Cannabis"           
# [6] "Clothing"            "Containers"          "Disassociatives"     "eBooks"              "Ecstasy"            
# [11] "Ecstasy-MDMA"        "Electronics"         "Guides"              "Hacking"             "Lethal firearms"    
# [16] "Melee"               "Methylone"           "Money"               "Non-lethal firearms" "Opioids"            
# [21] "Other"               "Pipes"               "Pirated"             "Prescription"        "Psychedelics"       
# [26] "RCs"                 "Smoked"              "Software"            "Steroids"            "Stimulants"         
# [31] "Watches"             "Weight loss" 

levels(fb$subsubcat)
# [1] "2C"         "5-MeO"      "Cocaine"    "DMT"        "Edibles"    "GBL"        "GHB"        "Hash"       "Ketamine"  
# [10] "LSD"        "MDA"        "MDMA"       "Mescaline"  "Meth"       "Mushrooms"  "MXE"        "NB"         "Other"     
# [19] "Others"     "Pills"      "Salvia"     "Speed"      "Spores"     "Synthetics" "Weed" 


# explore categories ---------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(vcd)

display.brewer.all()
ppal <- c("")

cat <- ggplot(p14, aes(x = cat, fill = cat)) + 
  geom_bar(stat = "count", color = "black") +
  scale_fill_manual(values = c("#EED5B7", "#8B1A1A"))

cat <- cat + theme_minimal() +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))

cat 

