# Agora Marketplace Analysis
# Association Rules - 2014 Product Data
# Cannabis Analysis - SubSubCategories

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)
library(tm)

p14 <- fread("~/GitHub/agora-data/agora-2014.csv", stringsAsFactors = T)
str(p14)

p14$list <- as.character(p14$list)
p14$list <- removeNumbers(p14$list)
p14$list <- gsub("--__", "", p14$list)
p14$list <- gsub(".html", "", p14$list)
p14$list <- as.factor(p14$list)

p14$vendor <- gsub("%7E", "", p14$vendor)

p14$feedback <- as.character(p14$feedback)
p14$feedback <- stripWhitespace(p14$feedback)

levels(p14$from)




quantile(p14$price)

# subset out the placeholder (high) prices
p14 <- subset(p14, p14$price < 3500) # 772357
# I chopped that subset down pretty quick. A lot of vendors keep placeholder listings when they're
# out of stock - with outrageously high prices. (citation: Economist) Not sure who is going to pay for over $1M USD 
# for 0.1 grams of speed paste, but I feel OK to make a judgement call subsetting that out.  

# switched from 8 to 12 clusters - curious how the prices distribute.
# during the time period covered in the data, the lowest BTC-USD exchange rate
# might have just dipped below $200.
# But it also soared above $600 too. 
# There's a WSS plot in the plot directory that might back the decision for 12. 

p14$price <- discretize(p14$price, method = "cluster", categories = 12)
levels(p14$price)
#  8 clusters:
# [1] "[   0.0000001,   4.2529326)" "[   4.2529326,  18.0214855)" "[  18.0214855,  63.4096931)"
# [4] "[  63.4096931, 180.7799059)" "[ 180.7799059, 421.0974602)" "[ 421.0974602, 939.8462904)"
# [7] "[ 939.8462904,2088.7955777)" "[2088.7955777,3199.0000000]"

#  12 clusters:
#  [1] "[   0.0000001,   1.4851368)" "[   1.4851368,   4.8476172)" "[   4.8476172,  10.6721562)" "[  10.6721562,  19.9506919)"
#  [5] "[  19.9506919,  35.5722542)" "[  35.5722542,  63.6548653)" "[  63.6548653, 118.9146899)" "[ 118.9146899, 222.2199907)"
#  [9] "[ 222.2199907, 441.8217437)" "[ 441.8217437, 947.7948146)" "[ 947.7948146,2088.7955777)" "[2088.7955777,3199.0000000]"


# feedback table ------------------------------------------------------------

fb.table <- fread("~/GitHub/agora-data/vfb-table-2014.csv")

p14$feedback <- as.character(p14$feedback)
fb <- subset(p14, p14$feedback != " Feedbacks: No feedbacks found. ")
# 349545
fb$greatFB <- grepl("^\\sFeedbacks: 5/5(.*)", fb$feedback)
fb$goodFB <- grepl("^\\sFeedbacks: 4/5(.*)", fb$feedback)
fb$okFB <- grepl("^\\sFeedbacks: 3/5(.*)", fb$feedback)
fb$badFB <- grepl("^\\sFeedbacks: 2/5(.*)", fb$feedback)
fb$poorFB <- grepl("^\\sFeedbacks: 1/5(.*)", fb$feedback)
fb$worstFB <- grepl("^\\sFeedbacks: 0/5(.*)", fb$feedback)

length(fb$greatFB[fb$greatFB == TRUE]) # 332500
length(fb$greatFB[fb$greatFB == FALSE]) # 17045
332500/349545 #  0.9512366

length(fb$goodFB[fb$goodFB == TRUE]) # 4703
length(fb$goodFB[fb$goodFB == FALSE]) # 344842
4703/349545 #  0.01345463

length(fb$okFB[fb$okFB == TRUE]) # 2834
length(fb$okFB[fb$okFB == FALSE]) # 346711
2843/349545 # 0.008133431

length(fb$badFB[fb$badFB == TRUE]) # 1169
length(fb$badFB[fb$badFB == FALSE]) # 348376
1169/349545 # 0.003344348

length(fb$poorFB[fb$poorFB == TRUE]) # 1422
length(fb$poorFB[fb$poorFB == FALSE]) # 348123
1422/349545 # 0.004068146

length(fb$worstFB[fb$worstFB == TRUE]) # 6911
length(fb$worstFB[fb$worstFB == FALSE]) # 342634
6911/349545 # 0.01977142

fb.table <- data.frame(vendor = fb$vendor, great = fb$greatFB, good = fb$goodFB, 
                       ok = fb$okFB, bad = fb$badFB, poor = fb$poorFB, worst = fb$worstFB)

write.csv(fb.table, file = "~/GitHub/agora-data/vfb-table-2014.csv",
          row.names = F)

fb.table$vendor <- gsub("%7E", "", as.character(fb.table$vendor))
fb.table$vendor <- as.factor(fb.table$vendor)

# Vendor-Category-Feedback subset ---------------------------------------------

fb <- fread("~/GitHub/agora-data/feedback-2014.csv", stringsAsFactors = T)
v2 <- subset(fb, select = c("vendor", "cat", "subcat", "subsubcat", "greatFB", "worstFB"))

v2 <- as(v2, "transactions")
v2
# transactions in sparse format with
# 349545 transactions (rows) and
# 1968 items (columns)

summary(v2)
# density of 0.002525919

# most frequent items:
# greatFB       cat=Drugs    subsubcat=NA subcat=Cannabis  subsubcat=Weed         (Other) 
# 332500          259558          164720           75952           49536          855325 

# element (itemset/transaction) length distribution:
# sizes
# 4      5 
# 10134 339411

# LHS all have length of 4 or 5. this may need to change.

# VCF - Mine Frequent Itemsets ------------------------------------------------

nrow(v2)
500/nrow(v2) # 0.001430431
# 'find an interesting support (have at least 500 observations)'

v2items <- apriori(v2, parameter = list(target = "frequent", 
                                        supp = 0.0014, minlen = 2, maxlen = 4))

summary(v2items)
# set of 1889 itemsets

# greatFB  983
# Drugs    686
# sscat    516
# Cannabis 217
# Weed     124
# other    2839

# element (itemset/transaction) length distribution:sizes
# 2   3   4 
# 699 793 397

# mean: 2.84

# summary of quality measures:
# support        
# Min.   :0.001402  
# 1st Qu.:0.001759  
# Median :0.002526  
# Mean   :0.008199  
# 3rd Qu.:0.005321  
# Max.   :0.707331 

# mining info:
# data ntransactions support confidence
#   v2        349545  0.0014          1

par(mar = c(20, 6, 4, 2), family = "FranklinGothicSSK")
itemFrequencyPlot(v2, support = 0.005, cex.names = 0.75)
itemFrequencyPlot(v2, support = 0.0095, cex.names = 0.8)

par(mar = c(20, 6, 4, 2), family = "FranklinGothicSSK")
itemFrequencyPlot(v2, support = 0.0095, cex.names = 0.8,
                  main = "Agora 2014: Frequent Items (support = 0.0095)")

# Need to cleanse or format the NA subsubcategory. 
# Perhaps can break up the `Drug` Category after establishing
# some ground truths on the population. 

# VCF - Mine Association Rules ------------------------------------------------

# Going to start out here with the same measure value from the 
# frequent itemset mining (0.0014), and a confidence of 0.60.
# Confidence close to one is ideal, so 0.6 hopefully pushes 
# towards that with generous flexibility to start.

v2rules <- apriori(v2, parameter = list(support = 0.0014, confidence = 0.6))
v2rules
# set of 3492 rules 
summary(v2rules)

# try out various subsets
cannabis <- subset(v2rules, subset = rhs %in% "cat=Drugs" & lift > 1.2) # 709 rules
synthetics <- subset(v2rules, subset = rhs %in% "subcat=Cannabis" & lift > 1.2)
# 230 rules

summary(cannabis)
summary(synthetics)

arules::inspect(head(cannabis))
arules::inspect(head(synthetics))
arules::inspect(tail(synthetics))
arules::inspect(tail(cannabis))

# the Question of Distribution ------------------------------------------------

# this is somewhat like building a profile - subsetting for presumed 
# attributes of someone with intent to distribute rather than 
# consume personally.

# highlight features of interest - wide range
levels(fb$cat)
# "Counterfeits" - identities
# "Drug paraphernalia"
# "Drugs"
# "Forgeries" - identities
# "Information" - how to / guides
# "Electronics" - check for scales
# "Weapons"

levels(fb$subcat)
# "Accessories"
# "Advertising" 
# "Ammunition"
# "Cannabis"
# "Electronics"
# "Grinders"
# "Guides"
# "Other
# "Paper"
# "Lethal firearms"
# "Melee"
# "Money"
# "Non-lethal firearms"
# "Other
# "Paper"
# "Paraphernalia"
# "Physical documents"
# "Pipes"
# "Prescription"
# "Scales"
# "Scans/Photos"
# "Smoked"
# "Stashes"
# "eBooks"

levels(fb$subsubcat)
# "Concentrates"
# "Edibles"
# "Hash"
# "Other"
# "Others"
# "Prescription"
# "Seeds"
# "Shake/trim"
# "Synthetics"
# "Weed"

# marijuana-related Rule Subsets ----------------------------------------------

# Weed ----------------------------------------------------
mj <- subset(v2rules, subset = rhs %in% "subsubcat=Weed" & lift > 1.2) # 98 rules
summary(mj)
# set of 98 rules

# rule length distribution (lhs + rhs):sizes
# 2  3  4  5 
# 12 37 37 12 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.0     3.0     3.5     3.5     4.0     5.0 

# summary of quality measures:
#          support       confidence            lift      
# Min.   :0.001408   Min.   :0.6025   Min.   :4.252  
# 1st Qu.:0.001968   1st Qu.:0.7088   1st Qu.:5.002  
# Median :0.002294   Median :0.9529   Median :6.724  
# Mean   :0.008015   Mean   :0.8513   Mean   :6.007  
# 3rd Qu.:0.003104   3rd Qu.:0.9663   3rd Qu.:6.819  
# Max.   :0.141716   Max.   :1.0000   Max.   :7.056  

# mining info:
#   data ntransactions support confidence
#     v2        349545  0.0014        0.6

arules::inspect(head(mj))
arules::inspect(tail(mj))

# Synthetics ----------------------------------------------
synth <- subset(v2rules, subset = rhs %in% "subsubcat=Synthetics")
synth
# set of 0 rules

synthCheck <- subset(fb, (fb$subsubcat == "Synthetics"))
# 1356 obs.

levels(fb$price)
levels(synthCheck$price)

table(synthCheck$price)
# [1.00e-07,1.49e+00) [1.07e+01,2.00e+01) [1.19e+02,2.22e+02) [1.49e+00,4.85e+00) [2.00e+01,3.56e+01) [2.09e+03,3.20e+03] [2.22e+02,4.42e+02) [3.56e+01,6.37e+01) 
# 1273                   0                   0                  62                   0                   0                   0                   0 
# [4.42e+02,9.48e+02) [4.85e+00,1.07e+01) [6.37e+01,1.19e+02) [9.48e+02,2.09e+03) 
# 0                  21                   0                   0 

table(fb$price)
# [1.00e-07,1.49e+00) [1.07e+01,2.00e+01) [1.19e+02,2.22e+02) [1.49e+00,4.85e+00) [2.00e+01,3.56e+01) [2.09e+03,3.20e+03] [2.22e+02,4.42e+02) [3.56e+01,6.37e+01) 
# 316365                1487                  79               24034                 644                  21                 118                 130 
# [4.42e+02,9.48e+02) [4.85e+00,1.07e+01) [6.37e+01,1.19e+02) [9.48e+02,2.09e+03) 
# 56                6457                  95                  59


unique(synthCheck$vendor)
#  [1] SusnaSmith           Heisenbergmontana    Charlie_Bartlett     axa                 
# [5] Illegal_Entrepreneur DoctorNick           nawlins              b1g1mpact           
# [9] CarlBildt            EZTest               aussieherbalsmoke    FerrariBlack1       
#  [13] lamachine            MrSunshine           ImportDirect         USAXpress           
# [17] drzheng              SatoshiShop          alchemycd            FREE                
# [21] TheAvatarSpirit

unique(synthCheck$from)
unique(synthCheck$product)

# Shake/trim ----------------------------------------------

shake <- subset(v2rules, rhs %in% "subsubcat=Shake/trim")
shake
# set of 0 rules

shakeCheck <- subset(fb, fb$subsubcat == "Shake/trim")
# 244 obs.

unique(shakeCheck$vendor)
# [1] GucciBuds   DrEarnhardt drugsRus    GrowerLove  
#     GreenStreet ModernLove  DankBoss101 krautling

unique(shakeCheck$from)
# [1]  USA           Netherlands   UK           <NA>           Germany

# Seeds ---------------------------------------------------

seeds <- subset(v2rules, rhs %in% "subsubcat=Seeds")
seeds
# set of 0 rules

seedCheck <- subset(fb, fb$subsubcat == "Seeds")
# 1298 variables
unique(seedCheck$vendor)
#  [1] simonlabond          Cannaseed            budbrother           toysoldiers         
# [5] PeterLustig          FreshTouch           HappyHippy2.0        superstrains        
# [9] silverapples         Gobotal              Seeds_Co             Spacelab            
# [13] Renegade             Quantum.Black.Widow  My_Private_Garden    Charlie_Bartlett    
# [17] derriese5            StrainHunters        Warbeast             CaliforniaGreenCross
# [21] HangAbout            AngelEyes            klosterbier          danman              
# [25] Alexandrus           OzGrow               BenzoAU

# Prescription --------------------------------------------

scrip <- subset(v2rules, rhs %in% "subsubcat=Prescription")
scrip # set of 0 rules

scripCheck <- subset(fb, fb$subsubcat == "Prescription")
# 4932 observations - all stimulants
unique(scripCheck$product)
# 473 versions of Adderall, Dexedrine, Vyvanse, Ritalin, Modafanil, Provigil
# Occasional Viagra and Cialis. 
# A misplaced Codeine, Lorazepam/Ativan.
# Could regex out some details later.

# "Hash" --------------------------------------------------

hash <- subset(v2rules, rhs %in% "subsubcat=Hash")
hash # set of 8 rules
summary(hash)

arules::inspect(hash)


# "Edibles" -----------------------------------------------

ed <- subset(v2rules, rhs %in% "subsubcat=Edibles")
summary(ed)
# set of 8 rules

# rule length distribution (lhs + rhs):sizes
# 2 3 4 5 
# 1 3 3 1 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.0     3.0     3.5     3.5     4.0     5.0 

# summary of quality measures:
#   support           confidence          lift      
# Min.   :0.002558   Min.   :0.6622   Min.   :47.04  
# 1st Qu.:0.002558   1st Qu.:0.6683   1st Qu.:47.47  
# Median :0.002683   Median :0.6900   Median :49.01  
# Mean   :0.002683   Mean   :0.6899   Mean   :49.01  
# 3rd Qu.:0.002809   3rd Qu.:0.7102   3rd Qu.:50.45  
# Max.   :0.002809   Max.   :0.7173   Max.   :50.95  

# mining info:
#   data ntransactions support confidence
# v2        349545  0.0014        0.6

arules::inspect(ed)
#      lhs                                                rhs                 support     confidence lift    
# 412  {vendor=Lion}                                   => {subsubcat=Edibles} 0.002809366 0.6657627  47.28999
# 1502 {vendor=Lion,subcat=Cannabis}                   => {subsubcat=Edibles} 0.002809366 0.7173119  50.95159
# 1504 {vendor=Lion,cat=Drugs}                         => {subsubcat=Edibles} 0.002809366 0.6721424  47.74314
# 1506 {vendor=Lion,greatFB}                           => {subsubcat=Edibles} 0.002557611 0.6622222  47.03850
# 2709 {vendor=Lion,cat=Drugs,subcat=Cannabis}         => {subsubcat=Edibles} 0.002809366 0.7173119  50.95159
# 2712 {vendor=Lion,subcat=Cannabis,greatFB}           => {subsubcat=Edibles} 0.002557611 0.7078385  50.27868
# 2715 {vendor=Lion,cat=Drugs,greatFB}                 => {subsubcat=Edibles} 0.002557611 0.6691617  47.53142
# 3361 {vendor=Lion,cat=Drugs,subcat=Cannabis,greatFB} => {subsubcat=Edibles} 0.002557611 0.7078385  50.27868

# "Concentrates" ------------------------------------------

concentrates <- subset(v2rules, rhs %in% "subsubcat=Concentrates")
summary(concentrates)
# set of 8 rules

# rule length distribution (lhs + rhs):sizes
# 2 3 4 5 
# 1 3 3 1 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.0     3.0     3.5     3.5     4.0     5.0 

# summary of quality measures:
#   support           confidence      lift      
# Min.   :0.002323   Min.   :1    Min.   :42.36  
# 1st Qu.:0.002323   1st Qu.:1    1st Qu.:42.36  
# Median :0.002336   Median :1    Median :42.36  
# Mean   :0.002336   Mean   :1    Mean   :42.36  
# 3rd Qu.:0.002349   3rd Qu.:1    3rd Qu.:42.36  
# Max.   :0.002349   Max.   :1    Max.   :42.36  

# mining info:
#   data ntransactions support confidence
#     v2        349545  0.0014        0.6

arules::inspect(concentrates)
#      lhs                                                     rhs                      support     confidence lift    
# 246  {vendor=chipzahoy}                                   => {subsubcat=Concentrates} 0.002348768 1          42.35882
# 1093 {vendor=chipzahoy,subcat=Cannabis}                   => {subsubcat=Concentrates} 0.002348768 1          42.35882
# 1095 {vendor=chipzahoy,cat=Drugs}                         => {subsubcat=Concentrates} 0.002348768 1          42.35882
# 1097 {vendor=chipzahoy,greatFB}                           => {subsubcat=Concentrates} 0.002323020 1          42.35882
# 2380 {vendor=chipzahoy,cat=Drugs,subcat=Cannabis}         => {subsubcat=Concentrates} 0.002348768 1          42.35882
# 2383 {vendor=chipzahoy,subcat=Cannabis,greatFB}           => {subsubcat=Concentrates} 0.002323020 1          42.35882
# 2386 {vendor=chipzahoy,cat=Drugs,greatFB}                 => {subsubcat=Concentrates} 0.002323020 1          42.35882
# 3271 {vendor=chipzahoy,cat=Drugs,subcat=Cannabis,greatFB} => {subsubcat=Concentrates} 0.002323020 1          42.35882



