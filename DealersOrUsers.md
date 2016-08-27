# Exploratory Cannabis Analysis

_unedited observations_

The question of personal use vs. distribution: Are there product groupings that could indicate one of these two intentions? What's the profile of a large-scale dealer, and what might their product interests be? 

Start off by taking a look at categorical levels. After identifying a wide range of potential variables, begin to subset from sub-subcategory back to top level category; and begin with products that are definitively marijuana and move out towards products that suggest possibilities.

- [Features of Interest](#features-of-interest)
- [Sub Sub Category Subsets](#sub-sub-category-subsets)
- [Sub Category Subsets](#sub-category-subsets)

# Features of Interest

``` {R}
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
```

# Sub Sub Category Subsets

# Weed
The first call is to 'Weed' in 'subsubcat': 

``` {R}
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
```

Distribution of quality measures seems to even out across the board - more than when looking at higher-level, broader categories. Confidence runs a nice span from min to max, as does lift. The quantiles of support appear low; the mean is above the 3rd quantile.

``` {R}
> arules::inspect(head(mj))
    lhs                              rhs              support     confidence lift    
17  {vendor=SeattlesBestCannabis} => {subsubcat=Weed} 0.001407544 0.9571984  6.754359
41  {vendor=laWnmoWermAn}         => {subsubcat=Weed} 0.001467622 0.9535316  6.728484
84  {vendor=Marleys}              => {subsubcat=Weed} 0.001501953 0.8793970  6.205362
204 {vendor=Rook}                 => {subsubcat=Weed} 0.001985438 0.9216467  6.503493
242 {vendor=zeltasgarden}         => {subsubcat=Weed} 0.002257220 0.9610231  6.781348
313 {vendor=Amsint}               => {subsubcat=Weed} 0.002294411 0.7544685  5.323819
```

Rules with two antecedents don't seem to say much - direct vendor to product relationship. It might be worth computing categories alone, without vendor.

```{R}
> arules::inspect(tail(mj))
  lhs                              rhs                  support confidence     lift
1 {vendor=moramaru,                                                                
   cat=Drugs,                                                                      
   subcat=Cannabis,                                                                
   greatFB}                     => {subsubcat=Weed} 0.002388820  0.8052073 5.681851
2 {vendor=BudBoss,                                                                 
   cat=Drugs,                                                                      
   subcat=Cannabis,                                                                
   greatFB}                     => {subsubcat=Weed} 0.003104035  1.0000000 7.056383
3 {vendor=the\_real\_caliconnect,                                                    
   cat=Drugs,                                                                      
   subcat=Cannabis,                                                                
   greatFB}                     => {subsubcat=Weed} 0.003115479  1.0000000 7.056383
4 {vendor=MrCronk,                                                                 
   cat=Drugs,                                                                      
   subcat=Cannabis,                                                                
   greatFB}                     => {subsubcat=Weed} 0.002712097  0.6245059 4.406753
5 {vendor=Drugs4you,                                                               
   cat=Drugs,                                                                      
   subcat=Cannabis,                                                                
   greatFB}                     => {subsubcat=Weed} 0.004205467  0.6869159 4.847142
6 {vendor=DrEarnhardt,                                                             
   cat=Drugs,                                                                      
   subcat=Cannabis,                                                                
   greatFB}                     => {subsubcat=Weed} 0.004806248  0.7547170 5.32557
```

'Weed' as slang for MJ has been a lingua franca term for some time - although slang terms cycle through phases and intervals. An [article on Slate](http://www.slate.com/blogs/lexicon_valley/2014/03/05/the_etymology_of_marijuana_and_the_rise_of_weed_as_the_preferred_slang_term.html) takes a look at the google n-gram viewer and compares terms by their usage over time - e.g. 'weed', 'grass', 'pot', 'dope', 'reefer'. All terms were searched with the verb 'smoke'. Reefer and Tea(?) never approach significant usage, perhaps being of a early 1900s. 'Grass' has a rally starting in the mid 1960s and peaking in the early 70s - followed by a long steady decline towards the irrelevance of 'Reefer'. 

'Pot' and 'Weed' both trend upward in usage from the 1990s-onward, with 'pot' cooling off at 2008.

Observation made by Slate regarding the New York Times approach to 'Weed':

'The New York Times is a bit more proper and allows weed in its news columns only in direct quotations, as in the Ronan Farrow case. Otherwise it sticks to marijuana, even in the face of extreme word repetition. A front-page article published last week, "Pivotal Point Is Seen on Legalizing Marijuana," uses marijuana 27 times (not including the headline) with the only variations being "the drug" and (once) "cannabis."'

Perhaps it remains in slang, but can be assumed to be universally understood.

# 8 Rules Each

- [Hash](#hash)
- [Edibles](#edibles)
- [Concentrates](#concentrates)

## Hash

We start out with a set of 8 rules here - lower number than expected, but with promising confidence and lift values.

``` {r}
> summary(hash)
# set of 8 rules

# rule length distribution (lhs + rhs):sizes
# 2 3 4 5 
# 1 3 3 1 

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     2.0     3.0     3.5     3.5     4.0     5.0 

summary of quality measures:
    support           confidence          lift      
 Min.   :0.001542   Min.   :0.9309   Min.   :31.45  
 1st Qu.:0.001542   1st Qu.:0.9310   1st Qu.:31.46  
 Median :0.001543   Median :0.9655   Median :32.62  
 Mean   :0.001543   Mean   :0.9655   Mean   :32.62  
 3rd Qu.:0.001545   3rd Qu.:1.0000   3rd Qu.:33.79  
 Max.   :0.001545   Max.   :1.0000   Max.   :33.79  

mining info:
 data ntransactions support confidence
   v2        349545  0.0014        0.6
```

Confidence drops no lower than 0.93 - and lift is in the 30s. But the quality measures are quite strong, likely because the associations aren't particularly surprising:

```{R}
> arules::inspect(hash)
     lhs                                                      rhs              support     confidence lift    
80   {vendor=theblossom}                                   => {subsubcat=Hash} 0.001544865 0.9310345  31.45853
760  {vendor=theblossom,subcat=Cannabis}                   => {subsubcat=Hash} 0.001544865 1.0000000  33.78879
762  {vendor=theblossom,cat=Drugs}                         => {subsubcat=Hash} 0.001544865 0.9310345  31.45853
764  {vendor=theblossom,greatFB}                           => {subsubcat=Hash} 0.001542005 0.9309154  31.45450
2162 {vendor=theblossom,cat=Drugs,subcat=Cannabis}         => {subsubcat=Hash} 0.001544865 1.0000000  33.78879
2165 {vendor=theblossom,subcat=Cannabis,greatFB}           => {subsubcat=Hash} 0.001542005 1.0000000  33.78879
2168 {vendor=theblossom,cat=Drugs,greatFB}                 => {subsubcat=Hash} 0.001542005 0.9309154  31.45450
3218 {vendor=theblossom,cat=Drugs,subcat=Cannabis,greatFB} => {subsubcat=Hash} 0.001542005 1.0000000  33.78879
```

Maybe the most surprising thing is that only _one_ vendor - over an entire year of listings - has hash.

## Edibles

This starts out strongly resembling `Hash` - 8 rules, high confidence and lift, exact same file size. And after inspecting the rules I can see why now: both `hash` and `edibles` appear to be sold by only one vendor. 'theblossom' covering hash and 'Lion' for edibles.

``` {R}
> summary(ed)
set of 8 rules

rule length distribution (lhs + rhs):sizes
2 3 4 5 
1 3 3 1 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    2.0     3.0     3.5     3.5     4.0     5.0 

summary of quality measures:
    support           confidence          lift      
 Min.   :0.002558   Min.   :0.6622   Min.   :47.04  
 1st Qu.:0.002558   1st Qu.:0.6683   1st Qu.:47.47  
 Median :0.002683   Median :0.6900   Median :49.01  
 Mean   :0.002683   Mean   :0.6899   Mean   :49.01  
 3rd Qu.:0.002809   3rd Qu.:0.7102   3rd Qu.:50.45  
 Max.   :0.002809   Max.   :0.7173   Max.   :50.95  

mining info:
 data ntransactions support confidence
   v2        349545  0.0014        0.6

arules::inspect(ed)
     lhs                                                rhs                 support     confidence lift    
412  {vendor=Lion}                                   => {subsubcat=Edibles} 0.002809366 0.6657627  47.28999
1502 {vendor=Lion,subcat=Cannabis}                   => {subsubcat=Edibles} 0.002809366 0.7173119  50.95159
1504 {vendor=Lion,cat=Drugs}                         => {subsubcat=Edibles} 0.002809366 0.6721424  47.74314
1506 {vendor=Lion,greatFB}                           => {subsubcat=Edibles} 0.002557611 0.6622222  47.03850
2709 {vendor=Lion,cat=Drugs,subcat=Cannabis}         => {subsubcat=Edibles} 0.002809366 0.7173119  50.95159
2712 {vendor=Lion,subcat=Cannabis,greatFB}           => {subsubcat=Edibles} 0.002557611 0.7078385  50.27868
2715 {vendor=Lion,cat=Drugs,greatFB}                 => {subsubcat=Edibles} 0.002557611 0.6691617  47.53142
3361 {vendor=Lion,cat=Drugs,subcat=Cannabis,greatFB} => {subsubcat=Edibles} 0.002557611 0.7078385  50.27868
```

## Concentrates

I'm surprised each generates 8 rules, and has one dedicated vendor associated per specific class of Cannabis. Further inspection of the rules and the data cleansing are in order.

``` {R}
> arules::inspect(concentrates)
     lhs                                                     rhs                      support     confidence lift    
246  {vendor=chipzahoy}                                   => {subsubcat=Concentrates} 0.002348768 1          42.35882
1093 {vendor=chipzahoy,subcat=Cannabis}                   => {subsubcat=Concentrates} 0.002348768 1          42.35882
1095 {vendor=chipzahoy,cat=Drugs}                         => {subsubcat=Concentrates} 0.002348768 1          42.35882
1097 {vendor=chipzahoy,greatFB}                           => {subsubcat=Concentrates} 0.002323020 1          42.35882
2380 {vendor=chipzahoy,cat=Drugs,subcat=Cannabis}         => {subsubcat=Concentrates} 0.002348768 1          42.35882
2383 {vendor=chipzahoy,subcat=Cannabis,greatFB}           => {subsubcat=Concentrates} 0.002323020 1          42.35882
2386 {vendor=chipzahoy,cat=Drugs,greatFB}                 => {subsubcat=Concentrates} 0.002323020 1          42.35882
3271 {vendor=chipzahoy,cat=Drugs,subcat=Cannabis,greatFB} => {subsubcat=Concentrates} 0.002323020 1          42.35882
```

# Zero Rule Club 

- [Synthetics](#synthetics)
- [Shake/trim](#shake-trim)
- [Seeds](#seeds)
- [Prescription](prescription)

## Synthetics

Category and subcategory are all the same value with Synthetics as the consequent: Drugs, Cannabis, Synthetics.
Variation might be found in price and shipping locations in this case. Product and feedback listing can be mined for amounts/weights offered, which then might lead to other points of interest.

``` {R}
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
```

Following the larger population, prices fall into the first and second factors moving right from zero.

## Shake/trim

Also no rules. Only 8 vendors out of 1868 even sell shake, and 4 countries(the largest ones) will ship it. 

``` {R}
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
```

## Seeds

``` {R}
seeds <- subset(v2rules, rhs %in% "subsubcat=Seeds")
seeds
# set of 0 rules

seedCheck <- subset(fb, fb$subsubcat == "Seeds")
# 1298 variables
unique(seedCheck$vendor)
# [1]  simonlabond          Cannaseed            budbrother           toysoldiers         
# [5]  PeterLustig          FreshTouch           HappyHippy2.0        superstrains        
# [9]  silverapples         Gobotal              Seeds_Co             Spacelab            
# [13] Renegade             Quantum.Black.Widow  My\_Private\_Garden  Charlie_Bartlett    
# [17] derriese5            StrainHunters        Warbeast             CaliforniaGreenCross
# [21] HangAbout            AngelEyes            klosterbier          danman              
# [25] Alexandrus           OzGrow               BenzoAU
```

27 vendors out of 1868 provide seeds - most names suggest an affinity with cannibus. Not hard to imagine what 'CaliforniaGreenCross', 'StrainHunters', 'Quantum.Black.Widow', 'budbrother', 'superstrains', 'Seeds_Co', &c. cater to. 

Which makes `BenzoAU` stand out. Appears this vendor offers `50+ CANNABIS SATIVA SEEDS` - one single listing. The most recent feedback begins:

		Feedbacks: 0/5 Scam artist. Here to take your bitcoin...

AU in this case does not refer to gold but rather Australia.

## Prescription

Turns out `Prescription` is this sense does not refer to medical marijuana.

``` {R}
scrip <- subset(v2rules, rhs %in% "subsubcat=Prescription")
scrip # set of 0 rules

scripCheck <- subset(fb, fb$subsubcat == "Prescription")
# 4932 observations - all stimulants
unique(scripCheck$product)
```

A look at the levels of `Prescription` shows 473 versions of Adderall, Dexedrine, Vyvanse, Ritalin, Modafanil, Provigil, and their generic counterparts. Excepting an occasional listing for Viagra/Cialis, a misplaced Codeine, and a single Lorazepam/Ativan - `Prescription` can more or less be synonymous with `Stimulants`. 

##### This is small ecosystem of substances that can be examined more closely in terms of quantities (e.g. milligrams, number of pills) using regex.


# Sub Categories - a level up
































