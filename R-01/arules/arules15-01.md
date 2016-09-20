# Association Rules - 2015 Data (listings < $2,000)

file: `arules-2015.R`

Load it up:

``` {r}
library(arules)
library(arulesViz)
library(data.table)
library(qdap)
```

I might use `qdap` to get some word stats on the product listings - and from there find a way to reduce them to just the products.

But first to trim the outlier high price listings.

```{r}
quantile(v15$usd)
#          0%                25%                50%                75%               100% 
# 0.000017728       20.184994602       80.164550000      267.745375000 61413750.000000000 

summary(v15$usd)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#        0       20       80     1490      268 61410000
{r}

Seeing a mean of $1490, and price distribution up to the 75th percentile at $267.75. Inclined to believe that most of the listings are under $1,000, but just to be sure:

```{r}
plot(v15$usd)
```
![not so scattered plot](http://pi.mozzarella.website/ag/usd-all.jpeg)
3 listings above 20,000,000. Cut it down to 10 million and plot.

```{r}
v15 <- subset(v15, v15$usd < 10000000)  # 1133664
v15 <- subset(v15, v15$usd <= 1000000)  # 1133312
```

Total difference of 352 listings.

``` {r}
v15 <- subset(v15, v15$usd <= 429972.2) # 1133196
```

116 less. The price is chosen because of the listing from vendor HonestCocaine. He offers 10 kg cocaine for that price, which is somewhat reasonable @ ~43k/kilo. Although at that scale there should be a bulk deal. Of course, these listings may just be a 'presence' - vendor not expected anyone to purchase outright, but rather to get the 'right' person's attention...

Under $100k didn't look any more promising than under $10 million as a subset. I'm deciding to keep one subset at under $50k and work on another at under $2000 for now.

```{r}
plot(v15b$usd)
# OK. Stick with under 50k as one subset and do another at under 2k.
```

The plot is almost starting to make sense. But onto subsetting under 2k and discretizing those prices. 

``` {r}
v15c <- subset(v15, v15$usd <= 2000)  # 1080082
v15c$usd <- discretize(v15c$usd, method = "cluster", categories = 10)
levels(v15c$usd)

#  10 clusters:
 [1] "[   0.0000177,  46.0683773)" "[  46.0683773, 117.3368770)" "[ 117.3368770, 212.1022398)"
 [4] "[ 212.1022398, 335.7857443)" "[ 335.7857443, 490.6833477)" "[ 490.6833477, 690.1482268)"
 [7] "[ 690.1482268, 939.4003723)" "[ 939.4003723,1239.5701318)" "[1239.5701318,1598.9965098)"
[10] "[1598.9965098,2000.0000000]"
```
That might be a lot of bins for cash - but hope that it leads to interesting rules. Also worth trying would be 4-5 categories on the next discretization.

# Transactions

```{r}
v2 <- subset(v15c, select = c("vendor", "allcat", "product", "from"))
summary(v2)
#          vendor                           allcat      
#  captainkirk: 39301   Drugs, Cannabis, Weed  :103593  
#  Optumis    : 32327   Drugs, Prescription, NA: 98618  
#  rc4me      : 29757   Drugs, Benzos, NA      : 67481  
#  RXChemist  : 28073   Info/eBooks, Other, NA : 63347  
#  fake       : 27485   Drugs, Steroids, NA    : 52972  
#  optiman    : 25178   Drugs, Ecstasy, MDMA   : 38633  
# (Other)    :897961   (Other)                :655438  
#                                                                           product       
# custom hotel booking                                                     :    463  
# VPN > TOR > SOCK TUTORIAL                                                :    461  
# Amazon refunds/Ddips - 20% - 30%                                         :    457  
# 1# Spot For Trips Connoisseurs NBOMe $2 ONLY $2                          :    423  
# Ultra Low Dose Naltrexone // Enhances Opiate Euphoria, Reduces Tolerance :    386  
# Codeine                                                                  :    367  
# (Other)                                                                  :1077525  

#        from       
# USA      :241898  
# No Info  :210997  
# UK       :100129  
# China    : 80884  
# Australia: 71123  
# EU       : 56560  
# (Other)  :318491 
```

Breakdown by factor counts: 

- `vendor` 1960 levels
- `allcat` 101 levels (all categories)
- `product` 61625 levels (out of 1080082 observations)
- `from` 66 levels

Converting the data frame to transactions:

```{r}
transactions as itemMatrix in sparse format with
 1080082 rows (elements/itemsets/transactions) and
 63752 columns (items) and a density of 0.00006274313 

most frequent items:
                    location=USA                 location=No Info   category=Drugs, Cannabis, Weed 
                          241898                           210997                           103593 
                     location=UK category=Drugs, Prescription, NA                          (Other) 
                          100129                            98618                          3565093 

element (itemset/transaction) length distribution:
sizes
      4 
1080082 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      4       4       4       4       4       4 

includes extended item information - examples:
             labels variables     levels
1 vendor=-Euphoria-    vendor -Euphoria-
2     vendor=-FliP-    vendor     -FliP-
3   vendor=-Inanna-    vendor   -Inanna-

includes extended transaction information - examples:
  transactionID
1             1
2             2
3             3
```

# Apriori - Frequent Itemsets

```{r}
v2items <- apriori(v2, parameter = list(target = "frequent", 
                                        supp = 0.000925, minlen = 2, maxlen = 4))

Parameter specification:
 confidence minval smax arem  aval originalSupport  support minlen maxlen            target   ext
         NA    0.1    1 none FALSE            TRUE 0.000925      2      4 frequent itemsets FALSE

Algorithmic control:
 filter tree heap memopt load sort verbose
    0.1 TRUE TRUE  FALSE TRUE    2    TRUE

Absolute minimum support count: 999 
```

Interesting what shows up as most frequent itemsets - was expecting Cannabis but see Benzodiazapines...
The itemset length distribution looks promising.

```{r}
set of 723 itemsets

# most frequent items:
# location=USA  location=No Info    location=UK   location=China  category=Drugs, Benzos, NA    (Other) 
# 116           89                  58            47              42                            1244 

element (itemset/transaction) length distribution:sizes
  2   3 
573 150 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   2.000   2.207   2.000   3.000 

summary of quality measures:
    support         
 Min.   :0.0009259  
 1st Qu.:0.0011893  
 Median :0.0016304  
 Mean   :0.0030600  
 3rd Qu.:0.0030863  
 Max.   :0.0433986  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions  support confidence
   v2       1080082 0.000925          1
```   

After taking a look at itemFrequencyPlots with varying supports, decided on a wider miniumum support of 0.0095 and starting confidence of 0.60. 
These parameters pruned the itemsets to a list of 33 rules - can't say there's too many.

``` {r}
v2rules <- apriori(v2, parameter = list(support = 0.0095, confidence = 0.6))
v2rules
# set of 33 rules
summary(v2rules)
set of 33 rules

rule length distribution (lhs + rhs):sizes
 2  3 
22 11 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   2.000   2.333   3.000   3.000 

summary of quality measures:
    support           confidence          lift       
 Min.   :0.009625   Min.   :0.6197   Min.   : 2.767  
 1st Qu.:0.012816   1st Qu.:0.7376   1st Qu.: 4.577  
 Median :0.016324   Median :0.8799   Median : 8.709  
 Mean   :0.019077   Mean   :0.8513   Mean   :13.207  
 3rd Qu.:0.024324   3rd Qu.:0.9730   3rd Qu.:14.334  
 Max.   :0.036858   Max.   :1.0000   Max.   :57.706  

mining info:
 data ntransactions support confidence
   v2       1080082  0.0095        0.6
```


The rules:

```{r}
> arules::inspect(v2rules)
   lhs                                         rhs                                      support confidence      lift
1  {category=Drugs, Cannabis, Edibles}      => {location=USA}                       0.009689079  0.8610334  3.844541
2  {vendor=TheDigital}                      => {location=No Info}                   0.012186112  0.8798716  4.504014
3  {vendor=etimbuk}                         => {location=No Info}                   0.014110966  0.9962740  5.099872
4  {category=Drugs, Cannabis, Synthetics}   => {location=China}                     0.010492722  0.7324371  9.780577
5  {vendor=Gotmilk}                         => {category=Drugs, Prescription, NA}   0.012391652  0.7448798  8.158057
6  {vendor=Gotmilk}                         => {location=No Info}                   0.012637003  0.7596282  3.888495
7  {vendor=sexyhomer}                       => {category=Counterfeits, Watches, NA} 0.016323761  0.9419779 37.224256
8  {category=Counterfeits, Watches, NA}     => {vendor=sexyhomer}                   0.016323761  0.6450681 37.224256
9  {vendor=sexyhomer}                       => {location=China}                     0.016487637  0.9514345 12.704952
10 {category=Info/eBooks, Making money, NA} => {location=No Info}                   0.012815694  0.7375713  3.775587
11 {vendor=optiman}                         => {location=USA}                       0.014447051  0.6197474  2.767191
12 {category=Drugs, Cannabis, Concentrates} => {location=USA}                       0.016711694  0.7043353  3.144879
13 {category=Counterfeits, Watches, NA}     => {location=China}                     0.016145996  0.6380433  8.520092
14 {vendor=fake}                            => {category=Info/eBooks, Other, NA}    0.019894786  0.7818083 13.330024
15 {vendor=fake}                            => {location=No Info}                   0.022751050  0.8940513  4.576599
16 {vendor=RXChemist}                       => {category=Drugs, Prescription, NA}   0.025290672  0.9730346 10.656849
17 {vendor=RXChemist}                       => {location=No Info}                   0.025022174  0.9627044  4.928031
18 {vendor=rc4me}                           => {location=China}                     0.027550686  1.0000000 13.353469
19 {vendor=Optumis}                         => {location=No Info}                   0.029259815  0.9776039  5.004300
20 {vendor=captainkirk}                     => {location=Internet}                  0.036387052  1.0000000 19.251426
21 {location=Internet}                      => {vendor=captainkirk}                 0.036387052  0.7005026 19.251426
22 {category=Info/eBooks, Other, NA}        => {location=No Info}                   0.036858313  0.6284433  3.216967
23 {vendor=Gotmilk,                                                                                                 
    category=Drugs, Prescription, NA}       => {location=No Info}                   0.010049237  0.8109683  4.151302
24 {vendor=Gotmilk,                                                                                                 
    location=No Info}                       => {category=Drugs, Prescription, NA}   0.010049237  0.7952231  8.709426
25 {vendor=sexyhomer,                                                                                               
    category=Counterfeits, Watches, NA}     => {location=China}                     0.016145996  0.9891101 13.208051
26 {vendor=sexyhomer,                                                                                               
    location=China}                         => {category=Counterfeits, Watches, NA} 0.016145996  0.9792790 38.698288
27 {category=Counterfeits, Watches, NA,                                                                             
    location=China}                         => {vendor=sexyhomer}                   0.016145996  1.0000000 57.705936
28 {vendor=fake,                                                                                                    
    category=Info/eBooks, Other, NA}        => {location=No Info}                   0.019126326  0.9613738  4.921219
29 {vendor=fake,                                                                                                    
    location=No Info}                       => {category=Info/eBooks, Other, NA}    0.019126326  0.8406788 14.333781
30 {vendor=RXChemist,                                                                                               
    category=Drugs, Prescription, NA}       => {location=No Info}                   0.024324079  0.9617806  4.923302
31 {vendor=RXChemist,                                                                                               
    location=No Info}                       => {category=Drugs, Prescription, NA}   0.024324079  0.9721009 10.646624
32 {category=Drugs, Prescription, NA,                                                                               
    location=No Info}                       => {vendor=RXChemist}                   0.024324079  0.6517166 25.074177
33 {vendor=captainkirk,                                                                                             
    category=Info/eBooks, Other, NA}        => {location=Internet}                  0.009625195  1.0000000 19.251426
```

It'd be nice to aggregate these vendors together. A look at the rules sorted by lift:

```{r}
> arules::inspect(v2rules, by = "lift")
   lhs                                                      rhs                                  support     confidence lift     
1  {category=Drugs, Cannabis, Edibles}                   => {location=USA}                       0.009689079 0.8610334   3.844541
2  {vendor=TheDigital}                                   => {location=No Info}                   0.012186112 0.8798716   4.504014
3  {vendor=etimbuk}                                      => {location=No Info}                   0.014110966 0.9962740   5.099872
4  {category=Drugs, Cannabis, Synthetics}                => {location=China}                     0.010492722 0.7324371   9.780577
5  {vendor=Gotmilk}                                      => {category=Drugs, Prescription, NA}   0.012391652 0.7448798   8.158057
6  {vendor=Gotmilk}                                      => {location=No Info}                   0.012637003 0.7596282   3.888495
7  {vendor=sexyhomer}                                    => {category=Counterfeits, Watches, NA} 0.016323761 0.9419779  37.224256
8  {category=Counterfeits, Watches, NA}                  => {vendor=sexyhomer}                   0.016323761 0.6450681  37.224256
9  {vendor=sexyhomer}                                    => {location=China}                     0.016487637 0.9514345  12.704952
10 {category=Info/eBooks, Making money, NA}              => {location=No Info}                   0.012815694 0.7375713   3.775587
11 {vendor=optiman}                                      => {location=USA}                       0.014447051 0.6197474   2.767191
12 {category=Drugs, Cannabis, Concentrates}              => {location=USA}                       0.016711694 0.7043353   3.144879
13 {category=Counterfeits, Watches, NA}                  => {location=China}                     0.016145996 0.6380433   8.520092
14 {vendor=fake}                                         => {category=Info/eBooks, Other, NA}    0.019894786 0.7818083  13.330024
15 {vendor=fake}                                         => {location=No Info}                   0.022751050 0.8940513   4.576599
16 {vendor=RXChemist}                                    => {category=Drugs, Prescription, NA}   0.025290672 0.9730346  10.656849
17 {vendor=RXChemist}                                    => {location=No Info}                   0.025022174 0.9627044   4.928031
18 {vendor=rc4me}                                        => {location=China}                     0.027550686 1.0000000  13.353469
19 {vendor=Optumis}                                      => {location=No Info}                   0.029259815 0.9776039   5.004300
20 {vendor=captainkirk}                                  => {location=Internet}                  0.036387052 1.0000000  19.251426
21 {location=Internet}                                   => {vendor=captainkirk}                 0.036387052 0.7005026  19.251426
22 {category=Info/eBooks, Other, NA}                     => {location=No Info}                   0.036858313 0.6284433   3.216967
23 {vendor=Gotmilk,category=Drugs, Prescription, NA}     => {location=No Info}                   0.010049237 0.8109683   4.151302
24 {vendor=Gotmilk,location=No Info}                     => {category=Drugs, Prescription, NA}   0.010049237 0.7952231   8.709426
25 {vendor=sexyhomer,category=Counterfeits, Watches, NA} => {location=China}                     0.016145996 0.9891101  13.208051
26 {vendor=sexyhomer,location=China}                     => {category=Counterfeits, Watches, NA} 0.016145996 0.9792790  38.698288
27 {category=Counterfeits, Watches, NA,location=China}   => {vendor=sexyhomer}                   0.016145996 1.0000000  57.705936
28 {vendor=fake,category=Info/eBooks, Other, NA}         => {location=No Info}                   0.019126326 0.9613738   4.921219
29 {vendor=fake,location=No Info}                        => {category=Info/eBooks, Other, NA}    0.019126326 0.8406788  14.333781
30 {vendor=RXChemist,category=Drugs, Prescription, NA}   => {location=No Info}                   0.024324079 0.9617806   4.923302
31 {vendor=RXChemist,location=No Info}                   => {category=Drugs, Prescription, NA}   0.024324079 0.9721009  10.646624
32 {category=Drugs, Prescription, NA,location=No Info}   => {vendor=RXChemist}                   0.024324079 0.6517166  25.074177
33 {vendor=captainkirk,category=Info/eBooks, Other, NA}  => {location=Internet}                  0.009625195 1.0000000  19.251426
```

# New Parameters

I decreased the support to 0.00095 to see more rules. 

```{r}
set of 532 rules

rule length distribution (lhs + rhs):sizes
  2   3 
276 256 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   2.000   2.481   3.000   3.000 

summary of quality measures:
    support            confidence          lift        
 Min.   :0.0009509   Min.   :0.6086   Min.   :  2.756  
 1st Qu.:0.0012073   1st Qu.:0.8413   1st Qu.:  6.724  
 Median :0.0016008   Median :0.9664   Median : 15.186  
 Mean   :0.0031882   Mean   :0.9050   Mean   : 43.525  
 3rd Qu.:0.0028259   3rd Qu.:1.0000   3rd Qu.: 29.520  
 Max.   :0.0368583   Max.   :1.0000   Max.   :762.770  

mining info:
 data ntransactions support confidence
   v2       1080082 0.00095        0.6
```

the top 30 by lift:
```{r}
> arules::inspect(head(sort(v3rules, by = "lift"), 30))
    lhs                                                         rhs                                   support      confidence lift    
294 {category=Tobacco, Smoked, NA,location=Torland}          => {vendor=mikehamer}                    0.0012795325 1.0000000  762.7698
287 {category=Tobacco, Smoked, NA,location=EU}               => {vendor=ShopAgent}                    0.0011378766 0.9199102  743.6964
307 {category=Drugs, Steroids, NA,location=Denmark}          => {vendor=diamonddweller}               0.0012702739 1.0000000  666.3060
71  {location=Philippines}                                   => {vendor=Pharma-deal}                  0.0013610078 0.8081363  592.9711
70  {vendor=Pharma-deal}                                     => {location=Philippines}                0.0013610078 0.9986413  592.9711
320 {category=Drugs, Prescription, NA,location=Worldwide}    => {vendor=New_demension}                0.0011869469 0.7372053  444.0838
310 {category=Data, Software, NA,location=Torland}           => {vendor=sereal}                       0.0013369355 0.6501576  425.3322
317 {category=Drugs, Benzos, NA,location=Denmark}            => {vendor=Heisenberg-meds}              0.0014147074 0.6663759  406.8630
361 {vendor=MrCronk,category=Drugs, Cannabis, Weed}          => {location=Undeclared}                 0.0015804356 0.8513716  393.1386
360 {category=Drugs, Cannabis, Weed,location=Undeclared}     => {vendor=MrCronk}                      0.0015804356 1.0000000  380.5786
165 {location=Undeclared}                                    => {vendor=MrCronk}                      0.0021433558 0.9897392  376.6735
166 {vendor=MrCronk}                                         => {location=Undeclared}                 0.0021433558 0.8157153  376.6735
33  {vendor=kriminale2}                                      => {location=Italy}                      0.0011684298 1.0000000  372.4421
350 {category=Drugs, Psychedelics, Spores,location=EU}       => {vendor=klosterbier}                  0.0010813994 0.9189614  298.1537
190 {vendor=b1g1mpact}                                       => {location=Poland}                     0.0026099870 0.9122977  287.7793
191 {location=Poland}                                        => {vendor=b1g1mpact}                    0.0026099870 0.8233061  287.7793
388 {category=Drugs, Cannabis, Seeds,location=EU}            => {vendor=klosterbier}                  0.0017878272 0.8461876  274.5425
334 {category=Tobacco, Paraphernalia, NA,location=USA}       => {vendor=ES-Light}                     0.0012526827 0.7848028  270.5558
370 {category=Other, NA, NA,location=UK}                     => {vendor=PlutoPete}                    0.0009628899 0.6287787  269.6040
379 {category=Drugs, Cannabis, Concentrates,location=Canada} => {vendor=chipzahoy}                    0.0026062836 0.7159207  259.5680
404 {category=Tobacco, Smoked, NA,location=UK}               => {vendor=only}                         0.0035238065 0.9142445  246.9265
57  {vendor=europedrugs}                                     => {location=France}                     0.0012628671 0.9722024  246.0882
136 {category=Tobacco, Paraphernalia, NA}                    => {vendor=ES-Light}                     0.0012526827 0.6785356  233.9209
109 {vendor=Heisenberg-meds}                                 => {location=Denmark}                    0.0016378386 1.0000000  228.1542
316 {vendor=Heisenberg-meds,category=Drugs, Benzos, NA}      => {location=Denmark}                    0.0014147074 1.0000000  228.1542
88  {vendor=diamonddweller}                                  => {location=Denmark}                    0.0014619260 0.9740901  222.2427
306 {vendor=diamonddweller,category=Drugs, Steroids, NA}     => {location=Denmark}                    0.0012702739 0.9702970  221.3773
391 {category=Drugs, Ecstasy, MDMA,location=China}           => {vendor=alchemycd}                    0.0017535706 0.6963235  221.1369
418 {category=Electronics, NA, NA,location=Torland}          => {vendor=smart666tiger}                0.0014526675 0.8394864  193.8666
113 {vendor=meowtime}                                        => {category=Counterfeits, Clothing, NA} 0.0016637626 1.0000000  177.1497
```





