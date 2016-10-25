# Agora Associations 05

R script: [**_agora-associations-05.R_**](R/arules/agora-associations-05.R)


- [Variable Selection](#variable-selection)
- [Transaction Conversion](#transaction-conversion)
- [Item Frequency / Frequent Itemsets](#item-frequency)
- [Mining Association Rules](#mining-association-rules)
- [Vis: Grouped Matrices](#visualizations---grouped-matrices)
- [Vis: Network Graphs](#visualizations---network-graphs)


# Variable Selection

dataframe: **ag**

2317353 observations of 4 variables:

| name  | variable      | levels    |   notes                                       |
|-------|---------------|-----------|-----------------------------------------------|
| `p`   | price         | 10        | discretized into 10 bins                      |
| `f`   | from          | 85        | product origin location                       |
| `sc`  | subcategory   | 105       | subcategory as labeled on Agora               |
| `v`   | vendor        | 3183      | anonymized with SHA256 hashing algorithm      |

Price `p` ranged from $0-20,000 and was discretized manually into 10 bins: 

- $0-10
- $10-50
- $50-100
- $100-200
- $200-600
- $600-1200
- $1200-2000
- $2000-5000
- $5000-10000
- $10000-20000

While not in equal intervals, these bins reflect the distribution of prices on the market and take into account a large number of listings under 10- and 200- USD.

Although vendors did not use given names, there have been cases where even a vendor's online name could be used to identify them<sup>[1](#references-and-notes)</sup>. To avoid this implication, names were run through the function `anonymize`. This salted and then hashed the names using SHA256, and from there names were abbreviated for clarity. While likely not following the strictest security protocol, this level of anonymization felt suited for the application. In practical terms - all of this data is publicly available, so these measures were taken out of a careful respect for privacy.

```{r}
ag <- subset(ag, select = c("p", "from", "all.c", "v3"))
colnames(ag) <- c("p", "f", "c", "v")
head(ag)
              p       f                  c      v
1 $10000-$20000   China              Other e63948
2       $10-$50 No Info             Guides 189622
3       $10-$50 No Info             Guides 447418
4   $1200-$2000   China                RCs a00543
5       $10-$50     USA Stimulants-Cocaine e99113
6       $10-$50 No Info             Guides 189622
```

# Transaction Conversion


```{R}
a5 <- as(ag, "transactions")
```

```{R}
summary(a5)
transactions as itemMatrix in sparse format with
 2317353 rows (elements/itemsets/transactions) and
 3383 columns (items) and a density of 0.001182383 

most frequent items:
  p=$10-$50       f=USA   f=No Info p=$200-$600  p=$50-$100     (Other) 
     510361      497780      407122      385562      376526     7092061 

element (itemset/transaction) length distribution:
sizes
      4 
2317353 

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       4       4       4       4       4       4 

includes extended item information - examples:
      labels variables   levels
1   p=$0-$10         p   $0-$10
2  p=$10-$50         p  $10-$50
3 p=$50-$100         p $50-$100

includes extended transaction information - examples:
  transactionID
1             1
2             2
3             3
```

So showing up most frequently are the location 'USA' and 'No Info', and three price bins: $10-$50, $50-$100, and $200-$600. Prices from $0-$10 and $100-$200 are not as freqeuent...

# Item Frequency

``` {R}
# Item Frequency Plot ---------------------------------------------------------

par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
itemFrequencyPlot(a5, support = 0.005, cex.names = 0.70, col = "white", horiz = T,
                  main = "Agora Marketplace: Frequent Items (support > 0.005)")
```

![itemFreq-0.005](plots/arules-a5/a5u-ItemFreq-005.png)

quick observations at 0.005 minimum support:

- somewhat competetive balance of frequencies
- observations at the lower frequencies are interesting with this much detail/grain
- affinities in frequency, (~0.015-0.020): speed, meth, pirated, money, worldwide, India, Belgium
- affinities in frequency, (~0.025): 3 vendors, RCs, hacking, hash, eBooks, watches, concentrates
- 3 vendors appear just as frequently as all listings for RCs

Moving forward - wanted to see how different minimum supports affected item frequency, so scripted a loop to plot a sequence of different values.

```{R}
# Item Frequency Plot - Loop --------------------------------------------------

# define support intervals
sup <- seq(0.000, 0.1, by = 0.005)
sup

# plot loop
for (i in 1:length(sup)) {
  
  par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
  
  png(filename = paste("~/GitHub/agora-local-market/arules/ifp/a5-ItemFreq", sup[i], ".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  itemFrequencyPlot(a5, support = sup[i], cex.names = 0.8, col = "#FFFFFF00", horiz = T,
                    main = paste("Agora Marketplace: Frequent Items (support >", 
                                 sup[i], ")"))
  
  dev.off()
  
}

  
}
```

![ifp-0.025](plots/arules-a5/a5u-ItemFreq-025.png)

Observed at a minimum support of 0.025:

- USA dominates all locations, with 'No Info' second.
- price bins have a higher frequency as there are less levels.

Items observed as occuring with roughly the same relative frequency:

- (above ~0.075): prescription, weed, UK, China, Agora/Internet/Torland<sup>[2](#references-and-notes)</sup>., $600-$1200
- (around ~0.05): benzodiazapines, Netherlands, Germany, EU, Australia
- (< 0.05): 3 vendors, RCs, Guides, steroids, ecstasy: MDMA, ecstasy: pills, Canada


![ifp-0.075](plots/arules-a5/a5u-ItemFreq-075.png)

With a mininum support of 0.075, it becomes clear the most frequently occuring of the frequent items.


# Frequent Itemsets

_arguments_ for `apriori`:

| parameter             |  value    | 
|-----------------------|-----------|
| target                |  frequent |
| minimum support       |  0.0025   |
| min rule length       |  2        |
| max rule length       |  5        |


_results_:

| parameter             |  value        |
|-----------------------|---------------|
| yield                 |  577 itemsets |
| itemset length 2      |  401          |
| itemset length 3      |  153          |
| itemset length 4      |  23           |
| minumum support       |  0.002511     |
| maximum support       |  0.071603     |


_most frequent items_:

| f=USA  | p=$10-$50  | f=No Info | p=$200-$600 | (other) |
|--------|------------|-----------|-------------|---------|
| 61     |      81    |    82     |     61      |   426   |


_the call_:

```{r}
a5items <- apriori(a5, parameter = list(target = "frequent",
                                        supp = 0.0025, minlen = 2, maxlen = 5))
```

A generous set of paramters to generate many rules.

```{r}
summary(a5items)
set of 577 itemsets

most frequent items:
  f=No Info   p=$10-$50    p=$0-$10 p=$200-$600       f=USA     (Other) 
         82          81          74          61          61         994 

element (itemset/transaction) length distribution:sizes
  2   3   4 
401 153  23 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   2.000   2.345   3.000   4.000 

summary of quality measures:
    support        
 Min.   :0.002511  
 1st Qu.:0.003251  
 Median :0.004392  
 Mean   :0.006858  
 3rd Qu.:0.007808  
 Max.   :0.071603  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions support confidence
   a5       2317353  0.0025          1
```

Distribution of itemset lengths is encouraging, althought seeing more sets of length greater than 2 would be nice. Support improves slightly from previous minings; now with a max that hits 7%. Values up to the 3rd quartile are still under a single percentage, though.

Looking at the top of the itemset list, sorted by support:


```{R}
a5items <- sort(a5items, by = "support", decreasing = T)

inspect(head(a5items, 12))
    items                               support   
394 {p=$0-$10,f=No Info}                0.07160325
401 {p=$10-$50,f=USA}                   0.06360360
382 {p=$0-$10,f=Agora/Internet/Torland} 0.05378421
390 {f=USA,c=Cannabis-Weed}             0.04106970
397 {p=$50-$100,f=USA}                  0.03860051
399 {p=$200-$600,f=USA}                 0.03563376
393 {p=$100-$200,f=USA}                 0.03477027
372 {f=No Info,c=Prescription}          0.03234639
400 {p=$10-$50,f=No Info}               0.03227173
327 {p=$0-$10,c=Other}                  0.03056677
230 {f=Agora/Internet/Torland,v=056783} 0.02996695
231 {p=$0-$10,v=056783}                 0.02996695
```

- 6% of all transactions are from the USA and listing for between $10-$50. 
- 4% of all transactions are from the USA and within the category "Cannabis-Weed". 
- Presciption listings with no location info provided comprise apporx 3.2% of itemsets.
- 5.3% of transactions are between $0-$10 and are from online source: Agora/Internet/Torland

```{r}
inspect(a5items)[48:56,]
                                 items    support
305                  {f=EU,c=Steroids} 0.01509006
368       {p=$100-$200,c=Prescription} 0.01451225
187    {f=USA,c=Cannabis-Concentrates} 0.01436769
363              {p=$100-$200,f=China} 0.01436553
341                   {f=USA,c=Benzos} 0.01395471
362               {p=$600-$1200,f=USA} 0.01376916
392            {p=$100-$200,f=No Info} 0.01372385
326 {f=Agora/Internet/Torland,c=Other} 0.01308260
352          {p=$100-$200,f=Australia} 0.01302262
```

[back to top](#AgAssociationRules-05.md)

- 1.5%: EU and Steroids
- 1.4%: USA and Cannabis-Concentrates
- 1.3%: USA and benzos, USA and listings within $600-$1200.

# Mining Association Rules


## Parameters

`apriori` _algorithm arguments_:

| parameter             |  value    | 
|-----------------------|-----------|
| minimum support       |  0.0025   |
| minumum confidence    |  0.6      |
| min rule length       |  2        |
| max rule length       |  --       |

_results_:

| parameter             |  value      |
|-----------------------|-------------|
| yield                 |  329 rules  |
| rules length 2        |  100        |
| rules length 3        |  167        |
| rules length 4        |  62         |


## Function Call and Summary

```{r}
a5rules <- apriori(a5, parameter = list(support = 0.0025, confidence = 0.6, minlen = 2))
```

Keeping a generous minlength at 2 to hopefully mine interesting rules of length > 2.

```{r}
summary(a5rules)
set of 329 rules

rule length distribution (lhs + rhs):sizes
  2   3   4 
100 167  62 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   3.000   2.884   3.000   4.000 

summary of quality measures:
    support           confidence          lift        
 Min.   :0.002511   Min.   :0.6026   Min.   :  2.757  
 1st Qu.:0.003251   1st Qu.:0.7919   1st Qu.:  5.012  
 Median :0.004139   Median :0.9468   Median :  9.716  
 Mean   :0.006367   Mean   :0.8834   Mean   : 23.243  
 3rd Qu.:0.007669   3rd Qu.:0.9941   3rd Qu.: 19.747  
 Max.   :0.053784   Max.   :1.0000   Max.   :292.835  

mining info:
 data ntransactions support confidence
   a5       2317353  0.0025        0.6
```

Out of 329 rules generated:

- 51% of rules of length 3; 30% length 2; and 19% of length 4. An encouraging ditribution of rule lengths.

- Median confidence is sitting nicely at 0.9468, and appears there are positive correlations across the board as seen in a minimum lift of 2.757. 

- The max lift is skewing the distribution of lift values; the mean of 23.243 is well higher than the 3rd quartile of 19.747. That said, 3rd quartile at that value is also encouraging.

- Support might be an issue - very low values observed here. On the other hand, there are so many listings that it might make sense such sparsity is observed.


### Top and Bottom 10

_Measures of Quality_

Given a population of **N** transactions that contains itemsets **N<sub>X</sub>** and **N<sub>Y</sub>**, the rule **X ⇒ Y** can be measured by:

| measure     | formula                                                 |
|-------------|---------------------------------------------------------|
| support     | N<sub>X ∪ Y</sub> / N                                   | 
| confidence  | N<sub>X ∪ Y</sub> / N<sub>X</sub>                       | 
| lift        | N<sub>X ∪ Y</sub> * N / N<sub>X</sub> * N<sub>Y</sub>   |

Sorting by support and checking the top rules mined:

```{r}
a5rules <- sort(a5rules, by = "support", decreasing = T)
arules::inspect(head(a5rules, 10))
    lhs                                    rhs                        support    confidence lift     
100 {f=Agora/Internet/Torland}          => {p=$0-$10}                 0.05378421 0.6469407   4.038385
99  {c=Other}                           => {p=$0-$10}                 0.03056677 0.6026118   3.761672
97  {v=056783}                          => {f=Agora/Internet/Torland} 0.02996695 1.0000000  12.028450
98  {v=056783}                          => {p=$0-$10}                 0.02996695 1.0000000   6.242280
261 {f=Agora/Internet/Torland,v=056783} => {p=$0-$10}                 0.02996695 1.0000000   6.242280
262 {p=$0-$10,v=056783}                 => {f=Agora/Internet/Torland} 0.02996695 1.0000000  12.028450
94  {v=653472}                          => {f=China}                  0.02795388 0.9996605  12.718114
96  {v=d36261}                          => {f=No Info}                0.02574532 0.8881694   5.055492
89  {v=221880}                          => {c=Prescription}           0.02068092 0.9735511  11.894166
90  {v=221880}                          => {f=No Info}                0.02034822 0.9578890   5.452338
```

A nice distribution of items in the consequent; a bit too specific with vendors in the antecedent. Although it can be observed with certainty that {v=056783} operates solely from {f=Agora/Internet/Torland} with listings in the price range of {p=$0-$10}. The redundancy of the top rules 2 through 5 confirm and re-confirm this.

- Coming across a listing that originates from {Agora/Internet/Torland}, there's a 65% probability it will be for less than $10.

- listings in the category 'Other' have a 60% chance of being under $10.

- if the vendor is {v=653472}, there's a 99% probability the listing originates from {China}.

- there's a 97% chance that if the vendor is {v=221880}, the listing will be for Prescriptions.


```{r}
arules::inspect(tail(a5rules, 10))
    lhs                               rhs                        support     confidence lift      
215 {p=$0-$10,v=383177}            => {f=Agora/Internet/Torland} 0.002551187 0.6565971    7.897845
124 {c=Other,v=d95154}             => {f=No Info}                0.002546008 1.0000000    5.692036
120 {c=Steroids,v=d55820}          => {p=$10-$50}                0.002541262 0.6695850    3.040328
121 {p=$10-$50,v=d55820}           => {c=Steroids}               0.002541262 0.8988095   21.045995
144 {p=$10-$50,v=a14658}           => {f=USA}                    0.002524863 0.9237449    4.300380
204 {p=$10-$50,c=Pirated}          => {f=USA}                    0.002524432 0.7983079    3.716423
278 {p=$10-$50,c=Pirated,v=a14658} => {f=USA}                    0.002524432 0.9240246    4.301682
279 {p=$10-$50,f=USA,v=a14658}     => {c=Pirated}                0.002524432 0.9998291   68.544966
280 {p=$10-$50,f=USA,c=Pirated}    => {v=a14658}                 0.002524432 1.0000000  135.083241
327 {p=$10-$50,c=Other,v=d36261}   => {f=No Info}                0.002511055 0.9192733    5.232537
```

- Listings between $10-$50 for Pirated material have an 80% chance of being from the USA. On top of that, if said listing _is_ from the USA, there's a 100% probability the vendor is {a14658}. This rule appears varying degrees 4 times; quite redundant.

- {Steroids} that cost between {$10-$50} are associated with the vendor {d55820}; if the vendor _is_ {d55820} and the listing costs between {$10-$50}, the probability of it being {Steroids} is 90%. 

- However, {d55820} has other Steroids that cost either more or less than {$10-$50}. An antecedent of {c=Steroids,v=d55820} has only a 67% probability of being in the range of p=$10-$50}.

Overall with this particular grouping of variables and discretized bins for prices: there are many redundancies and a lot of vendors appear within the rules. The ruleset might be tightened up by increasing the minlength to 3, or eliminating vendors as transaction variables.

```{R}
inspect(a5rules)[48:56,]
                       lhs             rhs     support confidence      lift
72    {c=Cannabis-Edibles} =>      {f=USA} 0.009023226  0.8532604  3.972248
70              {v=844130} =>   {p=$0-$10} 0.008994314  0.9146882  5.709740
253 {p=$200-$600,v=653472} =>    {f=China} 0.008969717  0.9996633 12.718150
171    {p=$0-$10,v=844130} =>  {f=No Info} 0.008954613  0.9955860  5.666911
172   {f=No Info,v=844130} =>   {p=$0-$10} 0.008954613  0.9143424  5.707582
73           {f=Hong Kong} =>  {c=Watches} 0.008845437  0.8148030 33.109230
66              {v=f41963} =>  {f=No Info} 0.008828607  0.9535328  5.427543
69              {v=632118} =>  {f=No Info} 0.008651681  0.8805393  5.012061
77              {v=a56606} => {p=$50-$100} 0.008533875  0.6480535  3.988486
```

[back to top](#agora-associations-05)

# Mining Association Rules - 3 Rule Length

## Parameters

`apriori` _algorithm arguments_:

| parameter             |  value    | 
|-----------------------|-----------|
| minimum support       |  0.0025   |
| minumum confidence    |  0.6      |
| min rule length       |  3        |
| max rule length       |  --       |

_results_:

| parameter             |  value      |
|-----------------------|-------------|
| yield                 |  229 rules  |
| rules length 2        |  ---        |
| rules length 3        |  167        |
| rules length 4        |  62         |

## Function Call and Summary

```{R}
a5r2 <- apriori(a5, parameter = list(support = 0.0025, confidence = 0.6, minlen = 3))
summary(a5r2)

set of 229 rules

rule length distribution (lhs + rhs):sizes
  3   4 
167  62 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.271   4.000   4.000 

summary of quality measures:
    support           confidence          lift        
 Min.   :0.002511   Min.   :0.6075   Min.   :  2.879  
 1st Qu.:0.003176   1st Qu.:0.7946   1st Qu.:  5.340  
 Median :0.003910   Median :0.9571   Median : 10.814  
 Mean   :0.005483   Mean   :0.8898   Mean   : 23.903  
 3rd Qu.:0.006709   3rd Qu.:0.9968   3rd Qu.: 21.282  
 Max.   :0.029967   Max.   :1.0000   Max.   :245.844  

mining info:
 data ntransactions support confidence
   a5       2317353  0.0025        0.6
```

- max support drops by nearly half, but no big drops in confidence or lift. In fact, confidence and lift are both strengthened.
- Rules of length 3 now make up 73% of the total and length 4 27%. However, the number of these rules hasn't changed; just the total number of rules has been reduced without any of length 2.

``` {R}
a5rules <- sort(a5r2, by = "support", decreasing = T)
arules::inspect(head(a5r2, 10))
   lhs                           rhs               support     confidence lift      
1  {f=USA,v=b28893}           => {p=$10-$50}       0.002706536 0.8883853    4.033816
2  {p=$10-$50,v=b28893}       => {f=USA}           0.002706536 0.9977728    4.645008
3  {c=Ecstasy-Pills,v=807510} => {f=Netherlands}   0.002829090 0.9989334   17.666669
4  {f=Netherlands,v=807510}   => {c=Ecstasy-Pills} 0.002829090 0.7924574   21.281765
5  {c=Prescription,v=764212}  => {f=UK}            0.003340881 0.8977273   10.910283
6  {f=UK,v=764212}            => {c=Prescription}  0.003340881 1.0000000   12.217300
7  {c=Smoked,v=105690}        => {f=UK}            0.003910065 1.0000000   12.153227
8  {f=UK,v=105690}            => {c=Smoked}        0.003910065 1.0000000  115.045078
9  {f=UK,c=Smoked}            => {v=105690}        0.003910065 0.9430683  236.159725
10 {c=Smoked,v=105690}        => {p=$10-$50}       0.003897550 0.9967995    4.526083
```

The top rules even with added length still depend a lot on vendors being involved.

```{r}
arules::inspect(tail(a5r2, 10))
    lhs                                             rhs                        support     confidence lift     
220 {f=No Info,c=Guides,v=d36261}                => {p=$0-$10}                 0.007235842 0.7391343   4.613883
221 {p=$0-$10,f=No Info,c=Guides}                => {v=d36261}                 0.007235842 0.6802158  23.466276
222 {f=Agora/Internet/Torland,c=Guides,v=056783} => {p=$0-$10}                 0.003633456 1.0000000   6.242280
223 {p=$0-$10,c=Guides,v=056783}                 => {f=Agora/Internet/Torland} 0.003633456 1.0000000  12.028450
224 {p=$0-$10,f=Agora/Internet/Torland,c=Guides} => {v=056783}                 0.003633456 0.6367693  21.249052
225 {p=$0-$10,c=Other,v=d36261}                  => {f=No Info}                0.007578043 0.9773486   5.563103
226 {f=No Info,c=Other,v=d36261}                 => {p=$0-$10}                 0.007578043 0.7366500   4.598376
227 {p=$10-$50,c=Other,v=d36261}                 => {f=No Info}                0.002511055 0.9192733   5.232537
228 {f=Agora/Internet/Torland,c=Other,v=056783}  => {p=$0-$10}                 0.005269806 1.0000000   6.242280
229 {p=$0-$10,c=Other,v=056783}                  => {f=Agora/Internet/Torland} 0.005269806 1.0000000  12.028450
```

The reliance of these rules on vendors is too strong, specific.


# Visualizations - Grouped Matrices


```{R}
# individual
plot(a5rules, method = "grouped", control = list(k = 36))
```

36 rules: 

![36 Rule Group](plots/arules-a5/a5u-g1-3.png)

What initially stands out looking at a grouped matrix of 36 is the trend for rules to follow a pattern of {vendor} => {location}. While locations are welcome on the rhs to get 'portraits' of what a country might offer, strict {vendor} => {location} might be too specific to be interesting. Despite that, a few prices and categories come through on the lhs that might be interesting:

- in the lowest price bin of $0-$10 as antecedent, there are a total of 49 rules that result in UK as consequent. Originally the expectation was that items in this price range would be deliverables online e.g. eBooks, information, or pirated software. 

- Hyrdrocodone and Cannabis-Concentrates are antecendent for 81 rules which result in rhs of USA. Will have to see if the other items in the lhs are redundacies creating more rules or are leading to more relationships.

```{R}
# loop
for (i in 1:10) {
  
  png(filename = paste("~/GitHub/agora-local-market/arules/groups/g1-",i,".png"),
      width = 1800, height = 1400, pointsize = 20, bg = "transparent")
  
  k = i * 12
  
  plot(a5rules, method = "grouped", control = list(k = k), 
       main = paste("k =", k))
  
  dev.off()
  
}
```
48 rule grouping: 
![48 Rule Group](plots/arules-a5/a5u-g1-4.png)

Again, reinforcement of the trend for {vendor} => {location} rulesets observed in the 36 rule matrix. The observations on {$0-$10} => {UK},  
{Hyrdocodone} => {USA}, and {Cannabis-Concentrates} => continue to hold as well. 

96 rule grouping:
![96 Rule Group](plots/arules-a5/a5u-g1-8.png)

Looking at 96 rules, much of what was observed earlier continues to hold. 

- Info/eBooks: eBooks and Info/eBooks: IT appear in antecedents with prices between $0-$10 as consequent - as stated earlier, to be expected.
- Cannabis class opens up to include Cannabis-Edibles, and again USA is the consequent (6 rules)
- Vendors continue to make up most of the LHS - taking counts, Netherlands has 9 vendors in LHS, Germany: 7, China 5, and Sweden: 2.
- {Vendor => UK}: 13
- {Vendor => USA}: 10
- in the antecedent of rhs {$0-$10}: UK, Agora/Internet/Torland, No Info (location), Info/eBooks:eBooks, and Info/eBooks: IT - affinities across location and category by price.

# Visualizations - Network Graphs

Network graphs were plotted from rules, which were sorted by the 3 quality measures in different quantities.

- sort by support, confidence, and lift
- sort by lift
- sort by support and confidence

For better or worse, this selection/treatment of variables yielded many vendors - making it a bit difficult to see how classes of category, location, and price relate.

82 rules by support, confidence, and lift. Layout DH, more vendors than desired.

![a5u-r1-82](plots/arules-a5/a5u-r1-SCL-82.png)

```{r}
# plot by Support, Confidence, and Lift
r1 <- head(sort(a5rules, by = c("support", "confidence", "lift")), 82)
p1 <- plot(r1, method = "graph", 
           main = "82 rules ~ support + confidence + lift (dh)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.color = pdpal(100),
           vertex.label.color = "grey8", 
           vertex.label.cex = 0.68, layout = layout_with_dh,
           vertex.label.dist = 0)
```

![a5u-r2-36](plots/arules-a5/a5u-r2-36.png)

Graph of 36 rules sorted by lift alone, plotted using using Kamada & Kawai force-directed layout. It's possible to discern cliques beginning to form around locations, but these types of relationships are a bit too specific to be interesting. While it'd be possible to subset for specific classes, it might be better to revise variable selection and transformation. The minimum support to generate these rules was already quite low, and the dominance of vendor antecedents is less than encouraging.

On to a5...after revising the features selected.


[back to top](#agora-associations-04-04)

# References and Notes

<sup>1</sup> "Shedding Light on the Dark Web." The Economist. The Economist Newspaper, 2016. Web. 23 [Sept. 2016.](http://www.economist.com/news/international/21702176-drug-trade-moving-street-online-cryptomarkets-forced-compete)

<sup>2</sup> the virtual location of Agora/Internet/Torland specializes in books, ebooks, hacking, guides, services.

<sup>3</sup> [Trans High Market Quotations](http://hightimes.com/?s=thmq) published monthly by High Times; data available online goes back to March 2014.



