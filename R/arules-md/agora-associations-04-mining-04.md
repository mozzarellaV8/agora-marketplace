# Agora Associations 04-04

R script: **_agora-associations-04-mining-02.R_**

- [Variable Selection](#variable-selection)
- [Transaction Conversion](#transaction-conversion)
- [Mining Association Rules](#mining-association-rules)
- [Grouped Matrices]()
- [Network Graphs]()


# Variable Selection

dataframe: **a3**

2317353 observations of 4 variables:

| name  | variable      | levels    |   notes                                       |
|-------|---------------|-----------|-----------------------------------------------|
| `p`   | price         | 6         | discretized into 6 bins                       |
| `f`   | from          | 85        | product origin location                       |
| `sc`  | subcategory   | 105       | subcategory as labeled on Agora               |
| `v`   | vendor    | 3183          | anonymized with SHA256 hashing algorithm      |

Price `p` ranged from $0-20,000 and was discretized manually into 6 bins: 

- $0-10
- $10-150
- $150-600
- $600-2000
- $2000-10000
- $10000-20000

While not in equal intervals, these bins reflect the distribution of prices on the market and take into account a large number of listings under 10- and 200- USD.

Although vendors did not use given names, there have been cases where even a vendor's online name could be used to identify them<sup>[1](#references-and-notes)</sup>. To avoid this implication, names were run through the function `anonymize`. This salted and then hashed the names using SHA256, and from there names were abbreviated for clarity. While likely not following the strictest security protocol, this level of anonymization felt suited for the application. In practical terms - all of this data is publicly available, so these measures were taken out of a careful respect for privacy.

```{r}
ag <- subset(ag, select = c("p", "from", "sc", "v3"))
colnames(ag) <- c("p", "f", "sc", "v")
head(ag)
             p       f                 sc      v
1 $10000-20000   China              Other e63948
2      $10-150 No Info             Guides 189622
3      $10-150 No Info             Guides 447418
4    $600-2000   China                RCs a00543
5      $10-150     USA Stimulants-Cocaine e99113
6      $10-150 No Info             Guides 189622
```

# Transaction Conversion


```{R}
a4 <- as(ag, "transactions")
```

```{R}
summary(a4)
transactions as itemMatrix in sparse format with
 2317353 rows (elements/itemsets/transactions) and
 3379 columns (items) and a density of 0.001183782 

most frequent items:
 p=$10-150 p=$150-600      f=USA  f=No Info    p=$0-10    (Other) 
   1086166     515111     497780     407122     371235    6391998 

element (itemset/transaction) length distribution:
sizes
      4 
2317353 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      4       4       4       4       4       4 

includes extended item information - examples:
          labels variables       levels
1        p=$0-10         p        $0-10
2      p=$10-150         p      $10-150
3 p=$10000-20000         p $10000-20000

includes extended transaction information - examples:
  transactionID
1             1
2             2
3             3
```

# Item Frequency Plot


``` {R}
# Item Frequency Plot ---------------------------------------------------------

par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
itemFrequencyPlot(a4, support = 0.0025, cex.names = 0.65, col = "white", horiz = T,
                  main = "Agora Marketplace: Frequent Items (support > 0.0025)")
```

![itemFreq-0.005](plots/arules/a4-04-ItemFreq-0005.png)

Wanting to see how different minimum supports affected item frequency, scripted a loop to plot a sequence of values.

```{R}
# Item Frequency Plot Loop ----------------------------------------------------

# define support intervals
sup <- seq(0.000, 0.1, by = 0.005)
sup

# plot loop
for (i in 1:length(sup)) {
  
  par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
  
  png(filename = paste("~/GitHub/agora-local-market/arules/ifp/ItemFreq", sup[i], ".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  itemFrequencyPlot(a4, support = sup[i], cex.names = 0.8, col = "#FFFFFF00", horiz = T,
                    main = paste("Agora Marketplace: Frequent Items (support >", 
                                 sup[i], ")"))
  
  dev.off()
  
}
```

![ifp-0.025](a4-04-ItemFreq-0.025.png)

![ifp-0.075](a4-04-ItemFreq-0.075.png)




# Mining Association Rules


## Parameters

_`apriori` algorithm arguments_:

| parameter             |  value    | 
|-----------------------|-----------|
| minimum support       |  0.0025   |
| minumum confidence    |  0.6      |
| min rule length       |  2        |
| max rule length       |  5        |

_results_:

| parameter       |  value    |
|-----------------------|---------------|
| yield           |  395 rules  |
| rules length 2      |  27     |
| rules length 3    |  200      |
| rules length 4    |  68     |


## Function Call and Summary

```{r}

a3 <- subset(ag, select = c("p", "f", "sc", "v"))

a3rules <- apriori(a3, parameter = list(support = 0.0025, confidence = 0.6,
                                        minlen = 2, maxlen = 5))
```


```{r}
summary(a3rules)

set of 395 rules

rule length distribution (lhs + rhs):sizes
  2   3   4 
127 200  68 

 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
    2.000   2.000   3.000   2.851   3.000   4.000 

summary of quality measures:
    support           confidence          lift        
 # Min.   :0.002524   Min.   :0.6011   Min.   :  1.284  
 # 1st Qu.:0.003277   1st Qu.:0.7594   1st Qu.:  3.881  
 # Median :0.004357   Median :0.9069   Median :  6.103  
 # Mean   :0.006679   Mean   :0.8662   Mean   : 19.707  
 # 3rd Qu.:0.007633   3rd Qu.:0.9859   3rd Qu.: 17.782  
 # Max.   :0.053784   Max.   :1.0000   Max.   :292.835  

mining info:
 data ntransactions support confidence
   a3       2317353  0.0025        0.6
```


### Top and Bottom 10

_Measures of Quality_

Given a population of **N** transactions that contains itemsets **N<sub>X</sub>** and **N<sub>Y</sub>**, the rule **X ⇒ Y** can be measured by:

| measure     | formula                           |
|---------------|-----------------------------------------------------------|
| support     | N<sub>X ∪ Y</sub> / N                   | 
| confidence  | N<sub>X ∪ Y</sub> / N<sub>X</sub>             | 
| lift      | N<sub>X ∪ Y</sub> * N / N<sub>X</sub> * N<sub>Y</sub>   | 



A quick `inspect` of the top and bottom 10 rules. 

```{r}
arules::inspect(head(a3rules, 10))
   lhs                rhs             support     confidence lift      
1  {v=a74314}      => {f=USA}         0.002610306 0.9879144    4.599113
2  {v=259334}      => {f=Netherlands} 0.002663384 1.0000000   17.685532
3  {sc=Stashes}    => {p=$10-150}     0.002632745 0.9489812    2.024667
4  {v=682306}      => {f=China}       0.002677624 0.9627618   12.248673
5  {v=553007}      => {f=Poland}      0.002612895 0.9274008  292.834867
6  {f=Poland}      => {v=553007}      0.002612895 0.8250443  292.834867
7  {v=690113}      => {f=Sweden}      0.002845488 0.9996968   92.414646
8  {sc=Containers} => {p=$10-150}     0.002651732 0.8862129    1.890750
9  {v=d96493}      => {p=$0-10}       0.002805356 0.9273894    5.789025
10 {v=b28893}      => {f=USA}         0.003046579 0.9980209    4.646163
```

Rule length two at the head; longer rules at the bottom. Support could be higher all around, but it was decided to keep that value low to generate more rules. Confidence appears healthy with many values at or near 1.0. Lift also appears to be doing well - although many of the rules with very high lift might be too obvious to warrant investigation. An example from above being rule 5:

  5  {v=553007}      => {f=Poland}      0.002612895 0.9274008  292.834867

What can we learn from this other than a particular vendor operates out of Poland? Having a support near 0 and and large value for `N` could be inflating the lift for this particular rule. A more interesting rule, subjectively, might be number 8:
  
  8  {sc=Containers} => {p=$10-150}     0.002651732 0.8862129    1.890750

There's an 88% chance that if the subcategory is `Containers`, the price will be within 10-150 USD. From exploratory plots, 


N<sub>X ∪ Y</sub> / N = 0.002651732 

N<sub>X ∪ Y</sub> / 2317353 = 0.002651732 

N<sub>X ∪ Y</sub> = 0.002651732 * 2317353 = 6144.999



N<sub>X ∪ Y</sub> / N<sub>X</sub> = 0.8862129 

6144.999 / N<sub>X</sub> = 0.8862129 

N<sub>X</sub> = 6144.999 / 0.8862129 

N<sub>X</sub> = 6933.999



N<sub>X ∪ Y</sub> * N / N<sub>X</sub> * N<sub>Y</sub> = 1.890750

6144.999 * 2317535 / N<sub>X</sub> * N<sub>Y</sub> = 1.890750

14241250257 / (6933.999 * N<sub>Y</sub>) = 1.890750









```{r}
arules::inspect(tail(a3rules, 10))
    lhs                                              rhs                        support     confidence lift     
386 {f=Agora/Internet/Torland,sc=Guides,v=056783} => {p=$0-10}                  0.003633456 1.0000000   6.242280
387 {p=$0-10,sc=Guides,v=056783}                  => {f=Agora/Internet/Torland} 0.003633456 1.0000000  12.028450
388 {p=$0-10,f=Agora/Internet/Torland,sc=Guides}  => {v=056783}                 0.003633456 0.6367693  21.249052
389 {p=$150-600,sc=RCs,v=653472}                  => {f=China}                  0.003118213 1.0000000  12.722433
390 {p=$150-600,f=China,sc=RCs}                   => {v=653472}                 0.003118213 0.6099949  21.814071
391 {p=$0-10,sc=Other,v=d36261}                   => {f=No Info}                0.007578043 0.9773486   5.563103
392 {f=No Info,sc=Other,v=d36261}                 => {p=$0-10}                  0.007578043 0.7366500   4.598376
393 {p=$10-150,sc=Other,v=d36261}                 => {f=No Info}                0.002662952 0.9049714   5.151130
394 {f=Agora/Internet/Torland,sc=Other,v=056783}  => {p=$0-10}                  0.005269806 1.0000000   6.242280
395 {p=$0-10,sc=Other,v=056783}                   => {f=Agora/Internet/Torland} 0.005269806 1.0000000  12.028450
```

# References and Notes

<sup>1</sup> "Shedding Light on the Dark Web." The Economist. The Economist Newspaper, 2016. Web. 23 [Sept. 2016.](http://www.economist.com/news/international/21702176-drug-trade-moving-street-online-cryptomarkets-forced-compete)

<sup>2</sup> While likely not following the strictest security protocol, this level of anonymization felt suited for the application. In practical terms, all of this data is publicly available so these measures were done out of a careful respect for privacy.





