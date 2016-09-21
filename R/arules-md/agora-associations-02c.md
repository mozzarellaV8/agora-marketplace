# Agora - All Data - Association Rules 02b

# Subset and Discretize

This is a subset of listings under $1,000, including the variables

- location
- price
- category 
- subcategories

Price was initially discretized into 5 categories by interval, resulting in bins of $200. Distribution:

``` {R}
summary(agora$p)
# [   0, 200) [ 200, 400) [ 400, 600) [ 600, 800) [ 800,1000] 
#     1586915      269554      116037       66343       52581 
```


# Transactions

Summary after converting `a2` to transactions yields frequent itemsets with prices between 0-400 USD, locations of USA and 'No Info', and the category of `Drugs`.

``` {R}
summary(a2)
transactions as itemMatrix in sparse format with
 2091430 rows (elements/itemsets/transactions) and
 221 columns (items) and a density of 0.01809955 

most frequent items:
  p=$0-$200     c=Drugs       l=USA   l=No Info p=$200-$400     (Other) 
    1586915     1398867      461421      382687      269554     4266276 

element (itemset/transaction) length distribution:
sizes
      4 
2091430 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      4       4       4       4       4       4 

includes extended item information - examples:
                    labels variables                 levels
1            l=Afghanistan         l            Afghanistan
2 l=Agora/Internet/Torland         l Agora/Internet/Torland
3          l=Aland Islands         l          Aland Islands

includes extended transaction information - examples:
  transactionID
1             1
2             2
3             3
```

# Frequent Itemsets

Mining frequent itemsets yields a decent amount - 1427:

``` {R}
summary(a2items)
set of 1427 itemsets

most frequent items:
  p=$0-$200     c=Drugs       l=USA p=$200-$400   l=No Info     (Other) 
        712         699         242         201         160        2646 

element (itemset/transaction) length distribution:sizes
   3    4 
1048  379 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.266   4.000   4.000 

summary of quality measures:
    support         
 Min.   :0.0004781  
 1st Qu.:0.0007079  
 Median :0.0012723  
 Mean   :0.0030999  
 3rd Qu.:0.0028301  
 Max.   :0.1350420  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions  support confidence
   a2       2091430 0.000478          1

```

But is rich with possibilities in length distributions, which can hopefully provide clues on dataset structure. Unfortunately support remains rather low, with a max value just under 0.04 and most values up to the 3rd quantile less than 0.0005.

After looking at [6] item frequency plots, it was decided to mine rules with a minimum support of 0.001, minumum confidence of 0.60, and perhaps most importantly a minumum length of 3. The goal here was to balance finding a large number of rules to prune from, but also to have at least 2 items as antecedent to see relationships between categories.

# Association Rules

``` {R}
a2rules <- apriori(a2, parameter = list(support = 0.001, confidence = 0.6,
                                        minlen = 3, maxlen = 5))

set of 1156 rules

rule length distribution (lhs + rhs):sizes
  3   4 
768 388 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.336   4.000   4.000 

summary of quality measures:
    support           confidence          lift         
 Min.   :0.001002   Min.   :0.6010   Min.   :  0.7921  
 1st Qu.:0.001592   1st Qu.:0.8616   1st Qu.:  1.2689  
 Median :0.002484   Median :1.0000   Median :  1.4951  
 Mean   :0.005360   Mean   :0.9229   Mean   : 12.7942  
 3rd Qu.:0.005193   3rd Qu.:1.0000   3rd Qu.:  5.4206  
 Max.   :0.135042   Max.   :1.0000   Max.   :335.6796  

mining info:
 data ntransactions support confidence
   a2       2091430   0.001        0.6
```

This yielded a set of 1156 rules, with good rule length distribution. Confidence and lift value distributions also seemed promising.

``` {r}
arules::inspect(head(a2rules, 10))
   lhs                                                                  rhs                    support     confidence lift     
1  {c=Drug paraphernalia,sc=Drug paraphernalia, Scales}              => {p=$0-$200}            0.001158537 0.9983519   1.315750
2  {p=$0-$200,sc=Drug paraphernalia, Scales}                         => {c=Drug paraphernalia} 0.001158537 1.0000000  65.683553
3  {c=Drug paraphernalia,sc=Drug paraphernalia, Injecting equipment} => {p=$0-$200}            0.001146584 0.9848049   1.297896
4  {p=$0-$200,sc=Drug paraphernalia, Injecting equipment}            => {c=Drug paraphernalia} 0.001146584 1.0000000  65.683553
5  {c=Drugs,sc=Drugs, Opioids, Hydromorphone}                        => {p=$0-$200}            0.001048087 0.8965235   1.181548
6  {p=$0-$200,sc=Drugs, Opioids, Hydromorphone}                      => {c=Drugs}              0.001048087 1.0000000   1.495089
7  {c=Info/eBooks,sc=Info/eBooks, Philosophy}                        => {p=$0-$200}            0.001375136 1.0000000   1.317922
8  {p=$0-$200,sc=Info/eBooks, Philosophy}                            => {c=Info/eBooks}        0.001375136 1.0000000  13.048602
9  {c=Drug paraphernalia,sc=Drug paraphernalia, Paper}               => {l=USA}                0.001142281 0.7936877   3.597457
10 {l=USA,sc=Drug paraphernalia, Paper}                              => {c=Drug paraphernalia} 0.001142281 1.0000000  65.683553
```


# Visualizing the Rules

First - a few matrices to see where the quality measures lie. Then some grouped matrices with controls at 100, 75, 50, 25, 10. 

### Itemsets in Consequent (RHS)

All rules:

``` {R}
Itemsets in Consequent (RHS)
 [1] "{p=$0-$200}"                       
 [2] "{c=Drug paraphernalia}"            
 [3] "{c=Drugs}"                         
 [4] "{c=Info/eBooks}"                   
 [5] "{l=USA}"                           
 [6] "{c=Weapons}"                       
 [7] "{sc=Drugs, Cannabis, Weed}"        
 [8] "{l=Agora/Internet/Torland}"        
 [9] "{c=Tobacco}"                       
[10] "{sc=Tobacco, Paraphernalia}"       
[11] "{l=No Info}"                       
[12] "{c=Forgeries}"                     
[13] "{c=Services}"                      
[14] "{c=Counterfeits}"                  
[15] "{sc=Forgeries, Scans/Photos}"      
[16] "{c=Data}"                          
[17] "{sc=Forgeries, Physical documents}"
[18] "{l=China}"                         
[19] "{sc=Jewelry, No Info/Other}"       
[20] "{c=Jewelry}"                       
[21] "{sc=Electronics, No Info/Other}"   
[22] "{c=Electronics}"                   
[23] "{sc=Tobacco, Smoked}"              
[24] "{sc=Counterfeits, Watches}"        
[25] "{l=Hong Kong}"                     
[26] "{sc=Services, Money}"              
[27] "{sc=Data, Accounts}"               
[28] "{c=Other}"                         
[29] "{sc=Other, No Info/Other}"         
[30] "{c=Information}"                   
[31] "{sc=Data, Pirated}"                
[32] "{sc=Information, Guides}"          
[33] "{sc=Drugs, Prescription}"
```

### SubRules1: by SCL, 128 top rules:

```{R}
Itemsets in Consequent (RHS)
 [1] "{c=Drugs}"                  "{p=$0-$200}"               
 [3] "{l=No Info}"                "{c=Info/eBooks}"           
 [5] "{c=Information}"            "{sc=Drugs, Prescription}"  
 [7] "{l=China}"                  "{sc=Information, Guides}"  
 [9] "{c=Counterfeits}"           "{c=Other}"                 
[11] "{sc=Other, No Info/Other}"  "{c=Data}"                  
[13] "{l=USA}"                    "{sc=Counterfeits, Watches}"
[15] "{c=Services}" 

```























