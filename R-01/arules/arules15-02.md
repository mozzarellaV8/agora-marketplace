# Association Rules 2015-02



```{r}
transactions as itemMatrix in sparse format with
 73748 rows (elements/itemsets/transactions) and
 63760 columns (items) and a density of 0.00007841907 

most frequent items:
usd=[    0.0000177,  257.3572043)                      location=USA 
                            50405                             21184 
usd=[  257.3572043,  785.2310794)    category=Drugs, Cannabis, Weed 
                            12085                             11901 
                 location=No Info                           (Other) 
                            10103                            263062 

element (itemset/transaction) length distribution:
sizes
    5 
73748 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      5       5       5       5       5       5 

includes extended item information - examples:
             labels variables     levels
1 vendor=-Euphoria-    vendor -Euphoria-
2     vendor=-FliP-    vendor     -FliP-
3   vendor=-Inanna-    vendor   -Inanna-

```

```{r}
set of 32 itemsets

most frequent items:
usd=[    0.0000177,  257.3572043)                      location=USA 
                               27                                11 
                 location=No Info    category=Drugs, Cannabis, Weed 
                                9                                 7 
                 vendor=sexyhomer                           (Other) 
                                4                                39 

element (itemset/transaction) length distribution:sizes
 3  4 
31  1 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.031   3.000   4.000 

summary of quality measures:
    support        
 Min.   :0.006821  
 1st Qu.:0.007526  
 Median :0.008163  
 Mean   :0.010798  
 3rd Qu.:0.011095  
 Max.   :0.046957  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions     support confidence
   v2         73748 0.006779845          1
```

``` {r}

> v2rules <- apriori(v2, parameter = list(support = 0.006821, confidence = 0.6))
set of 106 rules 
> summary(v2rules)
set of 106 rules

rule length distribution (lhs + rhs):sizes
 1  2  3  4 
 1 56 45  4 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   2.000   2.491   3.000   4.000 

summary of quality measures:
    support           confidence          lift         
 Min.   :0.006848   Min.   :0.6011   Min.   :  0.8795  
 1st Qu.:0.007831   1st Qu.:0.7013   1st Qu.:  1.1611  
 Median :0.009458   Median :0.8601   Median :  1.4502  
 Mean   :0.023165   Mean   :0.8338   Mean   : 10.9759  
 3rd Qu.:0.016462   3rd Qu.:0.9740   3rd Qu.:  6.9460  
 Max.   :0.683476   Max.   :1.0000   Max.   :104.1638  

mining info:
 data ntransactions  support confidence
   v2         73748 0.006821        0.6

```