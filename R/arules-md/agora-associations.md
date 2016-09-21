# Agora - All Data - Association Rules

A first look at all the data from Agora, through Association Rule Mining.

To minimize false placeholder listings, I took subset of the data with prices under $50,000.

The variables `category`, `subcategory`, and `subsubcategory` were gathered into one column.

``` {R}
ag <- subset(a, a$usd <= 50000) # 2319949
ag <- subset(ag, select = c("vendor", "product", "cat", "subcat", "subsubcat", "from"))
agora <- gather(ag, key = level, value = category, cat, subcat, subsubcat)
```

After removing duplicates and the `category` column, we have 355808 observations to convert to transactions.

The first transaction set includes:

- vendor
- product
- location (from)
- category

From the summary we can see the locations `USA` & `UK` - along with categories `Drugs` &`Cannabis` - appear most frequently. `No Info` doesn't seem promising at first, but including it a network graph in the RHS could reveal what categories or products remain anonymous in the market.

```{R}
transactions as itemMatrix in sparse format with
 355808 rows (elements/itemsets/transactions) and
 110409 columns (items) and a density of 0.00003520758 

most frequent items:
     l=USA    c=Drugs  l=No Info       l=UK c=Cannabis    (Other) 
    107168      95514      43113      34497      31682    1071135 

element (itemset/transaction) length distribution:
sizes
     3      4 
 40123 315685 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   4.000   4.000   3.887   4.000   4.000 

includes extended item information - examples:
          labels variables       levels
1 v=-BIGG-BALLs-         v -BIGG-BALLs-
2   v=-Euphoria-         v   -Euphoria-
3       v=-FliP-         v       -FliP-

includes extended transaction information - examples:
  transactionID
1             1
2             2
3             3
```

From here we can mine frequent itemsets. The parameters were a minumum support of "0.0014", minimum length of 2 and maximum length of 5. Most of the itemsets had a length of 2 (193); only 8 had a length of 3. 

The summary:

``` {R}
500/nrow(a2) #  0.001405252

summmary(a2items)
set of 201 itemsets

most frequent items:
    l=USA l=No Info   c=Drugs      l=UK l=Germany   (Other) 
       40        33        23        18        17       279 

element (itemset/transaction) length distribution:sizes
  2   3 
193   8 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    2.00    2.00    2.04    2.00    3.00 

summary of quality measures:
    support        
 Min.   :0.001402  
 1st Qu.:0.001841  
 Median :0.002277  
 Mean   :0.004466  
 3rd Qu.:0.004199  
 Max.   :0.090855  

includes transaction ID lists: FALSE 
```

![](plots/arules/freqItemSets-001.jpeg)

Incrementally increasing the minimum support for frequent itemset plotting.

Association Rules:

``` {R}
a2rules <- apriori(a2, parameter = list(support = 0.0010, confidence = 0.6,
                                        minlen = 2, maxlen = 5))
a2rules
# set of 165 rules 
summary(a2rules)

set of 165 rules

rule length distribution (lhs + rhs):sizes
  2   3 
145  20 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   2.000   2.121   2.000   3.000 

summary of quality measures:
    support           confidence          lift        
 Min.   :0.001001   Min.   :0.6280   Min.   :  2.125  
 1st Qu.:0.001152   1st Qu.:0.8940   1st Qu.:  3.320  
 Median :0.001374   Median :0.9652   Median : 10.236  
 Mean   :0.001998   Mean   :0.9262   Mean   : 20.792  
 3rd Qu.:0.002105   3rd Qu.:1.0000   3rd Qu.: 14.604  
 Max.   :0.008996   Max.   :1.0000   Max.   :382.589  

mining info:
 data ntransactions support confidence
   a2        355808   0.001        0.6
```













mining info:
 data ntransactions support confidence
   a2        355808  0.0014          1



