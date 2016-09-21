# Agora Associations - 01

transactions as itemMatrix in sparse format with
 2319949 rows (elements/itemsets/transactions) and
 3386 columns (items) and a density of 0.0008860012 

most frequent items:
                   l=USA                l=No Info  c=Drugs, Cannabis, Weed 
                  498196                   407392                   228618 
l=Agora/Internet/Torland                     l=UK                  (Other) 
                  192673                   190829                  5442139 

element (itemset/transaction) length distribution:
sizes
      3 
2319949 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      3       3       3       3       3       3 

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

a2items <- apriori(a2, parameter = list(target = "frequent",
                                         supp = 0.00022, minlen = 3, maxlen = 5))

summary(a2items)
# set of 1622 itemsets

#     support        
# Min.   :0.006821  
# 1st Qu.:0.007526  
# Median :0.008163  
# Mean   :0.010798  
# 3rd Qu.:0.011095  
# Max.   :0.046957


par(mfrow = c(1, 1), mar = c(20, 6, 4, 2), family = "GillSans")
itemFrequencyPlot(a2, support = 0.15, cex.names = 1, col = "white",
                  main = "Agora 2015: Frequent Items (support > 0.15)")


a2rules <- apriori(a2, parameter = list(support = 0.001, confidence = 0.6,
                                         minlen = 3, maxlen = 5))

summary(a2rules)
set of 207 rules

rule length distribution (lhs + rhs):sizes
  3 
207 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      3       3       3       3       3       3 

summary of quality measures:
    support           confidence          lift        
 Min.   :0.001006   Min.   :0.6028   Min.   :  3.433  
 1st Qu.:0.001275   1st Qu.:0.8676   1st Qu.: 10.595  
 Median :0.001788   Median :0.9779   Median : 17.631  
 Mean   :0.002719   Mean   :0.9170   Mean   : 53.666  
 3rd Qu.:0.002383   3rd Qu.:1.0000   3rd Qu.: 56.987  
 Max.   :0.019863   Max.   :1.0000   Max.   :544.888  

mining info:
 data ntransactions support confidence
   a2       2319949   0.001        0.6
