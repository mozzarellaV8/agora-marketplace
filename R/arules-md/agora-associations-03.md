# Agora Associations 03

outputs from the file `agora-associations-03.R`

# Preparation

## Subset and Categorize

```{r}
ag <- subset(a, a$usd <= 20000) # 2319949
```

## Combine Subcategories

```{r}
ag$subcat <- as.character(ag$subcat)
ag$subsubcat <- as.character(ag$subsubcat)
ag$subcat[is.na(ag$subcat)] <- ""
ag$subsubcat[is.na(ag$subsubcat)] <- ""

ag$sc <- paste(ag$subcat, ag$subsubcat, sep = ", ")
levels(as.factor(ag$sc))
ag$sc <- gsub("\\b,\\s$", "", ag$sc)

levels(as.factor(ag$sc))
ag$sc <- factor(ag$sc) # 106 levels
```

## Discretize Prices

```{R}
# manually
ag$p <- ag$usd
ag$p <- ifelse(ag$p <= 10.00, "$0-10", 
               ifelse(ag$p > 10 & ag$p <= 150.00, "$10-150",
                      ifelse(ag$p > 150 & ag$p <= 600.00, "$150-600",
                             ifelse(ag$p > 600 & ag$p <= 2000.00, "$600-2000",
                                    ifelse(ag$p > 2000 & ag$p <= 10000, "$2000-10000",
                                           ifelse(ag$p > 10000, "$10000-20000", NA))))))


ag$p <- factor(ag$p)  # 6 levels

summary(ag$p)
#  $0-10      $10-150 $10000-20000     $150-600  $2000-10000    $600-2000 
# 371235      1086166         7393       515111       106747       230701 

371235/nrow(ag)   # 0.1601979
1086166/nrow(ag)  # 0.4687098
7393/nrow(ag)     # 0.003190278
515111/nrow(ag)   # 0.2222842
106747/nrow(ag)   # 0.04606419
230701/nrow(ag)   # 0.09955367

ggplot(ag, aes(reorder(p), color = "black", fill = p)) + geom_bar() +
  scale_fill_manual(values = c("#EE2C2C32", "#EE2C2C94", "#EE2C2C02", 
                               "#EE2C2C44", "#EE2C2C10", "#EE2C2C20"),
                    guide = F) +
  theme_minimal(base_size = 16, base_family = "GillSans") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.text.y = element_text(size = 14.75),
        axis.text.x = element_text(size = 14.75),
        legend.position = "none") +
  labs(title = "Distribution of Discretized Prices", 
       x = "", y = "", colour = "", fill = "")
```       

## Anonymize Vendors

```{R}
ag$v2 <- ag$vendor
ag$v2 <- anonymize(ag$v2, .algo = "sha256", .seed = 144, 
                   .chars = letters[seq(from = 1, to = 26)])

nchar(ag$v2[234]) # 64
ag$v3 <- abbreviate(ag$v2, minlength = 6, strict = F, method = "left.kept")
levels(as.factor(ag$v3))

ag$v3 <- factor(ag$v3)
summary(ag$v3)
```

# As Transactions

- price
- location
- category
- subcategory (pasted subcat and subsubcat)
- vendor (anonymized SHA256 and abbreviated)

### Transactions - Full
summary(a2)
transactions as itemMatrix in sparse format with
 2317353 rows (elements/itemsets/transactions) and
 3395 columns (items) and a density of 0.001472754 

most frequent items:
   c=Drugs  p=$10-150 p=$150-600      f=USA  f=No Info    (Other) 
   1605481    1086166     515111     497780     407122    7475105 

element (itemset/transaction) length distribution:
sizes
      5 
2317353 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      5       5       5       5       5       5 

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


### Transactions - Unique


```{R}
transactions as itemMatrix in sparse format with
 30956 rows (elements/itemsets/transactions) and
 3395 columns (items) and a density of 0.001472754 

most frequent items:
    c=Drugs   p=$10-150       f=USA  p=$150-600 p=$600-2000     (Other) 
      25866       12000        9167        7904        4588       95255 

element (itemset/transaction) length distribution:
sizes
    5 
30956 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      5       5       5       5       5       5 

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


# Frequent Itemsets

```{R}
a2items <- apriori(a2, parameter = list(target = "frequent",
                                        supp = 0.0025, minlen = 2, maxlen = 5))

summary(a2items)
set of 738 itemsets

most frequent items:
    c=Drugs   p=$10-150       f=USA  p=$150-600 p=$600-2000     (Other) 
        337         163         140         118          70        1055 

element (itemset/transaction) length distribution:sizes
  2   3   4 
382 305  51 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   2.000   2.551   3.000   4.000 

summary of quality measures:
    support        
 Min.   :0.002520  
 1st Qu.:0.003327  
 Median :0.004975  
 Mean   :0.010439  
 3rd Qu.:0.009303  
 Max.   :0.323136  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions support confidence
   a2         30956  0.0025          1
```

# Mine Association Rules

```{r}
a2rules <- apriori(a2, parameter = list(support = 0.0025, confidence = 0.6,
                                        minlen = 3, maxlen = 5))
```

Decided on minsup 0.0025, minconf 0.6, minlen 3.

```{r}
summary(a2rules)
set of 298 rules

rule length distribution (lhs + rhs):sizes
  3   4 
245  53 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.178   3.000   4.000 

summary of quality measures:
    support           confidence          lift        
 Min.   :0.002520   Min.   :0.6028   Min.   : 0.7288  
 1st Qu.:0.003295   1st Qu.:1.0000   1st Qu.: 1.1968  
 Median :0.004797   Median :1.0000   Median : 1.1968  
 Mean   :0.007822   Mean   :0.9575   Mean   : 5.0688  
 3rd Qu.:0.008439   3rd Qu.:1.0000   3rd Qu.: 1.1968  
 Max.   :0.110156   Max.   :1.0000   Max.   :89.7731  

mining info:
 data ntransactions support confidence
   a2         30956  0.0025        0.6

```





