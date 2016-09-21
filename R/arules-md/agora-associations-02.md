# Agora - All Data - Association Rules 02

This is a subset of listings under $50,000, including the variables

- vendor
- price
- category
- location
- month/year

Price was initially discretized into 6 categories by cluster. The majority of the listings fall into the first bin under $1,000. 

``` {R}
# [    0.0000177,  584.9023594) [  584.9023594, 1945.4112438) [ 1945.4112438, 4545.1142149) [ 4545.1142149, 9398.0661600) 
# 5892384                        703047                        246204                         81687 
# [ 9398.0661600,21429.0853363) [21429.0853363,49968.8495313] 
# 29499                          7026 

a2$usd <- mapvalues(a2$usd, from = c("[    0.0000177,  584.9023594)", "[  584.9023594, 1945.4112438)", 
                                     "[ 1945.4112438, 4545.1142149)", "[ 4545.1142149, 9398.0661600)", 
                                     "[ 9398.0661600,21429.0853363)", "[21429.0853363,49968.8495313]"), 
                    to = c("$0 - $585", "$585 - $1945", "$1945 - $4545",  "$4545 - $9398", 
                           "$9398 - $21429", "$21429 - $49968"))

a2 <- as(a2, "transactions")
```

For finer grain, price was re-discretized into 12 clusters:

```{R}

a2$usd <- discretize(a2$usd, method = "cluster", categories = 12)
summary(a2$usd)
# [    0.0000177,  190.8081889) [  190.8081889,  558.6723350) [  558.6723350, 1116.3559664) [ 1116.3559664, 1925.3511669) 
# 4690299                       1176402                        469104                        255981 
# [ 1925.3511669, 3075.4029351) [ 3075.4029351, 4619.0670688) [ 4619.0670688, 6567.5924521) [ 6567.5924521, 9243.3192909) 
# 174399                         77637                         50142                         28590 
# [ 9243.3192909,13590.6334919) [13590.6334919,20912.4649857) [20912.4649857,32924.7602383) [32924.7602383,49968.8495313] 
# 21609                          8406                          5070                          2208

a2$usd <- mapvalues(a2$usd, from = c("[    0.0000177,  190.8081889)", "[  190.8081889,  558.6723350)",
                                     "[  558.6723350, 1116.3559664)", "[ 1116.3559664, 1925.3511669)",
                                     "[ 1925.3511669, 3075.4029351)", "[ 3075.4029351, 4619.0670688)",
                                     "[ 4619.0670688, 6567.5924521)", "[ 6567.5924521, 9243.3192909)",
                                     "[ 9243.3192909,13590.6334919)", "[13590.6334919,20912.4649857)", 
                                     "[20912.4649857,32924.7602383)", "[32924.7602383,49968.8495313]"), 
                    to = c("$0.00-$190", "$190-$558", "$558-$1116",  "$1116-$1925", "$1925-$3075", "$3075-$4620", 
                           "$4620-$6567", "$6567-$9243", "$9243-$13590", "$13950-20912", "$20912-$32924", "$32924-$49968"))
```

...and then renamed for clarity.

Summary after converting `a2` to transactions yields frequent itemsets in prices under $1000 USD, locations of USA and 'No Info'
, and the category of `Drugs`.

``` {R}
> a2 <- as(a2, "transactions")
> summary(a2)
transactions as itemMatrix in sparse format with
 6959847 rows (elements/itemsets/transactions) and
 3408 columns (items) and a density of 0.001414363 

most frequent items:
usd=$0.00-$190        c=Drugs          l=USA      l=No Info  usd=$190-$558        (Other) 
       4690299        1607961        1494588        1222176        1176402       23356086 

element (itemset/transaction) length distribution:
sizes
      4       5 
1251723 5708124 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.00    5.00    5.00    4.82    5.00    5.00 

includes extended item information - examples:
     labels variables  levels
1 d=2014-01         d 2014-01
2 d=2014-02         d 2014-02
3 d=2014-03         d 2014-03

includes extended transaction information - examples:
  transactionID
1             1
2             2
3             3
```

Mining frequent itemsets yields a large amount:

``` {R}

summary(a2items)
set of 13367 itemsets

most frequent items:
usd=$0.00-$190        c=Drugs          l=USA  usd=$190-$558      l=No Info        (Other) 
          6177           2154           1883           1499           1423          31191 

element (itemset/transaction) length distribution:sizes
   3    4    5 
9480 3548  339 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.316   4.000   5.000 

summary of quality measures:
    support         
 Min.   :0.0001401  
 1st Qu.:0.0001802  
 Median :0.0002576  
 Mean   :0.0005253  
 3rd Qu.:0.0004601  
 Max.   :0.0397661  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions support confidence
   a2       6959847 0.00014          1

```

But is rich with possibilities in length distributions, which can hopefully provide clues on dataset structure. Unfortunately support remains rather low, with a max value just under 0.04 and most values up to the 3rd quantile less than 0.0005.

After looking at [6] item frequency plots, it was decided to mine rules with a minimum support of 0.001, minumum confidence of 0.60, and perhaps most importantly a minumum length of 3. The goal here was to balance finding a large number of rules to prune from, but also to have at least 2 items as antecedent to see relationships between categories.

``` {R}
a2rules <- apriori(a2, parameter = list(support = 0.001, confidence = 0.6,
                                        minlen = 3, maxlen = 5))

summary(a2rules)
set of 1111 rules

rule length distribution (lhs + rhs):sizes
  3   4   5 
774 325  12 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.314   4.000   5.000 

summary of quality measures:
    support           confidence          lift         
 Min.   :0.001000   Min.   :0.6012   Min.   :  0.8921  
 1st Qu.:0.001215   1st Qu.:0.7657   1st Qu.:  1.2997  
 Median :0.001676   Median :0.9494   Median :  1.4839  
 Mean   :0.002728   Mean   :0.8830   Mean   : 11.9380  
 3rd Qu.:0.003037   3rd Qu.:0.9982   3rd Qu.: 10.8407  
 Max.   :0.039766   Max.   :1.0000   Max.   :288.8339  

mining info:
 data ntransactions support confidence
   a2       6959847   0.001        0.6
```

This yielded a set of 1111 rules, with good rule length distribution. Confidence and lift value distributions also seemed promising.

# Visualizing the Rules

First - a few matrices to see where the quality measures lie. Then some grouped matrices with controls at 100, 75, 50, 25, 10. 

## Itemsets in Consequent (RHS)

``` {R}
 [1] "{usd=$0.00-$190}"           "{l=China}"                  "{l=UK}"                     "{l=Agora/Internet/Torland}"
 [5] "{l=USA}"                    "{l=Denmark}"                "{l=Australia}"              "{l=EU}"                    
 [9] "{l=Sweden}"                 "{l=Canada}"                 "{l=No Info}"                "{usd=$190-$558}"           
[13] "{v=Bigdeal100}"             "{l=Poland}"                 "{v=b1g1mpact}"              "{l=Undeclared}"            
[17] "{v=MrCronk}"                "{v=only}"                   "{l=Netherlands}"            "{l=Germany}"               
[21] "{l=India}"                  "{v=optiman}"                "{v=rc4me}"                  "{v=wakeside917}"           
[25] "{v=captainkirk}"            "{l=Worldwide}"              "{v=mssource}"               "{l=Hong Kong}"             
[29] "{v=RepAAA}"                 "{v=indianpilldaddy}"        "{v=sexyhomer}"              "{l=Belgium}"               
[33] "{v=profesorhouse}"          "{v=RXChemist}"
```





















