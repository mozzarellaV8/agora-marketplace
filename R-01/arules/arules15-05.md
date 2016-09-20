# Association Rules (x < $50,000)

file: `arules2015-05.R`

- Subset all listings <= $500,000
- select - vendor, product, category, subcategory, all categories, and location
- read in as transactions from csv

```{r}
v2b <-read.transactions("v15b.csv", rm.duplicates=TRUE, format = "basket", sep = ",")
distribution of transactions with duplicates:
    1 
39547 
> summary(v2b)
transactions as itemMatrix in sparse format with
 1132477 rows (elements/itemsets/transactions) and
 1196225 columns (items) and a density of 0.000005822542 

most frequent items:
      Drugs         USA     No Info    Cannabis Info/eBooks     (Other) 
     797651      250337      217393      201413      136828     6284160 

element (itemset/transaction) length distribution:
sizes
      6       7 
  39557 1092920 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  6.000   7.000   7.000   6.965   7.000   7.000 

includes extended item information - examples:
                                          labels
1                                              -
2 - Commercial Ketama Hash - good quality - 100g
3  - Commercial Ketama Hash - good quality - 10g
```
Things were seeming good with the item naming, but after using apriori to mine itemsets, we have the same problem again with 'c=Drugs' instead of just 'Drugs.'

```{r}
summary(v2items)
set of 175 itemsets

most frequent items:
   c=Drugs      l=USA s=Cannabis  l=No Info  s=Ecstasy    (Other) 
       108         37         28         25         19        349 

element (itemset/transaction) length distribution:sizes
  3   4   5 
136  37   2 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.234   3.000   5.000 

summary of quality measures:
    support        
 Min.   :0.007268  
 1st Qu.:0.008272  
 Median :0.010874  
 Mean   :0.016044  
 3rd Qu.:0.015795  
 Max.   :0.161460  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions support confidence
   v2         70717  0.0071          1
```   

Wow, big set of rules - and most of them have 3 lhs items. I thought it was because I loosened the restriction on minlen being 3. It's actually because the support is far too low at '0.001'. I went back and set it to 0.007. 

```{r}

summary(v2rules)
set of 509 rules

rule length distribution (lhs + rhs):sizes
  2   3   4   5 
137 246 116  10 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   3.000   2.998   3.000   5.000 

summary of quality measures:
    support           confidence          lift         
 Min.   :0.007056   Min.   :0.6002   Min.   :  0.8185  
 1st Qu.:0.008428   1st Qu.:0.9809   1st Qu.:  1.2790  
 Median :0.010874   Median :1.0000   Median :  9.7541  
 Mean   :0.019356   Mean   :0.9420   Mean   : 23.0931  
 3rd Qu.:0.017209   3rd Qu.:1.0000   3rd Qu.: 35.3408  
 Max.   :0.266654   Max.   :1.0000   Max.   :126.9605  

mining info:
 data ntransactions support confidence
   v2         70717   0.007        0.6
```   

Confidence loks strong for the most part, and I like the rule length distibution. Let's see if leads to something interesting or repetitive. Here are the consequent itemsets that should correspond to the following matrix and group plots.

```
 [1] "{s=Money}"                      "{a=Services, Money}"           
 [3] "{c=Services}"                   "{l=No Info}"                   
 [5] "{s=Dissociatives}"              "{c=Drugs}"                     
 [7] "{s=Opioids}"                    "{s=Pirated}"                   
 [9] "{a=Data, Pirated}"              "{c=Data}"                      
[11] "{a=Drugs, Prescription}"        "{s=Prescription}"              
[13] "{s=Psychedelics}"               "{l=China}"                     
[15] "{a=Counterfeits, Watches}"      "{v=sexyhomer}"                 
[17] "{s=Watches}"                    "{c=Counterfeits}"              
[19] "{l=USA}"                        "{s=Cannabis}"                  
[21] "{s=Making money}"               "{a=Info/eBooks, Making money}" 
[23] "{c=Info/eBooks}"                "{a=Information, eBooks}"       
[25] "{s=eBooks}"                     "{c=Information}"               
[27] "{a=Data, Accounts}"             "{s=Accounts}"                  
[29] "{a=Information, Guides}"        "{s=Guides}"                    
[31] "{a=Other}"                      "{c=Other}"                     
[33] "{s=-}"                          "{s=Stimulants}"                
[35] "{s=RCs}"                        "{a=Drugs, RCs}"                
[37] "{l=Internet}"                   "{s=Other}"                     
[39] "{a=Info/eBooks, Other}"         "{a=Drugs, Steroids}"           
[41] "{s=Steroids}"                   "{s=Benzos}"                    
[43] "{a=Drugs, Benzos}"              "{s=Ecstasy}"                   
[45] "{a=Drugs, Cannabis, Weed}"      "{v=RXChemist}"                 
[47] "{v=captainkirk}"                "{a=Drugs, Stimulants, Cocaine}"
[49] "{a=Drugs, Ecstasy, Pills}"     
```
