# Agora - All Data - Association Rules 02b

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

...and then renamed for clarity.

Summary after converting `a2` to transactions yields frequent itemsets in prices under $2000 USD, locations of USA and 'No Info'
, and the category of `Drugs`.

``` {R}
transactions as itemMatrix in sparse format with
 2319949 rows (elements/itemsets/transactions) and
 3407 columns (items) and a density of 0.001467567 

most frequent items:
   usd=$0 - $585          c=Drugs            l=USA        l=No Info usd=$585 - $1945 
         1964128          1607961           498196           407392           234350 
         (Other) 
         6887718 

element (itemset/transaction) length distribution:
sizes
      5 
2319949 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      5       5       5       5       5       5 

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

Mining frequent itemsets yields a large amount:

``` {R}

summary(a2items)
set of 5678 itemsets

most frequent items:
           usd=$0 - $585                  c=Drugs                    l=USA 
                    3234                     2648                      863 
               l=No Info l=Agora/Internet/Torland                  (Other) 
                     552                      352                    11882 

element (itemset/transaction) length distribution:sizes
   3    4    5 
3502 1855  321 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00    3.00    3.00    3.44    4.00    5.00 

summary of quality measures:
    support         
 Min.   :0.0004302  
 1st Qu.:0.0005647  
 Median :0.0008328  
 Mean   :0.0018095  
 3rd Qu.:0.0015712  
 Max.   :0.1546831  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions support confidence
   a2       2319949 0.00043          1

```

But is rich with possibilities in length distributions, which can hopefully provide clues on dataset structure. Unfortunately support remains rather low, with a max value just under 0.04 and most values up to the 3rd quantile less than 0.0005.

After looking at [6] item frequency plots, it was decided to mine rules with a minimum support of 0.001, minumum confidence of 0.60, and perhaps most importantly a minumum length of 3. The goal here was to balance finding a large number of rules to prune from, but also to have at least 2 items as antecedent to see relationships between categories.

``` {R}
a2rules <- apriori(a2, parameter = list(support = 0.001, confidence = 0.6,
                                        minlen = 3, maxlen = 5))

summary(a2rules)
set of 4532 rules

rule length distribution (lhs + rhs):sizes
   3    4    5 
2271 1826  435 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.595   4.000   5.000 

summary of quality measures:
    support           confidence          lift        
 Min.   :0.001000   Min.   :0.6003   Min.   :  0.709  
 1st Qu.:0.001262   1st Qu.:0.9286   1st Qu.:  1.181  
 Median :0.001828   Median :0.9983   Median :  3.565  
 Mean   :0.003448   Mean   :0.9431   Mean   : 24.713  
 3rd Qu.:0.003270   3rd Qu.:1.0000   3rd Qu.: 21.292  
 Max.   :0.154683   Max.   :1.0000   Max.   :603.581  

mining info:
 data ntransactions support confidence
   a2       2319949   0.001        0.6
```

This yielded a set of 1111 rules, with good rule length distribution. Confidence and lift value distributions also seemed promising.

{v=Jack_N_Hoff,c=Drugs}         => {l=China}

# Visualizing the Rules

First - a few matrices to see where the quality measures lie. Then some grouped matrices with controls at 100, 75, 50, 25, 10. 

## Itemsets in Consequent (RHS)

``` {R}
Itemsets in Consequent (RHS)
 [1] "{c=Drugs}"                          "{l=USA}"                           
 [3] "{l=China}"                          "{l=UK}"                            
 [5] "{usd=$0 - $585}"                    "{c=Counterfeits}"                  
 [7] "{l=Sweden}"                         "{l=Agora/Internet/Torland}"        
 [9] "{c=Drug paraphernalia}"             "{ac=Drugs, Cannabis, Weed}"        
[11] "{usd=$585 - $1945}"                 "{l=Canada}"                        
[13] "{c=Forgeries}"                      "{l=Netherlands}"                   
[15] "{l=Italy}"                          "{l=India}"                         
[17] "{l=Germany}"                        "{l=No Info}"                       
[19] "{ac=Drugs, Benzos}"                 "{l=Denmark}"                       
[21] "{l=France}"                         "{l=Australia}"                     
[23] "{ac=Drugs, Steroids}"               "{c=Info/eBooks}"                   
[25] "{l=EU}"                             "{ac=Counterfeits, Watches}"        
[27] "{v=AsianVixen}"                     "{l=Hong Kong}"                     
[29] "{ac=Drugs, Psychedelics, LSD}"      "{v=optiman}"                       
[31] "{ac=Drugs, Ecstasy, Pills}"         "{c=Weapons}"                       
[33] "{ac=Drugs, Cannabis, Seeds}"        "{v=diamonddweller}"                
[35] "{c=Tobacco}"                        "{v=ES-Light}"                      
[37] "{ac=Tobacco, Paraphernalia}"        "{c=Data}"                          
[39] "{ac=Data, Accounts}"                "{ac=Tobacco, Smoked}"              
[41] "{v=ShopAgent}"                      "{ac=Data, Software}"               
[43] "{v=mikehamer}"                      "{ac=Counterfeits, Clothing}"       
[45] "{v=AsianVixene}"                    "{ac=Weapons, Lethal firearms}"     
[47] "{v=klosterbier}"                    "{ac=Drugs, Cannabis, Edibles}"     
[49] "{ac=Forgeries, Scans/Photos}"       "{ac=Drugs, Cannabis, Concentrates}"
[51] "{v=chipzahoy}"                      "{c=Services}"                      
[53] "{v=TheDigital}"                     "{ac=Drugs, RCs}"                   
[55] "{l=Undeclared}"                     "{v=MrCronk}"                       
[57] "{l=Poland}"                         "{v=b1g1mpact}"                     
[59] "{ac=Drug paraphernalia, Pipes}"     "{ac=Forgeries, Physical documents}"
[61] "{ac=Services, Money}"               "{c=Information}"                   
[63] "{ac=Information, Guides}"           "{v=Optumis}"                       
[65] "{v=captainkirk}"                    "{ac=Drugs, Ecstasy, MDMA}"         
[67] "{v=alchemycd}"                      "{ac=Drugs, Prescription}"          
[69] "{ac=Electronics, No Info/Other}"    "{c=Electronics}"                   
[71] "{v=smart666tiger}"                  "{v=only}"                          
[73] "{v=Montfort}"                       "{v=Fappy}"                         
[75] "{v=rc4me}"                          "{v=stiffstyles}"                   
[77] "{ac=Information, eBooks}"           "{ac=Info/eBooks, Other}"           
[79] "{ac=Info/eBooks, Making money}"     "{l=Worldwide}"                     
[81] "{v=mssource}"                       "{ac=Data, Pirated}"                
[83] "{v=wakeside917}"                    "{v=RepAAA}"                        
[85] "{v=Bigdeal100}"                     "{c=Jewelry}"                       
[87] "{ac=Jewelry, No Info/Other}"        "{v=indianpilldaddy}"               
[89] "{c=Other}"                          "{ac=Other, No Info/Other}"         
[91] "{v=sexyhomer}"                      "{l=Belgium}"                       
[93] "{v=profesorhouse}"                  "{v=RXChemist}"              
[33] "{v=profesorhouse}"                  "{v=RXChemist}"
```


SubRules1: by SCL, 128 top rules:

```{R}
Itemsets in Antecedent (LHS)
  [1] "{c=Drugs,l=USA}"                                             
  [2] "{usd=$0 - $585,l=USA}"                                       
  [3] "{usd=$0 - $585,ac=Drugs, Cannabis, Weed}"                    
  [4] "{c=Drugs,ac=Drugs, Cannabis, Weed}"                          
  [5] "{usd=$0 - $585,ac=Drugs, Prescription}"                      
  [6] "{c=Drugs,ac=Drugs, Prescription}"                            
  [7] "{c=Drugs,l=UK}"                                              
  [8] "{usd=$0 - $585,l=UK}"                                        
  [9] "{c=Drugs,l=No Info}"                                         
 [10] "{usd=$0 - $585,ac=Drugs, Benzos}"                            
 [11] "{c=Drugs,ac=Drugs, Benzos}"                                  
 [12] "{usd=$0 - $585,l=Australia}"                                 
 [13] "{c=Drugs,l=Australia}"                                       
 [14] "{c=Info/eBooks,l=No Info}"                                   
 [15] "{usd=$0 - $585,c=Info/eBooks}"                               
 [16] "{ac=Drugs, Cannabis, Weed,l=USA}"                            
 [17] "{usd=$0 - $585,ac=Drugs, Steroids}"                          
 [18] "{c=Drugs,ac=Drugs, Steroids}"                                
 [19] "{c=Drugs,l=EU}"                                              
 [20] "{usd=$0 - $585,l=EU}"                                        
 [21] "{usd=$0 - $585,l=Netherlands}"                               
 [22] "{c=Drugs,l=Netherlands}"                                     
 [23] "{usd=$0 - $585,l=Germany}"                                   
 [24] "{c=Drugs,l=Germany}"                                         
 [25] "{ac=Drugs, Prescription,l=No Info}"                          
 [26] "{usd=$0 - $585,ac=Drugs, Cannabis, Weed,l=USA}"              
 [27] "{c=Drugs,ac=Drugs, Cannabis, Weed,l=USA}"                    
 [28] "{usd=$0 - $585,ac=Info/eBooks, Other}"                       
 [29] "{c=Info/eBooks,ac=Info/eBooks, Other}"                       
 [30] "{usd=$0 - $585,l=Canada}"                                    
 [31] "{c=Drugs,l=Canada}"                                          
 [32] "{v=captainkirk,usd=$0 - $585}"                               
 [33] "{v=captainkirk,l=Agora/Internet/Torland}"                    
 [34] "{usd=$0 - $585,ac=Drugs, Prescription,l=No Info}"            
 [35] "{c=Drugs,ac=Drugs, Prescription,l=No Info}"                  
 [36] "{usd=$0 - $585,c=Drugs,l=No Info}"                           
 [37] "{v=rc4me,l=China}"                                           
 [38] "{v=rc4me,c=Drugs}"                                           
 [39] "{usd=$0 - $585,ac=Information, Guides}"                      
 [40] "{c=Information,ac=Information, Guides}"                      
 [41] "{usd=$0 - $585,ac=Drugs, Ecstasy, MDMA}"                     
 [42] "{c=Drugs,ac=Drugs, Ecstasy, MDMA}"                           
 [43] "{c=Info/eBooks,l=Agora/Internet/Torland}"                    
 [44] "{v=fake,l=No Info}"                                          
 [45] "{v=fake,usd=$0 - $585}"                                      
 [46] "{usd=$0 - $585,ac=Drugs, Ecstasy, Pills}"                    
 [47] "{c=Drugs,ac=Drugs, Ecstasy, Pills}"                          
 [48] "{usd=$0 - $585,ac=Drugs, Stimulants, Cocaine}"               
 [49] "{c=Drugs,ac=Drugs, Stimulants, Cocaine}"                     
 [50] "{c=Information,l=No Info}"                                   
 [51] "{usd=$0 - $585,ac=Counterfeits, Watches}"                    
 [52] "{c=Counterfeits,ac=Counterfeits, Watches}"                   
 [53] "{usd=$0 - $585,c=Counterfeits}"                              
 [54] "{v=RXChemist,ac=Drugs, Prescription}"                        
 [55] "{v=RXChemist,c=Drugs}"                                       
 [56] "{v=RXChemist,l=No Info}"                                     
 [57] "{v=RXChemist,ac=Drugs, Prescription,l=No Info}"              
 [58] "{v=RXChemist,c=Drugs,l=No Info}"                             
 [59] "{v=RXChemist,c=Drugs,ac=Drugs, Prescription}"                
 [60] "{usd=$0 - $585,ac=Information, eBooks}"                      
 [61] "{c=Information,ac=Information, eBooks}"                      
 [62] "{ac=Info/eBooks, Other,l=No Info}"                           
 [63] "{usd=$0 - $585,ac=Info/eBooks, Other,l=No Info}"             
 [64] "{c=Info/eBooks,ac=Info/eBooks, Other,l=No Info}"             
 [65] "{usd=$0 - $585,c=Info/eBooks,ac=Info/eBooks, Other}"         
 [66] "{v=RXChemist,usd=$0 - $585}"                                 
 [67] "{usd=$585 - $1945,l=USA}"                                    
 [68] "{usd=$0 - $585,ac=Drugs, Cannabis, Hash}"                    
 [69] "{c=Drugs,ac=Drugs, Cannabis, Hash}"                          
 [70] "{usd=$0 - $585,ac=Drugs, RCs}"                               
 [71] "{c=Drugs,ac=Drugs, RCs}"                                     
 [72] "{v=RXChemist,usd=$0 - $585,ac=Drugs, Prescription}"          
 [73] "{v=RXChemist,usd=$0 - $585,c=Drugs}"                         
 [74] "{v=RXChemist,usd=$0 - $585,l=No Info}"                       
 [75] "{c=Counterfeits,l=China}"                                    
 [76] "{v=RXChemist,usd=$0 - $585,ac=Drugs, Prescription,l=No Info}"
 [77] "{v=RXChemist,usd=$0 - $585,c=Drugs,l=No Info}"               
 [78] "{v=RXChemist,usd=$0 - $585,c=Drugs,ac=Drugs, Prescription}"  
 [79] "{v=RXChemist,c=Drugs,ac=Drugs, Prescription,l=No Info}"      
 [80] "{usd=$0 - $585,c=Drugs,ac=Drugs, Prescription,l=No Info}"    
 [81] "{c=Information,l=Agora/Internet/Torland}"                    
 [82] "{usd=$0 - $585,ac=Drugs, Cannabis, Concentrates}"            
 [83] "{c=Drugs,ac=Drugs, Cannabis, Concentrates}"                  
 [84] "{usd=$0 - $585,l=India}"                                     
 [85] "{c=Drugs,l=India}"                                           
 [86] "{ac=Information, Guides,l=No Info}"                          
 [87] "{usd=$0 - $585,ac=Information, Guides,l=No Info}"            
 [88] "{c=Information,ac=Information, Guides,l=No Info}"            
 [89] "{usd=$0 - $585,c=Information,l=No Info}"                     
 [90] "{usd=$0 - $585,c=Information,ac=Information, Guides}"        
 [91] "{usd=$0 - $585,ac=Drugs, Psychedelics, LSD}"                 
 [92] "{c=Drugs,ac=Drugs, Psychedelics, LSD}"                       
 [93] "{ac=Drugs, Ecstasy, Pills,l=Netherlands}"                    
 [94] "{usd=$0 - $585,ac=Other, No Info/Other}"                     
 [95] "{usd=$0 - $585,c=Other}"                                     
 [96] "{c=Other,ac=Other, No Info/Other}"                           
 [97] "{v=captainkirk,c=Info/eBooks}"                               
 [98] "{v=captainkirk,usd=$0 - $585,c=Info/eBooks}"                 
 [99] "{v=captainkirk,c=Info/eBooks,l=Agora/Internet/Torland}"      
[100] "{usd=$0 - $585,c=Info/eBooks,l=Agora/Internet/Torland}" 

Itemsets in Consequent (RHS)
 [1] "{usd=$0 - $585}"            "{c=Drugs}"                 
 [3] "{l=No Info}"                "{c=Info/eBooks}"           
 [5] "{l=Agora/Internet/Torland}" "{ac=Drugs, Prescription}"  
 [7] "{l=China}"                  "{c=Information}"           
 [9] "{c=Counterfeits}"           "{ac=Counterfeits, Watches}"
[11] "{v=RXChemist}"              "{ac=Information, Guides}"  
[13] "{c=Other}"                  "{ac=Other, No Info/Other}" 
[15] "{v=captainkirk}"
```























