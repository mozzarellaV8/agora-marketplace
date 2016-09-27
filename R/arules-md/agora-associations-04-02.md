# Agora Associations 04-02

R script: _agora-associations-40-mining-02.R_

## Parameters

dataframe: **a3**: 2317353 observations of 4 variables:

- price - discretized into 6 bins
- location ('from')
- subcategory - 
- vendor - anonymized with SHA256 hashing algorithm

# Mining Rules

apriori algorithm:

| parameter  			|  value 	|
|-----------------------|-----------|
| minimum support   	|  0.0025  	|
| minumum confidence  	|  0.6		|
| min rule length		|  2		|
| max rule length		|  5 		|

apriori result:

| parameter  			|  value 		|
|-----------------------|---------------|
| yield				   	|  395 rules 	|
| rules length 2  		|  27			|
| rules length 3		|  200			|
| rules length 4		|  68			|


- yield: 395 rules
- rules length 2: 127
- rules length 3: 200
- rules length 4: 68
- max support: 0.054



```{r}

a3 <- subset(ag, select = c("p", "f", "sc", "v"))

a3rules <- apriori(a3, parameter = list(support = 0.0025, confidence = 0.6,
                                        minlen = 2, maxlen = 5))


summary(a3rules)

set of 395 rules

rule length distribution (lhs + rhs):sizes
  2   3   4 
127 200  68 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   2.000   3.000   2.851   3.000   4.000 

summary of quality measures:
    support           confidence          lift        
 Min.   :0.002524   Min.   :0.6011   Min.   :  1.284  
 1st Qu.:0.003277   1st Qu.:0.7594   1st Qu.:  3.881  
 Median :0.004357   Median :0.9069   Median :  6.103  
 Mean   :0.006679   Mean   :0.8662   Mean   : 19.707  
 3rd Qu.:0.007633   3rd Qu.:0.9859   3rd Qu.: 17.782  
 Max.   :0.053784   Max.   :1.0000   Max.   :292.835  

mining info:
 data ntransactions support confidence
   a3       2317353  0.0025        0.6
```


## top and bottom 10

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


