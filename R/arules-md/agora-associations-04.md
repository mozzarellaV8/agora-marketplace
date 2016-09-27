


transactions as itemMatrix in sparse format with
 30954 rows (elements/itemsets/transactions) and
 3394 columns (items) and a density of 0.001473188 

most frequent items:
    c=Drugs   p=$10-150       f=USA  p=$150-600 p=$600-2000     (Other) 
      25864       12000        9167        7904        4587       95248 

element (itemset/transaction) length distribution:
sizes
    5 
30954 

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



# Frequent Itemsets

``` {R}
a2items <- apriori(a2, parameter = list(target = "frequent",
                                        supp = 0.0025, minlen = 2, maxlen = 5))

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
 1st Qu.:0.003328  
 Median :0.004975  
 Mean   :0.010440  
 3rd Qu.:0.009304  
 Max.   :0.323157  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions support confidence
   a2         30954  0.0025          1

```


# Mine Association Rules

```{R}
a2rules <- apriori(a2, parameter = list(support = 0.0025, confidence = 0.6,
                                        minlen = 2, maxlen = 5))

summary(a2rules)
set of 389 rules

rule length distribution (lhs + rhs):sizes
  2   3   4 
 91 245  53 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   3.000   3.000   2.902   3.000   4.000 

summary of quality measures:
    support           confidence          lift        
 Min.   :0.002520   Min.   :0.6028   Min.   : 0.7288  
 1st Qu.:0.003328   1st Qu.:0.9640   1st Qu.: 1.1968  
 Median :0.005137   Median :1.0000   Median : 1.1968  
 Mean   :0.012686   Mean   :0.9530   Mean   : 6.4751  
 3rd Qu.:0.010500   3rd Qu.:1.0000   3rd Qu.: 1.1968  
 Max.   :0.323157   Max.   :1.0000   Max.   :96.7313  

mining info:
 data ntransactions support confidence
   a2         30954  0.0025        0.6
```

``` {R}
arules::inspect(head(a2rules, 20))
   lhs                         rhs             support     confidence lift     
1  {v=694969}               => {c=Drugs}       0.002519868 0.9512195   1.138418
2  {f=Norway}               => {c=Drugs}       0.002519868 0.9512195   1.138418
3  {sc=Psychedelics-5-MeO}  => {c=Drugs}       0.002649092 1.0000000   1.196799
4  {f=Ireland}              => {c=Drugs}       0.002681398 1.0000000   1.196799
5  {sc=Scans/Photos}        => {c=Forgeries}   0.002713704 1.0000000  75.682152
6  {v=251510}               => {c=Drugs}       0.002778316 0.9885057   1.183042
7  {sc=Pirated}             => {c=Data}        0.002842928 1.0000000  72.491803
8  {v=189448}               => {f=Worldwide}   0.002810622 0.9886364  39.486774
9  {v=189448}               => {c=Drugs}       0.002810622 0.9886364   1.183199
10 {v=e39480}               => {c=Drugs}       0.002842928 0.9777778   1.170203
11 {v=035591}               => {c=Drugs}       0.002907540 1.0000000   1.196799
12 {sc=Cannabis-Seeds}      => {c=Drugs}       0.002939846 1.0000000   1.196799
13 {sc=Cannabis-Shake/trim} => {c=Drugs}       0.002972152 1.0000000   1.196799
14 {v=790426}               => {f=No Info}     0.002778316 0.9347826   6.798699
15 {sc=Opioids-Codeine}     => {c=Drugs}       0.003004458 1.0000000   1.196799
16 {sc=Dissociatives-GHB}   => {c=Drugs}       0.003133682 1.0000000   1.196799
17 {sc=Making money}        => {c=Info/eBooks} 0.003295212 1.0000000  59.526923
18 {sc=Software}            => {c=Data}        0.003876720 1.0000000  72.491803
19 {f=Switzerland}          => {c=Drugs}       0.003941332 0.9838710   1.177495
20 {sc=Psychedelics-Other}  => {c=Drugs}       0.004296698 1.0000000   1.196799
```

```{R}
    lhs                                          rhs       support     confidence lift    
370 {p=$600-2000,f=USA,sc=Stimulants-Cocaine} => {c=Drugs} 0.003262906 1          1.196799
371 {p=$150-600,f=USA,sc=Stimulants-Cocaine}  => {c=Drugs} 0.004845900 1          1.196799
372 {p=$10-150,f=USA,sc=Stimulants-Cocaine}   => {c=Drugs} 0.006041222 1          1.196799
373 {p=$10-150,f=UK,sc=Ecstasy-MDMA}          => {c=Drugs} 0.002584480 1          1.196799
374 {p=$150-600,f=Australia,sc=Ecstasy-MDMA}  => {c=Drugs} 0.003553660 1          1.196799
375 {p=$10-150,f=Australia,sc=Ecstasy-MDMA}   => {c=Drugs} 0.003198294 1          1.196799
376 {p=$600-2000,f=USA,sc=Ecstasy-MDMA}       => {c=Drugs} 0.003133682 1          1.196799
377 {p=$150-600,f=USA,sc=Ecstasy-MDMA}        => {c=Drugs} 0.004555146 1          1.196799
378 {p=$10-150,f=USA,sc=Ecstasy-MDMA}         => {c=Drugs} 0.006041222 1          1.196799
379 {p=$2000-10000,f=USA,sc=Cannabis-Weed}    => {c=Drugs} 0.005362796 1          1.196799
380 {p=$150-600,f=UK,sc=Cannabis-Weed}        => {c=Drugs} 0.003489048 1          1.196799
381 {p=$10-150,f=UK,sc=Cannabis-Weed}         => {c=Drugs} 0.004716676 1          1.196799
382 {p=$10-150,f=Australia,sc=Cannabis-Weed}  => {c=Drugs} 0.002778316 1          1.196799
383 {p=$0-10,f=USA,sc=Cannabis-Weed}          => {c=Drugs} 0.003489048 1          1.196799
384 {p=$600-2000,f=No Info,sc=Cannabis-Weed}  => {c=Drugs} 0.002649092 1          1.196799
385 {p=$150-600,f=No Info,sc=Cannabis-Weed}   => {c=Drugs} 0.003909026 1          1.196799
386 {p=$10-150,f=No Info,sc=Cannabis-Weed}    => {c=Drugs} 0.003715190 1          1.196799
387 {p=$600-2000,f=USA,sc=Cannabis-Weed}      => {c=Drugs} 0.008464173 1          1.196799
388 {p=$150-600,f=USA,sc=Cannabis-Weed}       => {c=Drugs} 0.011533243 1          1.196799
389 {p=$10-150,f=USA,sc=Cannabis-Weed}        => {c=Drugs} 0.011436325 1          1.196799
```

```{r}
                                     lhs                   rhs     support confidence     lift
101                  {p=$0-10,sc=eBooks} =>    {c=Information} 0.002519868  0.6842105 56.78030
102              {f=No Info,sc=Accounts} =>           {c=Data} 0.002810622  1.0000000 72.49180
103              {p=$10-150,sc=Accounts} =>           {c=Data} 0.003133682  1.0000000 72.49180
104 {f=Agora/Internet/Torland,sc=Guides} =>    {c=Information} 0.002875234  1.0000000 82.98660
105                  {p=$0-10,sc=Guides} =>    {c=Information} 0.002939846  1.0000000 82.98660
106                {f=No Info,sc=Guides} =>    {c=Information} 0.003069070  1.0000000 82.98660
107            {f=No Info,c=Information} =>        {sc=Guides} 0.003069070  0.6375839 87.32642
108                {p=$10-150,sc=Guides} =>    {c=Information} 0.003133682  1.0000000 82.98660
109            {p=$10-150,c=Information} =>        {sc=Guides} 0.003133682  0.6554054 89.76734
110                   {f=USA,c=Listings} => {sc=No Info/Other} 0.002746010  1.0000000 21.61592
111               {p=$10-150,c=Listings} => {sc=No Info/Other} 0.002875234  1.0000000 21.61592
```



```{r}
# subset: Cannabis -----------------------------------------------------------

cannabis <- subset(a2rules, rhs %in% "sc=Cannabis-Weed" | lhs %in% "sc=Cannabis-Weed")
inspect(cannabis)

    lhs                                         rhs       support     confidence lift    
85  {sc=Cannabis-Weed}                       => {c=Drugs} 0.111843381 1          1.196799
262 {f=EU,sc=Cannabis-Weed}                  => {c=Drugs} 0.005039736 1          1.196799
270 {f=Canada,sc=Cannabis-Weed}              => {c=Drugs} 0.009368741 1          1.196799
278 {f=Germany,sc=Cannabis-Weed}             => {c=Drugs} 0.008690315 1          1.196799
286 {f=Netherlands,sc=Cannabis-Weed}         => {c=Drugs} 0.005718162 1          1.196799
311 {p=$2000-10000,sc=Cannabis-Weed}         => {c=Drugs} 0.011920915 1          1.196799
314 {f=UK,sc=Cannabis-Weed}                  => {c=Drugs} 0.011274795 1          1.196799
319 {f=Australia,sc=Cannabis-Weed}           => {c=Drugs} 0.006945791 1          1.196799
324 {p=$0-10,sc=Cannabis-Weed}               => {c=Drugs} 0.010144085 1          1.196799
325 {f=No Info,sc=Cannabis-Weed}             => {c=Drugs} 0.012857789 1          1.196799
326 {p=$600-2000,sc=Cannabis-Weed}           => {c=Drugs} 0.020482006 1          1.196799
327 {p=$150-600,sc=Cannabis-Weed}            => {c=Drugs} 0.031466046 1          1.196799
328 {f=USA,sc=Cannabis-Weed}                 => {c=Drugs} 0.040867093 1          1.196799
329 {p=$10-150,sc=Cannabis-Weed}             => {c=Drugs} 0.036344253 1          1.196799
365 {p=$150-600,f=Canada,sc=Cannabis-Weed}   => {c=Drugs} 0.002584480 1          1.196799
366 {p=$10-150,f=Canada,sc=Cannabis-Weed}    => {c=Drugs} 0.002519868 1          1.196799
367 {p=$10-150,f=Germany,sc=Cannabis-Weed}   => {c=Drugs} 0.003133682 1          1.196799
379 {p=$2000-10000,f=USA,sc=Cannabis-Weed}   => {c=Drugs} 0.005362796 1          1.196799
380 {p=$150-600,f=UK,sc=Cannabis-Weed}       => {c=Drugs} 0.003489048 1          1.196799
381 {p=$10-150,f=UK,sc=Cannabis-Weed}        => {c=Drugs} 0.004716676 1          1.196799
382 {p=$10-150,f=Australia,sc=Cannabis-Weed} => {c=Drugs} 0.002778316 1          1.196799
383 {p=$0-10,f=USA,sc=Cannabis-Weed}         => {c=Drugs} 0.003489048 1          1.196799
384 {p=$600-2000,f=No Info,sc=Cannabis-Weed} => {c=Drugs} 0.002649092 1          1.196799
385 {p=$150-600,f=No Info,sc=Cannabis-Weed}  => {c=Drugs} 0.003909026 1          1.196799
386 {p=$10-150,f=No Info,sc=Cannabis-Weed}   => {c=Drugs} 0.003715190 1          1.196799
387 {p=$600-2000,f=USA,sc=Cannabis-Weed}     => {c=Drugs} 0.008464173 1          1.196799
388 {p=$150-600,f=USA,sc=Cannabis-Weed}      => {c=Drugs} 0.011533243 1          1.196799
389 {p=$10-150,f=USA,sc=Cannabis-Weed}       => {c=Drugs} 0.011436325 1          1.196799

```

```{r}
# subset: prices  ------------------------------------------------------------
inspect(price010)
    lhs                                         rhs                support     confidence lift      
86  {p=$0-10}                                => {c=Drugs}          0.077954384 0.6395441   0.7654055
101 {p=$0-10,sc=eBooks}                      => {c=Information}    0.002519868 0.6842105  56.7803020
105 {p=$0-10,sc=Guides}                      => {c=Information}    0.002939846 1.0000000  82.9865952
146 {f=Agora/Internet/Torland,c=Info/eBooks} => {p=$0-10}          0.004329004 0.6700000   5.4967347
151 {p=$0-10,sc=Stimulants-Prescription}     => {c=Drugs}          0.002778316 1.0000000   1.1967986
163 {p=$0-10,c=Other}                        => {sc=No Info/Other} 0.005136654 1.0000000  21.6159218
164 {p=$0-10,sc=No Info/Other}               => {c=Other}          0.005136654 0.6411290  27.8729046
172 {p=$0-10,sc=Money}                       => {c=Services}       0.004813594 0.8370787  25.1562452
192 {p=$0-10,sc=Stimulants-Speed}            => {c=Drugs}          0.002746010 1.0000000   1.1967986
207 {p=$0-10,sc=Psychedelics-LSD}            => {c=Drugs}          0.003327518 1.0000000   1.1967986
218 {p=$0-10,sc=Cannabis-Hash}               => {c=Drugs}          0.003101376 1.0000000   1.1967986
226 {p=$0-10,sc=Opioids}                     => {c=Drugs}          0.002746010 1.0000000   1.1967986
234 {p=$0-10,sc=Prescription}                => {c=Drugs}          0.007333463 1.0000000   1.1967986
243 {p=$0-10,sc=Benzos}                      => {c=Drugs}          0.007010403 1.0000000   1.1967986
254 {p=$0-10,sc=Ecstasy-Pills}               => {c=Drugs}          0.004393616 1.0000000   1.1967986
263 {p=$0-10,f=EU}                           => {c=Drugs}          0.004781288 0.8705882   1.0419188
271 {p=$0-10,f=Canada}                       => {c=Drugs}          0.003650578 0.8759690   1.0483585
279 {p=$0-10,f=Germany}                      => {c=Drugs}          0.004038250 0.9057971   1.0840567
287 {p=$0-10,f=Netherlands}                  => {c=Drugs}          0.005427408 0.9710983   1.1622091
294 {p=$0-10,sc=Stimulants-Cocaine}          => {c=Drugs}          0.003650578 1.0000000   1.1967986
303 {p=$0-10,sc=Ecstasy-MDMA}                => {c=Drugs}          0.004070556 1.0000000   1.1967986
315 {p=$0-10,f=UK}                           => {c=Drugs}          0.010628675 0.8703704   1.0416581
320 {p=$0-10,f=Australia}                    => {c=Drugs}          0.006267364 0.7822581   0.9362054
324 {p=$0-10,sc=Cannabis-Weed}               => {c=Drugs}          0.010144085 1.0000000   1.1967986
330 {p=$0-10,f=USA}                          => {c=Drugs}          0.027233960 0.8216374   0.9833346
355 {p=$0-10,f=USA,sc=Prescription}          => {c=Drugs}          0.002810622 1.0000000   1.1967986
358 {p=$0-10,f=USA,sc=Benzos}                => {c=Drugs}          0.003198294 1.0000000   1.1967986
383 {p=$0-10,f=USA,sc=Cannabis-Weed}         => {c=Drugs}          0.003489048 1.0000000   1.1967986
```
# 10-150

The large set of rules for prices.

```{r}
inspect(price150)
    lhs                                             rhs                support     confidence lift      
25  {sc=Opioids-Buprenorphine}                   => {p=$10-150}        0.002746010 0.6028369   1.5550177
42  {c=Drug paraphernalia}                       => {p=$10-150}        0.004748982 0.6336207   1.6344246
91  {p=$10-150}                                  => {c=Drugs}          0.323156943 0.8335833   0.9976314
94  {p=$10-150,sc=Opioids-Buprenorphine}         => {c=Drugs}          0.002746010 1.0000000   1.1967986
95  {c=Drugs,sc=Opioids-Buprenorphine}           => {p=$10-150}        0.002746010 0.6028369   1.5550177
97  {p=$10-150,sc=Opioids-Morphine}              => {c=Drugs}          0.002519868 1.0000000   1.1967986
100 {p=$10-150,sc=Opioids-Hydrocodone}           => {c=Drugs}          0.002552174 1.0000000   1.1967986
103 {p=$10-150,sc=Accounts}                      => {c=Data}           0.003133682 1.0000000  72.4918033
108 {p=$10-150,sc=Guides}                        => {c=Information}    0.003133682 1.0000000  82.9865952
109 {p=$10-150,c=Information}                    => {sc=Guides}        0.003133682 0.6554054  89.7673403
111 {p=$10-150,c=Listings}                       => {sc=No Info/Other} 0.002875234 1.0000000  21.6159218
113 {p=$10-150,sc=Cannabis-Edibles}              => {c=Drugs}          0.004425922 1.0000000   1.1967986
114 {p=$10-150,sc=Psychedelics-DMT}              => {c=Drugs}          0.004329004 1.0000000   1.1967986
115 {p=$10-150,f=France}                         => {c=Drugs}          0.003424436 0.9549550   1.1428888
118 {p=$10-150,sc=Opioids-Fentanyl}              => {c=Drugs}          0.003295212 1.0000000   1.1967986
120 {p=$10-150,c=Electronics}                    => {sc=No Info/Other} 0.003650578 1.0000000  21.6159218
122 {p=$10-150,f=Sweden}                         => {c=Drugs}          0.003715190 0.9126984   1.0923162
123 {p=$10-150,sc=Psychedelics-2C}               => {c=Drugs}          0.004199780 1.0000000   1.1967986
126 {p=$10-150,sc=Ecstasy-Other}                 => {c=Drugs}          0.002972152 1.0000000   1.1967986
129 {p=$10-150,sc=Psychedelics-Mushrooms}        => {c=Drugs}          0.006202752 1.0000000   1.1967986
133 {p=$10-150,sc=Steroids}                      => {c=Drugs}          0.004975124 1.0000000   1.1967986
138 {p=$10-150,sc=RCs}                           => {c=Drugs}          0.004522840 1.0000000   1.1967986
142 {p=$10-150,sc=Dissociatives-Ketamine}        => {c=Drugs}          0.004781288 1.0000000   1.1967986
145 {p=$10-150,sc=Opioids-Other}                 => {c=Drugs}          0.007042709 1.0000000   1.1967986
150 {p=$10-150,sc=Opioids-Oxycodone}             => {c=Drugs}          0.007301157 1.0000000   1.1967986
154 {p=$10-150,sc=Stimulants-Prescription}       => {c=Drugs}          0.011274795 1.0000000   1.1967986
158 {p=$10-150,sc=Opioids-Heroin}                => {c=Drugs}          0.008044195 1.0000000   1.1967986
162 {p=$10-150,f=China}                          => {c=Drugs}          0.002584480 0.6611570   0.7912718
170 {p=$10-150,c=Other}                          => {sc=No Info/Other} 0.007624217 1.0000000  21.6159218
177 {p=$10-150,sc=Money}                         => {c=Services}       0.006138140 0.7509881  22.5690165
180 {p=$10-150,f=Worldwide}                      => {c=Drugs}          0.005233572 0.6183206   0.7400053
188 {p=$10-150,sc=Cannabis-Concentrates}         => {c=Drugs}          0.010596369 1.0000000   1.1967986
195 {p=$10-150,sc=Stimulants-Speed}              => {c=Drugs}          0.010951735 1.0000000   1.1967986
202 {p=$10-150,sc=Stimulants-Meth}               => {c=Drugs}          0.009885637 1.0000000   1.1967986
212 {p=$10-150,sc=Psychedelics-LSD}              => {c=Drugs}          0.011113265 1.0000000   1.1967986
223 {p=$10-150,sc=Cannabis-Hash}                 => {c=Drugs}          0.013310073 1.0000000   1.1967986
231 {p=$10-150,sc=Opioids}                       => {c=Drugs}          0.015862247 1.0000000   1.1967986
239 {p=$10-150,sc=Prescription}                  => {c=Drugs}          0.020740454 1.0000000   1.1967986
248 {p=$10-150,sc=Benzos}                        => {c=Drugs}          0.016120695 1.0000000   1.1967986
259 {p=$10-150,sc=Ecstasy-Pills}                 => {c=Drugs}          0.014537701 1.0000000   1.1967986
266 {p=$10-150,f=EU}                             => {c=Drugs}          0.016605285 0.8495868   1.0167843
274 {p=$10-150,f=Canada}                         => {c=Drugs}          0.017251405 0.9656420   1.1556790
282 {p=$10-150,f=Germany}                        => {c=Drugs}          0.021806552 0.9547383   1.1426295
290 {p=$10-150,f=Netherlands}                    => {c=Drugs}          0.018931317 0.9559543   1.1440848
299 {p=$10-150,sc=Stimulants-Cocaine}            => {c=Drugs}          0.021806552 1.0000000   1.1967986
308 {p=$10-150,sc=Ecstasy-MDMA}                  => {c=Drugs}          0.023001874 1.0000000   1.1967986
318 {p=$10-150,f=UK}                             => {c=Drugs}          0.036505783 0.9216966   1.1030852
323 {p=$10-150,f=Australia}                      => {c=Drugs}          0.032532144 0.8658641   1.0362650
329 {p=$10-150,sc=Cannabis-Weed}                 => {c=Drugs}          0.036344253 1.0000000   1.1967986
333 {p=$10-150,f=No Info}                        => {c=Drugs}          0.031756800 0.6249205   0.7479040
336 {p=$10-150,f=USA}                            => {c=Drugs}          0.110163468 0.9016393   1.0790807
337 {p=$10-150,f=USA,sc=Opioids-Other}           => {c=Drugs}          0.003327518 1.0000000   1.1967986
339 {p=$10-150,f=USA,sc=Opioids-Oxycodone}       => {c=Drugs}          0.003844414 1.0000000   1.1967986
341 {p=$10-150,f=USA,sc=Stimulants-Prescription} => {c=Drugs}          0.005847386 1.0000000   1.1967986
343 {p=$10-150,f=USA,sc=Opioids-Heroin}          => {c=Drugs}          0.003521354 1.0000000   1.1967986
348 {p=$10-150,f=USA,sc=Cannabis-Concentrates}   => {c=Drugs}          0.006331976 1.0000000   1.1967986
350 {p=$10-150,f=Australia,sc=Stimulants-Meth}   => {c=Drugs}          0.002552174 1.0000000   1.1967986
351 {p=$10-150,f=USA,sc=Stimulants-Meth}         => {c=Drugs}          0.003133682 1.0000000   1.1967986
352 {p=$10-150,f=USA,sc=Psychedelics-LSD}        => {c=Drugs}          0.002746010 1.0000000   1.1967986
354 {p=$10-150,f=USA,sc=Opioids}                 => {c=Drugs}          0.007010403 1.0000000   1.1967986
357 {p=$10-150,f=USA,sc=Prescription}            => {c=Drugs}          0.008044195 1.0000000   1.1967986
361 {p=$10-150,f=USA,sc=Benzos}                  => {c=Drugs}          0.006267364 1.0000000   1.1967986
363 {p=$10-150,f=Netherlands,sc=Ecstasy-Pills}   => {c=Drugs}          0.002972152 1.0000000   1.1967986
364 {p=$10-150,f=USA,sc=Ecstasy-Pills}           => {c=Drugs}          0.002584480 1.0000000   1.1967986
366 {p=$10-150,f=Canada,sc=Cannabis-Weed}        => {c=Drugs}          0.002519868 1.0000000   1.1967986
367 {p=$10-150,f=Germany,sc=Cannabis-Weed}       => {c=Drugs}          0.003133682 1.0000000   1.1967986
369 {p=$10-150,f=UK,sc=Stimulants-Cocaine}       => {c=Drugs}          0.003295212 1.0000000   1.1967986
372 {p=$10-150,f=USA,sc=Stimulants-Cocaine}      => {c=Drugs}          0.006041222 1.0000000   1.1967986
373 {p=$10-150,f=UK,sc=Ecstasy-MDMA}             => {c=Drugs}          0.002584480 1.0000000   1.1967986
375 {p=$10-150,f=Australia,sc=Ecstasy-MDMA}      => {c=Drugs}          0.003198294 1.0000000   1.1967986
378 {p=$10-150,f=USA,sc=Ecstasy-MDMA}            => {c=Drugs}          0.006041222 1.0000000   1.1967986
381 {p=$10-150,f=UK,sc=Cannabis-Weed}            => {c=Drugs}          0.004716676 1.0000000   1.1967986
382 {p=$10-150,f=Australia,sc=Cannabis-Weed}     => {c=Drugs}          0.002778316 1.0000000   1.1967986
386 {p=$10-150,f=No Info,sc=Cannabis-Weed}       => {c=Drugs}          0.003715190 1.0000000   1.1967986
389 {p=$10-150,f=USA,sc=Cannabis-Weed}           => {c=Drugs}          0.011436325 1.0000000   1.1967986
```

# 150-600

```{r}
inspect(price600)
    lhs                                              rhs                support     confidence lift      
89  {p=$150-600}                                  => {c=Drugs}          0.225205143 0.8819585   1.0555267
116 {p=$150-600,sc=Opioids-Fentanyl}              => {c=Drugs}          0.002552174 1.0000000   1.1967986
119 {p=$150-600,c=Electronics}                    => {sc=No Info/Other} 0.002584480 1.0000000  21.6159218
121 {p=$150-600,f=Sweden}                         => {c=Drugs}          0.002810622 0.9354839   1.1195858
124 {p=$150-600,sc=Ecstasy-Other}                 => {c=Drugs}          0.002649092 1.0000000   1.1967986
127 {p=$150-600,sc=Psychedelics-Mushrooms}        => {c=Drugs}          0.002616786 1.0000000   1.1967986
131 {p=$150-600,sc=Steroids}                      => {c=Drugs}          0.003618272 1.0000000   1.1967986
136 {p=$150-600,sc=RCs}                           => {c=Drugs}          0.003456742 1.0000000   1.1967986
141 {p=$150-600,sc=Dissociatives-Ketamine}        => {c=Drugs}          0.004167474 1.0000000   1.1967986
143 {p=$150-600,sc=Opioids-Other}                 => {c=Drugs}          0.003844414 1.0000000   1.1967986
148 {p=$150-600,sc=Opioids-Oxycodone}             => {c=Drugs}          0.004878206 1.0000000   1.1967986
152 {p=$150-600,sc=Stimulants-Prescription}       => {c=Drugs}          0.004490534 1.0000000   1.1967986
156 {p=$150-600,sc=Opioids-Heroin}                => {c=Drugs}          0.006138140 1.0000000   1.1967986
161 {p=$150-600,f=China}                          => {c=Drugs}          0.005427408 0.8704663   1.0417729
168 {p=$150-600,c=Other}                          => {sc=No Info/Other} 0.005168960 1.0000000  21.6159218
174 {p=$150-600,sc=Money}                         => {c=Services}       0.003327518 0.6959459  20.9148649
179 {p=$150-600,f=Worldwide}                      => {c=Drugs}          0.004587452 0.7319588   0.8760073
184 {p=$150-600,sc=Cannabis-Concentrates}         => {f=USA}            0.004748982 0.6255319   2.1122194
185 {p=$150-600,sc=Cannabis-Concentrates}         => {c=Drugs}          0.007591911 1.0000000   1.1967986
194 {p=$150-600,sc=Stimulants-Speed}              => {c=Drugs}          0.008044195 1.0000000   1.1967986
200 {p=$150-600,sc=Stimulants-Meth}               => {c=Drugs}          0.008528785 1.0000000   1.1967986
210 {p=$150-600,sc=Psychedelics-LSD}              => {c=Drugs}          0.008076501 1.0000000   1.1967986
213 {p=$150-600,sc=Other}                         => {c=Drugs}          0.004070556 0.6774194   0.8107346
221 {p=$150-600,sc=Cannabis-Hash}                 => {c=Drugs}          0.008722621 1.0000000   1.1967986
229 {p=$150-600,sc=Opioids}                       => {c=Drugs}          0.011048653 1.0000000   1.1967986
237 {p=$150-600,sc=Prescription}                  => {c=Drugs}          0.009336435 1.0000000   1.1967986
246 {p=$150-600,sc=Benzos}                        => {c=Drugs}          0.010757899 1.0000000   1.1967986
257 {p=$150-600,sc=Ecstasy-Pills}                 => {c=Drugs}          0.012405505 1.0000000   1.1967986
265 {p=$150-600,f=EU}                             => {c=Drugs}          0.010208697 0.8272251   0.9900219
273 {p=$150-600,f=Canada}                         => {c=Drugs}          0.012793177 0.9542169   1.1420054
281 {p=$150-600,f=Germany}                        => {c=Drugs}          0.014699231 0.9518828   1.1392121
289 {p=$150-600,f=Netherlands}                    => {c=Drugs}          0.015442269 0.9637097   1.1533664
297 {p=$150-600,sc=Stimulants-Cocaine}            => {c=Drugs}          0.019771274 1.0000000   1.1967986
306 {p=$150-600,sc=Ecstasy-MDMA}                  => {c=Drugs}          0.019448213 1.0000000   1.1967986
317 {p=$150-600,f=UK}                             => {c=Drugs}          0.022097306 0.9408528   1.1260114
322 {p=$150-600,f=Australia}                      => {c=Drugs}          0.025230988 0.9188235   1.0996467
327 {p=$150-600,sc=Cannabis-Weed}                 => {c=Drugs}          0.031466046 1.0000000   1.1967986
332 {p=$150-600,f=No Info}                        => {c=Drugs}          0.022646508 0.7031093   0.8414803
335 {p=$150-600,f=USA}                            => {c=Drugs}          0.070717839 0.9306973   1.1138572
338 {p=$150-600,f=USA,sc=Opioids-Oxycodone}       => {c=Drugs}          0.002552174 1.0000000   1.1967986
340 {p=$150-600,f=USA,sc=Stimulants-Prescription} => {c=Drugs}          0.002681398 1.0000000   1.1967986
342 {p=$150-600,f=USA,sc=Opioids-Heroin}          => {c=Drugs}          0.002519868 1.0000000   1.1967986
346 {p=$150-600,f=USA,sc=Cannabis-Concentrates}   => {c=Drugs}          0.004748982 1.0000000   1.1967986
347 {p=$150-600,c=Drugs,sc=Cannabis-Concentrates} => {f=USA}            0.004748982 0.6255319   2.1122194
349 {p=$150-600,f=Australia,sc=Stimulants-Meth}   => {c=Drugs}          0.002552174 1.0000000   1.1967986
353 {p=$150-600,f=USA,sc=Opioids}                 => {c=Drugs}          0.004458228 1.0000000   1.1967986
356 {p=$150-600,f=USA,sc=Prescription}            => {c=Drugs}          0.003812108 1.0000000   1.1967986
360 {p=$150-600,f=USA,sc=Benzos}                  => {c=Drugs}          0.004522840 1.0000000   1.1967986
362 {p=$150-600,f=Netherlands,sc=Ecstasy-Pills}   => {c=Drugs}          0.002907540 1.0000000   1.1967986
365 {p=$150-600,f=Canada,sc=Cannabis-Weed}        => {c=Drugs}          0.002584480 1.0000000   1.1967986
368 {p=$150-600,f=UK,sc=Stimulants-Cocaine}       => {c=Drugs}          0.002552174 1.0000000   1.1967986
371 {p=$150-600,f=USA,sc=Stimulants-Cocaine}      => {c=Drugs}          0.004845900 1.0000000   1.1967986
374 {p=$150-600,f=Australia,sc=Ecstasy-MDMA}      => {c=Drugs}          0.003553660 1.0000000   1.1967986
377 {p=$150-600,f=USA,sc=Ecstasy-MDMA}            => {c=Drugs}          0.004555146 1.0000000   1.1967986
380 {p=$150-600,f=UK,sc=Cannabis-Weed}            => {c=Drugs}          0.003489048 1.0000000   1.1967986
385 {p=$150-600,f=No Info,sc=Cannabis-Weed}       => {c=Drugs}          0.003909026 1.0000000   1.1967986
388 {p=$150-600,f=USA,sc=Cannabis-Weed}           => {c=Drugs}          0.011533243 1.0000000   1.1967986
```

# 2000-10000

```{r} 
    lhs                                       rhs       support     confidence lift     
82  {p=$2000-10000}                        => {c=Drugs} 0.066453447 0.8931828  1.0689600
159 {p=$2000-10000,f=China}                => {c=Drugs} 0.004490534 0.9652778  1.1552431
196 {p=$2000-10000,sc=Stimulants-Meth}     => {c=Drugs} 0.003133682 1.0000000  1.1967986
205 {p=$2000-10000,sc=Psychedelics-LSD}    => {c=Drugs} 0.002939846 1.0000000  1.1967986
240 {p=$2000-10000,sc=Benzos}              => {c=Drugs} 0.003359824 1.0000000  1.1967986
251 {p=$2000-10000,sc=Ecstasy-Pills}       => {c=Drugs} 0.004878206 1.0000000  1.1967986
269 {p=$2000-10000,f=Canada}               => {c=Drugs} 0.003941332 0.9682540  1.1588050
277 {p=$2000-10000,f=Germany}              => {c=Drugs} 0.005976610 0.9585492  1.1471904
285 {p=$2000-10000,f=Netherlands}          => {c=Drugs} 0.006913485 0.9639640  1.1536708
291 {p=$2000-10000,sc=Stimulants-Cocaine}  => {c=Drugs} 0.006913485 1.0000000  1.1967986
300 {p=$2000-10000,sc=Ecstasy-MDMA}        => {c=Drugs} 0.007559605 1.0000000  1.1967986
309 {p=$2000-10000,f=UK}                   => {c=Drugs} 0.004878206 0.9805195  1.1734844
310 {p=$2000-10000,f=Australia}            => {c=Drugs} 0.006719649 0.9244444  1.1063739
311 {p=$2000-10000,sc=Cannabis-Weed}       => {c=Drugs} 0.011920915 1.0000000  1.1967986
312 {p=$2000-10000,f=No Info}              => {c=Drugs} 0.007204239 0.7216828  0.8637091
313 {p=$2000-10000,f=USA}                  => {c=Drugs} 0.017832913 0.9340102  1.1178221
379 {p=$2000-10000,f=USA,sc=Cannabis-Weed} => {c=Drugs} 0.005362796 1.0000000  1.1967986
```























