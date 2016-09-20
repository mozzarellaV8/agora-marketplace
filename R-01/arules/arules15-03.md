# Association Rules (x < $40,000)

file: `arules2015-03`

## v2 - unique observations under 40k USD

## vendor, product, category, and location

### v2:
```{r}
# subset under 40k ---------------------------------------
v15b <- subset(v15, v15$usd <= 40000) # 1132307
quantile(v15b$usd)
# 75% is at $265

plot(v15b$usd, main = "Agora 2015: Prices under 40k USD")
v15b$usd <- discretize(v15b$usd, method = "cluster", categories = 10)
levels(v15b$usd)
# [1] "[    0.0000177,  359.4984838)" "[  359.4984838, 1139.7731641)" "[ 1139.7731641, 2372.9194491)"
# [4] "[ 2372.9194491, 4422.8807377)" "[ 4422.8807377, 7761.6439127)" "[ 7761.6439127,12970.8810281)"
# [7] "[12970.8810281,22015.2079275)" "[22015.2079275,39965.7600000

v15b <- subset(v15b, select = c("vendor", "product", "allcat", "from"))
colnames(v15b) <- c("vendor", "product", "category", "location")
head(v15b)

v2 <- v15b[!duplicated(v15b), ] # 70703
head(v2)
v2b <- v15b

# convert to transactions -----------------------------------------------------
v2 <- as(v2, "transactions")
summary(v2)
transactions as itemMatrix in sparse format with
 70703 rows (elements/itemsets/transactions) and
 63752 columns (items) and a density of 0.00006274313 

most frequent items:
                    from=USA allcat=Drugs, Cannabis, Weed                 from=No Info                      from=UK               from=Australia 
                       20395                        11417                         9860                         7031                         5670 
                     (Other) 
                      228439 

element (itemset/transaction) length distribution:
sizes
    4 
70703 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      4       4       4       4       4       4 

includes extended item information - examples:
             labels variables     levels
1 vendor=-Euphoria-    vendor -Euphoria-
2     vendor=-FliP-    vendor     -FliP-
3   vendor=-Inanna-    vendor   -Inanna-

includes extended transaction information - examples:
  transactionID
1             1
2             2
3             3
```

Had to loosen up the support to get 173 rules.

``` {r}
> summary(v2items)
set of 173 itemsets

most frequent items:
                    from=USA allcat=Drugs, Cannabis, Weed                 from=No Info                   from=China                      from=UK 
                          44                           43                           38                           12                           12 
                     (Other) 
                         370 

element (itemset/transaction) length distribution:sizes
  3 
173 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      3       3       3       3       3       3 

summary of quality measures:
    support         
 Min.   :0.0007213  
 1st Qu.:0.0008769  
 Median :0.0010608  
 Mean   :0.0013658  
 3rd Qu.:0.0014568  
 Max.   :0.0086559  

includes transaction ID lists: FALSE 

mining info:
 data ntransactions      support confidence
   v2         70703 0.0007071836          1
```

```{r}


v2rules <- apriori(v2, parameter = list(support = 0.001, confidence = 0.6,
                                        minlen = 3, maxlen = 5))


v2rules
set of 167 rules 
> summary(v2rules)
set of 167 rules

rule length distribution (lhs + rhs):sizes
  3 
167 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      3       3       3       3       3       3 

summary of quality measures:
    support           confidence          lift        
 Min.   :0.001004   Min.   :0.6031   Min.   :  3.127  
 1st Qu.:0.001146   1st Qu.:0.8675   1st Qu.:  6.193  
 Median :0.001358   Median :0.9810   Median : 14.527  
 Mean   :0.001826   Mean   :0.9214   Mean   : 50.752  
 3rd Qu.:0.001789   3rd Qu.:1.0000   3rd Qu.: 30.860  
 Max.   :0.008656   Max.   :1.0000   Max.   :636.964  

mining info:
 data ntransactions support confidence
   v2         70703   0.001        0.6
```   

All the rules (167):

```{r}

    lhs                                                                 rhs                                      support     confidence lift      
1   {vendor=kriminale1,location=Italy}                               => {category=Drugs, Cannabis, Hash}         0.001004201 1.0000000   38.488296
2   {vendor=kriminale1,category=Drugs, Cannabis, Hash}               => {location=Italy}                         0.001004201 1.0000000  364.448454
3   {category=Drugs, Cannabis, Hash,location=Italy}                  => {vendor=kriminale1}                      0.001004201 0.6396396  636.963964
4   {vendor=Peaceful,category=Drugs, Psychedelics, LSD}              => {location=Australia}                     0.001018344 1.0000000   12.469665
5   {vendor=Peaceful,location=Australia}                             => {category=Drugs, Psychedelics, LSD}      0.001018344 0.9113924   29.873981
6   {vendor=laWnmoWermAn,category=Drugs, Cannabis, Weed}             => {location=USA}                           0.001117350 1.0000000    3.466683
7   {vendor=laWnmoWermAn,location=USA}                               => {category=Drugs, Cannabis, Weed}         0.001117350 0.9875000    6.115373
8   {vendor=Cloud9Products,category=Drugs, Cannabis, Weed}           => {location=USA}                           0.001103206 0.9512195    3.297577
9   {vendor=Cloud9Products,location=USA}                             => {category=Drugs, Cannabis, Weed}         0.001103206 1.0000000    6.192783
10  {vendor=Top_Gear_UK,category=Drugs, Psychedelics, LSD}           => {location=UK}                            0.001173925 1.0000000   10.055895
11  {vendor=Top_Gear_UK,location=UK}                                 => {category=Drugs, Psychedelics, LSD}      0.001173925 1.0000000   32.778396
12  {vendor=bitcoinfashion,location=Hong Kong}                       => {category=Counterfeits, Watches, NA}     0.001004201 0.8554217   62.609606
13  {vendor=bitcoinfashion,category=Counterfeits, Watches, NA}       => {location=Hong Kong}                     0.001004201 1.0000000  213.604230
14  {vendor=Socal_Connect,category=Drugs, Cannabis, Weed}            => {location=USA}                           0.001103206 1.0000000    3.466683
15  {vendor=Socal_Connect,location=USA}                              => {category=Drugs, Cannabis, Weed}         0.001103206 0.9285714    5.750441
16  {vendor=chipzahoy,category=Drugs, Cannabis, Concentrates}        => {location=Canada}                        0.001060775 0.8823529   20.561964
17  {vendor=chipzahoy,location=Canada}                               => {category=Drugs, Cannabis, Concentrates} 0.001060775 1.0000000   28.394779
18  {vendor=TheHappyGuy,category=Drugs, Cannabis, Weed}              => {location=USA}                           0.001145637 1.0000000    3.466683
19  {vendor=TheHappyGuy,location=USA}                                => {category=Drugs, Cannabis, Weed}         0.001145637 0.9529412    5.901358
20  {vendor=oliver201,location=Canada}                               => {category=Drugs, Cannabis, Weed}         0.001258787 0.9569892    5.926426
21  {vendor=oliver201,category=Drugs, Cannabis, Weed}                => {location=Canada}                        0.001258787 0.9888889   23.044631
22  {vendor=cannabis-king,location=Germany}                          => {category=Drugs, Cannabis, Weed}         0.001131494 0.8695652    5.385028
23  {vendor=cannabis-king,category=Drugs, Cannabis, Weed}            => {location=Germany}                       0.001131494 0.9411765   13.672488
24  {vendor=Meds2Buy,category=Drugs, Prescription, NA}               => {location=UK}                            0.001287074 0.9285714    9.337617
25  {vendor=Meds2Buy,location=UK}                                    => {category=Drugs, Prescription, NA}       0.001287074 1.0000000   19.307209
26  {vendor=zeltasgarden,category=Drugs, Cannabis, Weed}             => {location=USA}                           0.001258787 1.0000000    3.466683
27  {vendor=zeltasgarden,location=USA}                               => {category=Drugs, Cannabis, Weed}         0.001258787 0.9081633    5.624058
28  {vendor=ickysticky,category=Drugs, Cannabis, Weed}               => {location=USA}                           0.001188068 1.0000000    3.466683
29  {vendor=ickysticky,location=USA}                                 => {category=Drugs, Cannabis, Weed}         0.001188068 0.8076923    5.001863
30  {vendor=MrBudget,category=Drugs, Ecstasy, Pills}                 => {location=Netherlands}                   0.001301218 0.9892473   15.682232
31  {vendor=MrBudget,location=Netherlands}                           => {category=Drugs, Ecstasy, Pills}         0.001301218 0.9019608   16.633107
32  {vendor=MrCronk,location=Undeclared}                             => {category=Drugs, Cannabis, Weed}         0.001032488 0.7156863    4.432090
33  {category=Drugs, Cannabis, Weed,location=Undeclared}             => {vendor=MrCronk}                         0.001032488 1.0000000  548.085271
34  {vendor=MrCronk,category=Drugs, Cannabis, Weed}                  => {location=Undeclared}                    0.001032488 0.8295455  563.955310
35  {vendor=OzJuice,category=Drugs, Steroids, NA}                    => {location=Australia}                     0.001343649 1.0000000   12.469665
36  {vendor=OzJuice,location=Australia}                              => {category=Drugs, Steroids, NA}           0.001343649 0.8962264   31.478339
37  {vendor=CB2013,category=Drugs, Cannabis, Weed}                   => {location=USA}                           0.001301218 1.0000000    3.466683
38  {vendor=CB2013,location=USA}                                     => {category=Drugs, Cannabis, Weed}         0.001301218 0.8518519    5.275333
39  {vendor=Remedyplus,location=India}                               => {category=Drugs, Prescription, NA}       0.001272930 0.8653846   16.708162
40  {vendor=Remedyplus,category=Drugs, Prescription, NA}             => {location=India}                         0.001272930 0.9375000   92.446391
41  {vendor=only,category=Tobacco, Smoked, NA}                       => {location=UK}                            0.001414367 1.0000000   10.055895
42  {vendor=only,location=UK}                                        => {category=Tobacco, Smoked, NA}           0.001414367 1.0000000  267.814394
43  {category=Tobacco, Smoked, NA,location=UK}                       => {vendor=only}                            0.001414367 0.8000000  514.203636
44  {vendor=blow,location=Canada}                                    => {category=Drugs, Cannabis, Weed}         0.001230499 0.7767857    4.810465
45  {vendor=blow,category=Drugs, Cannabis, Weed}                     => {location=Canada}                        0.001230499 1.0000000   23.303560
46  {vendor=StealthBomber,category=Drugs, Ecstasy, Pills}            => {location=Germany}                       0.001103206 1.0000000   14.527019
47  {vendor=StealthBomber,location=Germany}                          => {category=Drugs, Ecstasy, Pills}         0.001103206 0.6842105   12.617563
48  {vendor=CanadianChronic,location=Canada}                         => {category=Drugs, Cannabis, Weed}         0.001541660 1.0000000    6.192783
49  {vendor=CanadianChronic,category=Drugs, Cannabis, Weed}          => {location=Canada}                        0.001541660 0.9396552   21.897310
50  {vendor=Counsellor,category=Drugs, Cannabis, Concentrates}       => {location=USA}                           0.001074919 1.0000000    3.466683
51  {vendor=Counsellor,location=USA}                                 => {category=Drugs, Cannabis, Concentrates} 0.001074919 0.6495726   18.444472
52  {vendor=klosterbier,category=Drugs, Cannabis, Seeds}             => {location=EU}                            0.001202212 0.9770115   22.987569
53  {vendor=klosterbier,location=EU}                                 => {category=Drugs, Cannabis, Seeds}        0.001202212 0.7589286  151.150780
54  {category=Drugs, Cannabis, Seeds,location=EU}                    => {vendor=klosterbier}                     0.001202212 0.7589286  454.733278
55  {vendor=William_Shatner,category=Drugs, Cannabis, Concentrates}  => {location=USA}                           0.001739672 0.9919355    3.438726
56  {vendor=William_Shatner,location=USA}                            => {category=Drugs, Cannabis, Concentrates} 0.001739672 1.0000000   28.394779
57  {vendor=RechardSport,category=Counterfeits, Clothing, NA}        => {location=China}                         0.001117350 1.0000000   25.027611
58  {vendor=RechardSport,location=China}                             => {category=Counterfeits, Clothing, NA}    0.001117350 0.6030534  187.831220
59  {vendor=StrattonOakmont,category=Drugs, Cannabis, Weed}          => {location=USA}                           0.001824534 1.0000000    3.466683
60  {vendor=StrattonOakmont,location=USA}                            => {category=Drugs, Cannabis, Weed}         0.001824534 0.9772727    6.052038
61  {vendor=optiman,category=Drug paraphernalia, Stashes, NA}        => {location=USA}                           0.001301218 0.9019608    3.126812
62  {category=Drug paraphernalia, Stashes, NA,location=USA}          => {vendor=optiman}                         0.001301218 0.9787234   79.813934
63  {vendor=cerberus,category=Drugs, Steroids, NA}                   => {location=UK}                            0.001018344 1.0000000   10.055895
64  {vendor=alchemycd,location=China}                                => {category=Drugs, Ecstasy, MDMA}          0.001117350 0.6076923   12.446602
65  {vendor=alchemycd,category=Drugs, Ecstasy, MDMA}                 => {location=China}                         0.001117350 1.0000000   25.027611
66  {vendor=bigbudsforyou,category=Drugs, Cannabis, Weed}            => {location=USA}                           0.001499229 0.9814815    3.402485
67  {vendor=bigbudsforyou,location=USA}                              => {category=Drugs, Cannabis, Weed}         0.001499229 0.7625899    4.722554
68  {vendor=wakeside917,category=Data, Pirated, NA}                  => {location=USA}                           0.001767959 0.9057971    3.140111
69  {vendor=wakeside917,location=USA}                                => {category=Data, Pirated, NA}             0.001767959 0.9689922  113.240759
70  {category=Data, Pirated, NA,location=USA}                        => {vendor=wakeside917}                     0.001767959 0.9842520  486.640328
71  {vendor=itewqq,category=Drugs, Cannabis, Concentrates}           => {location=USA}                           0.001004201 1.0000000    3.466683
72  {vendor=itewqq,category=Drugs, Cannabis, Weed}                   => {location=USA}                           0.001046632 1.0000000    3.466683
73  {vendor=budbrother,location=Australia}                           => {category=Drugs, Cannabis, Weed}         0.001626522 0.7615894    4.716358
74  {vendor=budbrother,category=Drugs, Cannabis, Weed}               => {location=Australia}                     0.001626522 0.9663866   12.050517
75  {vendor=Steroid-Depot,category=Drugs, Steroids, NA}              => {location=EU}                            0.001272930 0.6040268   14.211817
76  {vendor=Steroid-Depot,location=EU}                               => {category=Drugs, Steroids, NA}           0.001272930 1.0000000   35.123199
77  {vendor=optiman,category=Drug paraphernalia, Containers, NA}     => {location=USA}                           0.001810390 0.9343066    3.238945
78  {category=Drug paraphernalia, Containers, NA,location=USA}       => {vendor=optiman}                         0.001810390 0.9411765   76.752018
79  {vendor=XTandMD,category=Drugs, Ecstasy, Pills}                  => {location=Netherlands}                   0.001711384 0.9307692   14.755197
80  {vendor=XTandMD,location=Netherlands}                            => {category=Drugs, Ecstasy, Pills}         0.001711384 0.7423313   13.689371
81  {vendor=the_real_caliconnect,location=No Info}                   => {category=Drugs, Cannabis, Weed}         0.002135694 1.0000000    6.192783
82  {vendor=the_real_caliconnect,category=Drugs, Cannabis, Weed}     => {location=No Info}                       0.002135694 0.8628571    6.187281
83  {vendor=TheDigital,category=Services, Advertising, NA}           => {location=No Info}                       0.001456798 0.9809524    7.034105
84  {category=Services, Advertising, NA,location=No Info}            => {vendor=TheDigital}                      0.001456798 0.6560510   56.292197
85  {vendor=medsguru,location=India}                                 => {category=Drugs, Prescription, NA}       0.001683097 0.9296875   17.949671
86  {vendor=medsguru,category=Drugs, Prescription, NA}               => {location=India}                         0.001683097 0.8263889   81.489782
87  {vendor=theben,category=Drugs, Steroids, NA}                     => {location=EU}                            0.002475142 1.0000000   23.528453
88  {vendor=theben,location=EU}                                      => {category=Drugs, Steroids, NA}           0.002475142 0.9615385   33.772307
89  {vendor=ThreeKings,category=Drugs, Steroids, NA}                 => {location=EU}                            0.002022545 0.9533333   22.430458
90  {vendor=ThreeKings,location=EU}                                  => {category=Drugs, Steroids, NA}           0.002022545 0.7944444   27.903430
91  {vendor=Drugs4you,category=Drugs, Cannabis, Weed}                => {location=Germany}                       0.001357792 1.0000000   14.527019
92  {vendor=RepAAA,location=Hong Kong}                               => {category=Counterfeits, Watches, NA}     0.002673154 1.0000000   73.191511
93  {vendor=RepAAA,category=Counterfeits, Watches, NA}               => {location=Hong Kong}                     0.002673154 1.0000000  213.604230
94  {category=Counterfeits, Watches, NA,location=Hong Kong}          => {vendor=RepAAA}                          0.002673154 0.7269231  271.934615
95  {vendor=InsideTheWhale,category=Drugs, Steroids, NA}             => {location=UK}                            0.001414367 0.9803922    9.858721
96  {vendor=WeedConnect,category=Drugs, Cannabis, Weed}              => {location=USA}                           0.002673154 1.0000000    3.466683
97  {vendor=WeedConnect,location=USA}                                => {category=Drugs, Cannabis, Weed}         0.002673154 0.9742268    6.033175
98  {vendor=HollandDutch,category=Drugs, Ecstasy, Pills}             => {location=Netherlands}                   0.002361993 1.0000000   15.852691
99  {vendor=HollandDutch,location=Netherlands}                       => {category=Drugs, Ecstasy, Pills}         0.002361993 0.8564103   15.793108
100 {vendor=Montfort,category=Drugs, RCs, NA}                        => {location=No Info}                       0.001018344 1.0000000    7.170690
101 {vendor=BudMasterGeneral,category=Drugs, Cannabis, Concentrates} => {location=USA}                           0.001145637 0.9878049    3.424406
102 {vendor=BudMasterGeneral,category=Drugs, Cannabis, Weed}         => {location=USA}                           0.001598235 0.9912281    3.436274
103 {vendor=OnePiece,category=Forgeries, Other, NA}                  => {location=No Info}                       0.001329505 1.0000000    7.170690
104 {category=Forgeries, Other, NA,location=No Info}                 => {vendor=OnePiece}                        0.001329505 0.7175573  115.303296
105 {vendor=Optumis,category=Info/eBooks, Psychology, NA}            => {location=No Info}                       0.001329505 1.0000000    7.170690
106 {category=Info/eBooks, Psychology, NA,location=No Info}          => {vendor=Optumis}                         0.001329505 0.8468468   50.957117
107 {vendor=captainkirk,category=Info/eBooks, Psychology, NA}        => {location=Internet}                      0.001230499 1.0000000   30.240804
108 {category=Info/eBooks, Psychology, NA,location=Internet}         => {vendor=captainkirk}                     0.001230499 0.9354839   47.963391
109 {vendor=HederalExpress,category=Drugs, Stimulants, Cocaine}      => {location=UK}                            0.001541660 1.0000000   10.055895
110 {vendor=SAGreat,location=Australia}                              => {category=Drugs, Cannabis, Weed}         0.002956027 0.9247788    5.726954
111 {vendor=SAGreat,category=Drugs, Cannabis, Weed}                  => {location=Australia}                     0.002956027 0.9952381   12.410286
112 {vendor=blackhand,category=Information, eBooks, NA}              => {location=Internet}                      0.001499229 1.0000000   30.240804
113 {vendor=blackhand,category=Info/eBooks, Other, NA}               => {location=Internet}                      0.001499229 1.0000000   30.240804
114 {vendor=MerckKGaA,category=Drugs, Ecstasy, Pills}                => {location=Germany}                       0.001994258 1.0000000   14.527019
115 {vendor=Optumis,category=Info/eBooks, Science, NA}               => {location=No Info}                       0.001386080 1.0000000    7.170690
116 {category=Info/eBooks, Science, NA,location=No Info}             => {vendor=Optumis}                         0.001386080 0.8032787   48.335501
117 {vendor=captainkirk,category=Info/eBooks, Science, NA}           => {location=Internet}                      0.001626522 1.0000000   30.240804
118 {category=Info/eBooks, Science, NA,location=Internet}            => {vendor=captainkirk}                     0.001626522 0.9663866   49.547809
119 {vendor=DrEarnhardt,category=Drugs, Cannabis, Weed}              => {location=Netherlands}                   0.001980114 1.0000000   15.852691
120 {vendor=passman,category=Info/eBooks, Making money, NA}          => {location=No Info}                       0.001103206 1.0000000    7.170690
121 {vendor=passman,category=Information, Guides, NA}                => {location=No Info}                       0.001456798 1.0000000    7.170690
122 {vendor=Bigdeal100,category=Jewelry, NA, NA}                     => {location=No Info}                       0.001640666 0.6203209    4.448128
123 {vendor=Bigdeal100,location=No Info}                             => {category=Jewelry, NA, NA}               0.001640666 0.7631579  183.529091
124 {vendor=XXiB,category=Data, Pirated, NA}                         => {location=Torland}                       0.001555804 0.7534247   26.449545
125 {vendor=XXiB,location=Torland}                                   => {category=Data, Pirated, NA}             0.001555804 0.8461538   98.885315
126 {category=Data, Pirated, NA,location=Torland}                    => {vendor=XXiB}                            0.001555804 0.7638889  190.845357
127 {vendor=etimbuk,category=Info/eBooks, Anonymity, NA}             => {location=No Info}                       0.001131494 0.9876543    7.082163
128 {vendor=Optumis,category=Info/eBooks, Anonymity, NA}             => {location=No Info}                       0.001216356 1.0000000    7.170690
129 {vendor=thomascheer,category=Drugs, RCs, NA}                     => {location=China}                         0.001004201 0.6635514   16.607106
130 {vendor=medibuds,category=Drugs, Cannabis, Concentrates}         => {location=USA}                           0.001739672 1.0000000    3.466683
131 {vendor=medibuds,category=Drugs, Cannabis, Weed}                 => {location=USA}                           0.002560005 1.0000000    3.466683
132 {vendor=TheDigital,category=Services, Other, NA}                 => {location=No Info}                       0.001159781 0.9647059    6.917606
133 {vendor=Optumis,category=Info/eBooks, Drugs, NA}                 => {location=No Info}                       0.001584091 1.0000000    7.170690
134 {vendor=Gotmilk,category=Drugs, Prescription, NA}                => {location=No Info}                       0.002630723 0.7072243    5.071286
135 {vendor=Gotmilk,location=No Info}                                => {category=Drugs, Prescription, NA}       0.002630723 0.7351779   14.194233
136 {vendor=OnePiece,category=Forgeries, Physical documents, NA}     => {location=No Info}                       0.001315361 1.0000000    7.170690
137 {vendor=OnePiece,category=Info/eBooks, Other, NA}                => {location=No Info}                       0.002036689 1.0000000    7.170690
138 {vendor=rc4me,category=Drugs, Cannabis, Synthetics}              => {location=China}                         0.002503430 1.0000000   25.027611
139 {category=Drugs, Cannabis, Synthetics,location=China}            => {vendor=rc4me}                           0.002503430 0.6389892   69.505310
140 {vendor=TheDigital,category=Services, Money, NA}                 => {location=No Info}                       0.001230499 0.8877551    6.365816
141 {vendor=etimbuk,category=Info/eBooks, Making money, NA}          => {location=No Info}                       0.001442654 1.0000000    7.170690
142 {vendor=etimbuk,category=Other, NA, NA}                          => {location=No Info}                       0.001428511 1.0000000    7.170690
143 {vendor=etimbuk,category=Info/eBooks, Other, NA}                 => {location=No Info}                       0.001329505 0.9894737    7.095209
144 {vendor=captainkirk,category=Data, Pirated, NA}                  => {location=Internet}                      0.001315361 1.0000000   30.240804
145 {category=Data, Pirated, NA,location=Internet}                   => {vendor=captainkirk}                     0.001315361 0.7099237   36.398646
146 {vendor=RXChemist,category=Drugs, Prescription, NA}              => {location=No Info}                       0.008104324 0.9614094    6.893968
147 {vendor=RXChemist,location=No Info}                              => {category=Drugs, Prescription, NA}       0.008104324 0.9711864   18.750900
148 {category=Drugs, Prescription, NA,location=No Info}              => {vendor=RXChemist}                       0.008104324 0.6481900   74.277116
149 {vendor=rc4me,category=Drugs, RCs, NA}                           => {location=China}                         0.002376137 1.0000000   25.027611
150 {vendor=sexyhomer,category=Counterfeits, Watches, NA}            => {location=China}                         0.008655927 0.9807692   24.546310
151 {vendor=sexyhomer,location=China}                                => {category=Counterfeits, Watches, NA}     0.008655927 0.9622642   70.429568
152 {category=Counterfeits, Watches, NA,location=China}              => {vendor=sexyhomer}                       0.008655927 1.0000000  103.216058
153 {vendor=Optumis,category=Info/eBooks, Making money, NA}          => {location=No Info}                       0.001626522 1.0000000    7.170690
154 {vendor=captainkirk,category=Info/eBooks, Making money, NA}      => {location=Internet}                      0.001032488 1.0000000   30.240804
155 {category=Info/eBooks, Making money, NA,location=Internet}       => {vendor=captainkirk}                     0.001032488 0.8795181   45.093957
156 {vendor=fake,category=Information, eBooks, NA}                   => {location=No Info}                       0.001527517 0.9818182    7.040313
157 {vendor=captainkirk,category=Information, eBooks, NA}            => {location=Internet}                      0.002489286 1.0000000   30.240804
158 {vendor=TheDigital,category=Data, Accounts, NA}                  => {location=No Info}                       0.002149838 0.8444444    6.055249
159 {vendor=TheDigital,category=Other, NA, NA}                       => {location=No Info}                       0.001089063 0.9871795    7.078758
160 {vendor=fake,category=Information, Guides, NA}                   => {location=No Info}                       0.004327963 0.9532710    6.835611
161 {vendor=captainkirk,category=Information, Guides, NA}            => {location=Internet}                      0.002460999 1.0000000   30.240804
162 {category=Information, Guides, NA,location=Internet}             => {vendor=captainkirk}                     0.002460999 0.7909091   40.550867
163 {vendor=fake,category=Info/eBooks, Other, NA}                    => {location=No Info}                       0.006039348 0.9595506    6.880639
164 {vendor=captainkirk,category=Other, NA, NA}                      => {location=Internet}                      0.001004201 1.0000000   30.240804
165 {category=Other, NA, NA,location=Internet}                       => {vendor=captainkirk}                     0.001004201 0.8554217   43.858506
166 {vendor=Optumis,category=Info/eBooks, Other, NA}                 => {location=No Info}                       0.003154039 1.0000000    7.170690
167 {vendor=captainkirk,category=Info/eBooks, Other, NA}             => {location=Internet}                      0.003550062 1.0000000   30.240804
```
![03-matrix167-01]()

Itemsets in Consequent (RHS)
 [1] "{category=Drugs, Cannabis, Hash}"         "{location=Italy}"                        
 [3] "{vendor=kriminale1}"                      "{location=Australia}"                    
 [5] "{category=Drugs, Psychedelics, LSD}"      "{location=USA}"                          
 [7] "{category=Drugs, Cannabis, Weed}"         "{location=UK}"                           
 [9] "{category=Counterfeits, Watches, NA}"     "{location=Hong Kong}"                    
[11] "{location=Canada}"                        "{category=Drugs, Cannabis, Concentrates}"
[13] "{location=Germany}"                       "{category=Drugs, Prescription, NA}"      
[15] "{location=Netherlands}"                   "{category=Drugs, Ecstasy, Pills}"        
[17] "{vendor=MrCronk}"                         "{location=Undeclared}"                   
[19] "{category=Drugs, Steroids, NA}"           "{location=India}"                        
[21] "{category=Tobacco, Smoked, NA}"           "{vendor=only}"                           
[23] "{location=EU}"                            "{category=Drugs, Cannabis, Seeds}"       
[25] "{vendor=klosterbier}"                     "{location=China}"                        
[27] "{category=Counterfeits, Clothing, NA}"    "{vendor=optiman}"                        
[29] "{category=Drugs, Ecstasy, MDMA}"          "{category=Data, Pirated, NA}"            
[31] "{vendor=wakeside917}"                     "{location=No Info}"                      
[33] "{vendor=TheDigital}"                      "{vendor=RepAAA}"                         
[35] "{vendor=OnePiece}"                        "{vendor=Optumis}"                        
[37] "{location=Internet}"                      "{vendor=captainkirk}"                    
[39] "{category=Jewelry, NA, NA}"               "{location=Torland}"                      
[41] "{vendor=XXiB}"                            "{vendor=rc4me}"                          
[43] "{vendor=RXChemist}"                       "{vendor=sexyhomer}"


Lift gets higher from 1 to 44 here. 
Same Key for 03-matrix167-02

