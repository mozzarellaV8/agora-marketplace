# highlight features of interest - wide range

``` {r}
levels(fb$cat)
# "Counterfeits" - identities
# "Drug paraphernalia"
# "Drugs"
# "Forgeries" - identities
# "Information" - how to / guides
# "Electronics" - check for scales
# "Weapons"

levels(fb$subcat)
# "Accessories"
# "Advertising" 
# "Ammunition"
# "Cannabis"
# "Electronics"
# "Grinders"
# "Guides"
# "Other
# "Paper"
# "Lethal firearms"
# "Melee"
# "Money"
# "Non-lethal firearms"
# "Other
# "Paper"
# "Paraphernalia"
# "Physical documents"
# "Pipes"
# "Prescription"
# "Scales"
# "Scans/Photos"
# "Smoked"
# "Stashes"
# "eBooks"

levels(fb$subsubcat)
# "Concentrates"
# "Edibles"
# "Hash"

# "Other"
# "Others"

# "Prescription"
## "Seeds"
## "Shake/trim"
## "Synthetics"
## "Weed"
```
```{r}
> summary(p14$usd)
      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
         0         29         95      15890        332 2215000000 
```
``` {r}
> summary(p14$cat)

      Counterfeits               Data Drug paraphernalia              Drugs 
             44151              35675              10112             709176 
       Electronics          Forgeries        Information            Jewelry 
              9631              18013             104337               7887 
          Listings              Other           Services            Tobacco 
              1532              18080              37077              13503 
           Weapons 
              8935 
```
``` {r}
> summary(p14$subcat)
               <NA>         Accessories            Accounts         Advertising 
              39236                4370               14996                 632 
         Ammunition        Barbiturates              Benzos            Cannabis 
               2533                 504               48054              177447 
           Clothing          Containers     Disassociatives       Dissociatives 
              10403                1821                 338               12407 
            Ecstasy        Ecstasy-MDMA       Ecstasy-NoSub         Electronics 
             109065                   7                   1                1495 
          Fireworks            Grinders              Guides             Hacking 
                134                1089               60164                6113 
Injecting equipment     Lethal firearms               Melee           Methylone 
                606                2727                2306                   4 
              Money Non-lethal firearms             Opioids               Other 
              24187                1235               46948               21663 
              Paper       Paraphernalia  Physical documents               Pipes 
                484                1964                7267                3426 
            Pirated        Prescription        Psychedelics                 RCs 
              13959               70208               72734               31772 
             Scales        Scans/Photos              Smoked            Software 
                727                7030               11539                6720 
            Stashes            Steroids          Stimulants             Watches 
               1959               35134               89377               24453 
        Weight loss              eBooks 
               4698               44173

> summary(p14$subsubcat)
          <NA>             2C          5-MeO   Barbiturates  Buprenorphine        Cocaine 
        518279          10295           3337             14           2005          41075 
       Codeine   Concentrates            DMT Dihydrocodeine        Ecstasy        Edibles 
           965          18146           6120            321             37          10050 
      Fentanyl            GBL            GHB           Hash         Heroin    Hydrocodone 
          4335            726           2407          24051          10093           1541 
 Hydromorphone       Ketamine            LSD            MDA           MDMA            MXE 
          1126           4485          23202           4120          43705           3460 
     Mescaline           Meth       Morphine      Mushrooms             NB          Opium 
          2160          16901           1365           6190          12377            374 
         Other         Others      Oxycodone            PCP          Pills   Prescription 
         31027           1130           5811             74          48356          11924 
        Salvia          Seeds     Shake/trim          Speed         Spores     Synthetics 
           553           6043            597          19472           1307          12875 
          Weed 
        105678
```