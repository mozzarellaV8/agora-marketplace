# Agora Association Rules - Preparation

- _outputs from the file `agora-associations-03.R`_
- basic info on [Association Rule Mining](AssociationBasics.md)

Contents:

- [Preparation](#preparation)
- [Discretize Prices](#discretize-prices)
- [Anonymize Vendors](#anonymize-vendor-names)
- [Transaction Conversion](#transaction-conversion)
- [References](#references)

```{R}

library(arules)
library(arulesViz)
library(data.table)
library(igraph)
library(geomnet)
library(ggplot2)
library(anonymizer)

a <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
```

Even though association rule mining is often written about in terms of finding novel itemsets and rules, I'm going to be focusing more on seeing what traverses this market network, what relationships might exist, and looking at probabilities for different classes occurring as a means to advise in selecting features. 

While the market is large, you might say the products on offer fall into 'niche' categories. I think of looking for novel itemsets/rules here as being akin to doing so at a supermarket but limiting yourself to only the produce section. Basically, I'm not hoping to find out something akin to sales of Pop-Tarts spiking before hurricanes<sup>[1](#references)</sup> - and nevermind about diapers and beer. There's less of a chance for "surprises" when the range of items doesn't span Amazon's entire catalog. 

That said - finding rules that traverse the network should still prove informative and interesting.

# Preparation

## Subset and Categorize

```{r}
ag <- subset(a, a$usd <= 20000) # 2319949
```

Why subset for prices under $20,000? 

Often on Agora there will be products listed at exorbitant prices.

While on the surface they may resemble scams, it's been observed that these prices are here for vendors to keep their listings active while waiting for their supply to be restocked<sup>[2](#references)</sup><sup>,</sup>[3](#references)</sup>. The prices are set high to discourage transactions, but keep their listings active to maintain their market presence and 'advertise' for the near-future when supply is replenished. While there is some gray area where 'placeholders' will mingle amongst potentially legitimate listings, the number of these listings is quite small compared to the population and can be easily subsetted and examined were it an issue. 

An example of this 'mingling': sorting by price might show a $45,000 gram of cannabis, next to a $47,000 listing for a kilogram of cocaine. 

In some ways, this can be seen as a 'waitlist' for certain products. As opposed to buying a rare or one-of-a-kind item, to maintain a placeholder listing suggests at the very least a perceived demand for the product listed and renewable supply...more on this in another document.


## Combine Subcategories


```{r}
# convert NA to blank
ag$subcat <- as.character(ag$subcat)
ag$subsubcat <- as.character(ag$subsubcat)
ag$subcat[is.na(ag$subcat)] <- ""
ag$subsubcat[is.na(ag$subsubcat)] <- ""

# combine subcategory and sub-subcategory
ag$sc <- paste(ag$subcat, ag$subsubcat, sep = ", ")
levels(as.factor(ag$sc))
ag$sc <- gsub("\\b,\\s$", "", ag$sc)

# convert to factor
levels(as.factor(ag$sc))
ag$sc <- factor(ag$sc) # 106 levels
```

From the HTML, generally 3 categories could be extracted from each listing. These would range from high-level description (e.g. '**Drugs**') to finer-grain sub- and sub-subcategories (e.g. '**Cannabis**', '**Concentrates**') further down the menu. Each is it's own variable in the data, but they're inextricably linked by the listing itself.

For the purposes of association rule mining, I decided to aggregate sub- and sub-subcategories into one variable so as to avoid numerous superflous itemsets and rules. It also provides a nicer description when plotted and still comes out to right number of levels as a factor. A look at the top:

```{R}

levels(ag$sc)
  [1] "Accessories"                "Accounts"                  
  [3] "Advertising"                "Aliens/UFOs"               
  [5] "Ammunition"                 "Anonymity"                 
  [7] "Barbiturates"               "Barbiturates, Barbiturates"
  [9] "Benzos"                     "Cannabis"                  
 [11] "Cannabis, Concentrates"     "Cannabis, Edibles"         
 [13] "Cannabis, Hash"             "Cannabis, Seeds"           
 [15] "Cannabis, Shake/trim"       "Cannabis, Synthetics"      
 [17] "Cannabis, Weed"             "Clothing"                  
 [19] "Containers"                 "Disassociatives"           
 [21] "Disassociatives, GBL"       "Disassociatives, GHB"      
 [23] "Disassociatives, Ketamine"  "Disassociatives, MXE"      
 [25] "Disassociatives, Other"     "Dissociatives, GBL"        
 [27] "Dissociatives, GHB"         "Dissociatives, Ketamine"   
 [29] "Dissociatives, MXE"         "Dissociatives, Other"      
 [31] "Dissociatives, PCP"         "Doomsday"                  
 [33] "eBooks"                     "Economy"                   
 [35] "Ecstasy"                    "Ecstasy, Ecstasy"          
 [37] "Ecstasy, MDA"               "Ecstasy, MDMA"
 ...

```

# Discretize Prices

Initially I'd been using `discretize` from the `arules` library to do this. I decided to discretize manually for the last round of rule mining, given new domain info (and maybe bc I kept running into a bug at this point in the RMarkdown file, refusing to knit `discretize` for some reason).

Using `discretize` previously involved a choice of whether to bin values by equal intervals or cluster. To inform that decision - examined and plotted distributions of prices. 

```{R}
# discretize prices - but into cluster or interval?
ag$usd <- round(ag$usd, 2)

summary(ag$usd)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00    24.28    84.97    426.40   290.20 20000.00

quantile(ag$usd)
#   0%      25%      50%      75%     100% 
# 0.00    24.28    84.97   290.19 20000.00 
```

I'll venture that most of the values are towards the left...

```{r}
par(mfrow = c(2, 2), mar = c(6, 6, 6, 6), family = "GillSans")

hist(ag$usd, breaks = 100, main = "n < $20,000", 
     xlab = "", ylab = "Frequency")
hist(ag$usd, breaks = 100, xlim = c(0, 5000), 
     main = "n < $5,000", xlab = "", ylab = "")
hist(ag$usd, breaks = 1000, xlim = c(0, 1000), 
     main = "n < $1,000", xlab = "price in USD", ylab = "Frequency")
hist(ag$usd, breaks = 10000, xlim = c(0, 200),
     main = "n < $200", xlab = "price in USD", ylab = "")
```
![usd dist x4](plots/arules/usd-dist-01.jpg)

As suspected. One more curiousity - although the summary above shows a mean price of $426.40, the feeling is that outliers on high end are pulling that value up - a mean above the 3rd quartile seems to indicate something...

```{R}
# heavy on the left/long tail - quick check of the log()
ag$log.usd <- log(ag$usd)

par(mfrow = c(1, 1), mar = c(6, 6, 6, 4), las = 1, family = "GillSans")
hist(ag$log.usd, main = "log(usd) Distribution of Prices, n = 2316650",
     breaks = 100, xlab = "", ylab = "")
axis(1, at = seq(-5, 10, 1))

summary(ag$log.usd)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -Inf   3.190   4.442    -Inf   5.671   9.903 

exp(c(4, 4.25, 4.5, 4.75, 5))
# 54.59815  70.10541  90.01713 115.58428 148.41316
```
![log dist usd](plots/arules/usd-dist-02-log.jpeg)

Visually it appears the 'mean' of the log distribution of prices falls around 4.5 - of course, visually, that might change depending on the number of breaks/binwidth. But assuming that's case, prices can be observed in a range from about $60-$100 near the mean. This is judging from exponentiating 4.25 and 4.75 out. The spike at at/near zero seems to indicate a number of $1 listings. From exploratory plots, this spike is likely the result of eBook listings.

Eventually I decided to bin the prices myself (after using `cluster` in `discretize` on a previous mining session). The bins were mostly following the results of discretizing by cluster, but accounting for the inflated price frequency near zero. 

```{R}
# manually discretize
ag$p <- ag$usd
ag$p <- ifelse(ag$p <= 10.00, "$0-10", 
               ifelse(ag$p > 10 & ag$p <= 150.00, "$10-150",
                      ifelse(ag$p > 150 & ag$p <= 600.00, "$150-600",
                             ifelse(ag$p > 600 & ag$p <= 2000.00, "$600-2000",
                                    ifelse(ag$p > 2000 & ag$p <= 10000, "$2000-10000",
                                           ifelse(ag$p > 10000, "$10000-20000", NA))))))


ag$p <- factor(ag$p)  # 6 levels
```
![usd-disc-dist](plots/arules/usd-disc-dist.png)

```{r}
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

Above is a histogram of the manually binned prices, fill opacity set to relative frequency (x2) by some quick calculations:

``` {r}
summary(ag$p)
#  $0-10      $10-150 $10000-20000     $150-600  $2000-10000    $600-2000 
# 371235      1086166         7393       515111       106747       230701 

371235/nrow(ag)   # 0.1601979
1086166/nrow(ag)  # 0.4687098
7393/nrow(ag)     # 0.003190278
515111/nrow(ag)   # 0.2222842
106747/nrow(ag)   # 0.04606419
230701/nrow(ag)   # 0.09955367
```

# Anonymize Vendor Names

I'm no expert or even novice at cryptography; but decided it was worth the extra measure of anonymizing vendor names before using them as variables in mining. Even though vendor names provided were all nicknames, as seen in the case of one vendor<sup>[4]((#references)</sup> even with just a handle a real identity could be found out. 

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
From reading the manual pages, `anonymize` salts then hashes a vector with a few choices for algorithms. `SHA256` felt the appropriate hashing algorithm, and in what might be an insecure method I abbreviated the output to 6 characters afterwards. 

In practical terms though, anyone caring to download the dataset could easily find the vendor names. Anonymization, in this case, is done less for security and mostly out of respect for privacy. 


# References

_in progress_

<sup>1</sup> Borne, Dr. Kirk. "Association Rule Mining – Not Your Typical Data Science Algorithm | MapR." Association Rule Mining – Not Your Typical Data Science Algorithm | MapR. N.p., 2014. Web. 25 [Sept. 2016](https://www.mapr.com/blog/association-rule-mining-not-your-typical-data-science-algorithm).

<sup>2</sup> "Shedding Light on the Dark Web." The Economist. The Economist Newspaper, 2016. Web. 23 [Sept. 2016.](http://www.economist.com/news/international/21702176-drug-trade-moving-street-online-cryptomarkets-forced-compete)

<sup>3</sup> Demant, Munksgaard, & Houborg 2016, “Personal use, social supply or redistribution? cryptomarket demand on Silk Road 2 and Agora” [August 2016](http://www.gwern.net/docs/sr/2016-demant.pdf)

<sup>4</sup> Economist, op. cit.



