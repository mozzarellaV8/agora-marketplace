# Agora Marketplace Analysis

Agora Marketplace index page on July 7th, 2015:

![2015-07-07-index](vis/index-2015-07-07.jpg)

Agora Marketplace index page on January 1st, 2014 (printout with notes for extracting data from html)

![](vis/index-2014-01-01.jpg)

_student work in R_ for [Foundations of Data Science](https://www.springboard.com/workshops/data-science)

- [the Problem](#the-problem)
- [the Data](#the-data)
- [extraction notes](extraction/readme.md)
- [extraction scripts](R/extraction)

## Agora and anonymous marketplaces

Agora was chosen as a market to analyze because of it's immense popularity and high usage. Additionally (and subjectively), the conditions of Agora's shutdown were unique to me in that the admins voluntarily shut it down after a [paper was published in August 2015](https://www.usenix.org/system/files/conference/usenixsecurity15/sec15-paper-kwon.pdf) that exposed vulnerabilities that could de-anonymize Tor users. 

This is in contrast to other markets of similar scale. The largest examples are Silk Road's demise at the hands of law enforcement; darknet markets [Evolution](https://www.deepdotweb.com/2015/03/18/evolution-marketplace-exit-scam-biggest-exist-scam-ever/) and [Sheep](https://www.deepdotweb.com/2013/11/30/sheep-marketplace-scammed-over-40000000-in-the-biggets-darknet-scam-ever/) turning out to be massive exit-scams.

It's a stretch to say (and impossible to prove) that Agora's administrators were completely altruisitic in their voluntary shutdown; but such protections of themselves and their clients might suggest that conducting business professionally was a priority above others. 

## the Problem

_broadly speaking:_ Darknet markets are _in the dark_, anonymous marketplaces are _anonymous_. To shed light, basic questions:

- What's there? What could be there?
- Where is it? Where from? Where will it be from?
- How much is there? _How much could be there?_
- How much does it cost? _How much could it cost?_


## the Data

Agora was a referral-based darknet market that rose to prominence after the demise of Silk Road 2 in 2013. 

The data was acquired via gwern's [black market archives](http://www.gwern.net/Black-market%20archives#grams); specifically from the torrent/magnet link. Comprising this archive are weekly crawls of multiple anonymous marketplaces on the darknet - well-trafficked and documented sites such as Silk Road and Evolution in addition to smaller, more ephemeral markets.

For Agora specifically, the crawl dates begin on 2014-01-01 and end on 2015-07-07. There are 206 daily crawls total, generally occurring weekly and occassionally more frequently.

The scale of gwern's harvest prevents it from being posted here. Here is a glimpse of the extraction from html, subsetted for listings that contained client feedback - potential indicator of a transaction.

![](vis/extractedSample02.png)

_*to download the data, please refer to gwern's black market archives link above. For me it took about about 1-2 hours to download; Agora is roughly 127 GB total. But each daily crawl contains many subfolders which adds to the tar.gz extraction time - which I just left unarchiving overnight._

## Exploratory Regression

Before diving into extraction of the data, I took a look at counts of the crawls themselves to get a sense of the scale of the market. Each page in the `p` directory corresponds to a single product listing; each in the `vendor` directory corresponds to a vendor's 'storefront' page. 

While not definitive in it's conclusion, the purpose of this exploration was to explore the question
	
		"What if Agora never shut down? What would the trend be?"

We know Agora began operations sometime in 2013, and picked up steam in 2014 after the downfall of Silk Road 2 - many buyers and vendors migrated over to Agora to continue business. 

total days | product listings | vendor pages  | start date |  end date
-----------| :--------------: | :------------:| :--------: | :--------:
533		   |	2,467,200     |	   19,245     | 2014-01-01 | 2015-07-07

Til I finish up with Poisson - just gonna violate some critical assumptions of linear regression:

``` r
pv <- read.csv("data/counts/crawl-distribution.csv")
str(pv)

pv$date <- as.Date(pv$date)
pv$vendor <- as.integer(pv$vendor)
summary(pv)

#         date                  p             vendor     
# Min.   :2014-01-01   Min.   :    1   Min.   :  1.0  
# 1st Qu.:2014-09-11   1st Qu.: 4453   1st Qu.: 49.5  
# Median :2014-12-07   Median :12697   Median : 96.0  
# Mean   :2014-11-26   Mean   :12154   Mean   : 94.8  
# 3rd Qu.:2015-03-17   3rd Qu.:19030   3rd Qu.:140.5  
# Max.   :2015-07-07   Max.   :27654   Max.   :184.0 

sum(pv$p)
# [1] 2467200

sum(pv$vendor)
# [1] 19245

```

![](plots/RDraft/pgDist-lm-product-01.png)

``` r
p.lm <- (product ~ date, data = pv)

#                 Estimate  Std.Error  t value 		Pr(>|t|)    
#   (Intercept) -4.542e+05  5.579e+04  	-8.142 	4.04e-14 ***
#   date         2.843e+01  3.401e+00    8.360 	1.03e-14 ***
# 	Multiple R-squared:  0.258,	Adjusted R-squared:  0.2543 
```

Given the limited number of variables, these models probably shouldn't be considered for drawing definitive conclusions. In addition to that, there are many outside factors with the crawls and markets that can influence product and vendor listing counts. 

It can't be assumed that every crawl represents a complete day's listings; the market itself would be down at seemingly random times to address server or security issues. Many pages (> 10,000) were removed during the extraction process because they were blank - too many requests had been made to the server. Gwern himself has made clear that it's best to consider his crawls a lower-bound for market activity. 

But maybe the best reason these plots are spurious is that ordinary least squares regression isn't exactly suited for count data or time series modeling. Violating a few assumptions here. 

![](plots/RDraft/pgDist-lm-vendor-01.png)

``` r
# Coefficients:
#                 Estimate   Std.Error  t value Pr(>|t|)    
#   (Intercept) -1.505e+03   4.271e+02  -3.523 	0.000528 ***
#   date         9.752e-02   2.604e-02   3.745 	0.000235 ***
# 	Multiple R-squared:  0.06523,	Adjusted R-squared:  0.06058
```
	
**_be back after the Poisson chapter ~_**

