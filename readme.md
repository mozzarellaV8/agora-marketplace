index.html, July 7th, 2015:

![2015-07-07-index](vis/index-2015-07-07.jpg)

# Agora Marketplace Analysis

_student work in R_ for [Foundations of Data Science](https://www.springboard.com/workshops/data-science)

- [the Problem](#the-problem)
- [the Data](#the-data)
- [extraction notes](extraction/readme.md)
- [extraction scripts](R/extraction)
- [exploratory regression](#exploratory-regression) - *revise with Poisson models
- [Works Cited](#works-cited)


## the Problem

_broadly speaking:_ Darknet markets are _in the dark_; anonymous marketplaces are _anonymous_. To shed light, basic questions:

- What's there? What could be there?
- Where is it? Where from? Where will it be from?
- How much is there? **_How much could be there?_**
- How much does it cost? **_How much could it cost?_**


## the Data

Agora was a referral-based darknet market, that rose to prominence after the demise of Silk Road 2 in 2014.

The data was acquired via the Gwern's [Black Market Archives](http://www.gwern.net/Black-market%20archives#grams); specifically from the torrent/magnet link. Comprising this archive are weekly crawls of multiple anonymous marketplaces on the darknet - well-trafficked and documented sites such as _Silk Road_ and _Evolution_ in addition to smaller, more ephemeral markets.

For Agora specifically, the crawl dates begin on 2014-01-01 and end on 2015-07-07. There are 206 daily crawls total, generally occurring weekly, but sometimes more frequently.

The scale of Gwern's harvest prevents it from being posted here. Here is a glimpse of the extraction from html. 

![](vis/extractedSample02.png)

index.html, January 1st, 2014 (with notes for data extraction):

![](vis/index-2014-01-01.jpg)

_*to download the data, please refer to the Black Market Archives link above. For me it took about about 1-2 hours to download - Agora alone is roughly 127 GB total. But each daily crawl contains many subfolders which adds to the extraction time. I ended up leaving the tarball to unarchive overnight; it was almost done when I woke up._

## Exploratory Regression

total days | product listings | vendor pages  | start date |  end date
-----------| :--------------: | :------------:| :--------: | :--------:
533		   |	2,467,200     |	   19,245     | 2014-01-01 | 2015-07-07

Before diving into extraction of the data, I took a look at counts of the crawls themselves to get a sense of the scale of the market. Each page in the `p` directory corresponds to a single product listing; each in the `vendor` directory corresponds to a vendor's 'storefront' page. 

While not definitive in it's conclusion, the purpose of this exploration was to explore the question

- "What if Agora never shut down? What would the trend be?"

We know Agora began operations sometime in 2013, and picked up steam in 2014 after the downfall of Silk Road 2 - many buyers and vendors migrated over to Agora to continue business. 

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
# 2467200

sum(pv$vendor)
# 19245

```

![](plots/RDraft/pgDist-lm-product-01.png)

``` r
p.lm <- (product ~ date, data = pv)

#                 Estimate  Std.Error  t value 		Pr(>|t|)    
#   (Intercept) -4.542e+05  5.579e+04  	-8.142 	4.04e-14 ***
#   date         2.843e+01  3.401e+00    8.360 	1.03e-14 ***
# 	Multiple R-squared:  0.258,	Adjusted R-squared:  0.2543 
```

For more than a few reasons, these models probably shouldn't be considered for drawing definitive conclusions. Many outside factors with the crawls and markets influence product and vendor listing counts - on top of whether or not I've extracted and compiled the data in a legitimate way (the code's all here of course, and I'm more than happy to receive suggestions for improvements).

It can't be assumed that every crawl represents a complete day's listings; the market itself would be down at seemingly random times to address server or security issues. Many pages (> 10,000) were removed during the extraction process because they were blank - too many requests had been made to the server, or pages would be incomplete. 

Beyond that, Gwern himself has made clear that it's best to consider his crawls a [lower-bound for market activity](http://www.gwern.net/Black-market%20archives#interpreting-analyzing). A count might be a simple-enough measure, but it's probably best to also keep in mind the amount of scam-listings and general deception that can occur on anonymous marketplaces. 

But maybe one of the better reasons these plots are likely spurious is that ordinary least squares regression isn't exactly suited for count data or time series modeling. Maybe violating a few assumptions here. Just from looking at the plot, I can imagine a pretty large value on the residuals; the intercept appears biased towards values near the origin.

To do:

- Poisson Regression
- 3-point moving average for days without counts? 
- Time Series decompostions

**_be back after the Poisson chapter ~_**

## Works Cited

_in progress_

- Gwern Branwen, Nicolas Christin, David Décary-Hétu, Rasmus Munksgaard Andersen, StExo, El Presidente, Anonymous, Daryl Lau, Sohhlz, Delyan Kratunov, Vince Cakic, Van Buskirk, & Whom. “Dark Net Market archives, 2011-2015”, 12 July 2015. Web. [May 2016](www.gwern.net/Black-market%20archives)

- Demant, Munksgaard, & Houborg 2016, “Personal use, social supply or redistribution? cryptomarket demand on Silk Road 2 and Agora” [August 2016](http://www.gwern.net/docs/sr/2016-demant.pdf)

- [“Darknet Market [Evolution] Basket Analysis”](http://ryancompton.net/2015/03/24/darknet-market-basket-analysis/), Ryan Compton

- ["Social Physics: How Good Ideas Spread"](https://youtu.be/HMBl0ttu-Ow), Alex Pentland. YouTube. Talks at Google, 2014. Web. 07 Sept. 2016.

- Buchanan, Mark. ["Wealth Happens"](https://hbr.org/2002/04/wealth-happens). hbr.org. Harvard Business Review, Apr. 2002. Web. May 2016.
