# Agora Marketplace Analysis

_student work in R_

- [the Problem](#the-problem)
- [the Data](#the-data)
- [current strategy](#current-strategy)
- [choosing agora](#choosing-agora)
- [some images](#images)

- [scrape cleanse / html parsing](parse-html/readme.md)
- [grams](grams) - archive of exploratory analysis on the 'Grams' dataset.

## the Problem

_Who is your client and why do they care about this problem? In other words, what will your client DO or DECIDE based on your analysis that they wouldn’t have otherwise?_

My client is Amazon.com. 

Their currently-in-development Medical Marijuana, Research Chemical, and Counterfeit Outerwear departments are lacking in street market values and potential consumer behaviors due to the previously illegal nature of the products to be offered. 

They want to develop a rough model of the type of person who is interested in such things, so they can get a headstart tranining their recommender systems for a seamless transition when President ToBe Determined signs into law a bill broadening the definition of 'legal'. 

The client will decide, based on this analysis, the necessary scale for their newly legal departments, and they'll also decide if it is worth aggregating these darknet vendors into their network. By conducting a market basket analysis of vendor listings, Amazon will discover which vendors cluster into 'hit' products and which might reside in the 'long tail' of their ecommerce network. 

## the Data

Agora was a referral-based darknet market that rose to prominence after the demise of Silk Road 2 in 2013. 

The data was acquired via gwern's [black market archives](http://www.gwern.net/Black-market%20archives#grams); specifically from the torrent/magnet link. Comprising this archive are weekly crawls of multiple anonymous marketplaces on the darknet - well-trafficked and documented sites such as Silk Road and Evolution, as well as smaller, more ephemeral markets.

For Agora specifically, the crawl dates begin on 2014-01-01 and end on 2015-07-07. There are 206 daily crawls total, occurring weekly and occassionally more frequently.

Because of the scale of gwern's data harvest, it can't be posted here. But to get an idea of what the data looks like, please do refer to the [parse-html](parse-html/readme.md) folder.

## current strategy

The ultimate goal will be to conduct a Market Basket Analysis; to create associate rules given the products and services available in the data. While some transaction data is available, in much more abundace is vendor data. Given this, it will likely be that associate rules will be created from vendor listings - i.e. "vendors who sell this, also sell that". 

There are many ways to categorize illegal substances - [DEA Scheduling](https://www.dea.gov/druginfo/ds.shtml) comes to mind. With this particular data, we can possibly derive another classification scheme using association rules on vendor listings - a look at what products (drugs) tend to cluster together from a supply-side perspective. 

## choosing Agora

Agora was chosen as a market to analyze because of it's immense popularity and high usage. Additionally, the conditions of Agora's shutdown were uniqute to me in that the admins voluntarily shut it down after a [paper was published in August 2015](https://www.usenix.org/system/files/conference/usenixsecurity15/sec15-paper-kwon.pdf) that threatened to de-anonymize Tor users. This is in contrast to other markets of similar scale; Silk Road's demise at the hands of law enforcement; [Evolution](https://www.deepdotweb.com/2015/03/18/evolution-marketplace-exit-scam-biggest-exist-scam-ever/) and [Sheep](https://www.deepdotweb.com/2013/11/30/sheep-marketplace-scammed-over-40000000-in-the-biggets-darknet-scam-ever/) turning out to be massive exit-scams.

It's a stretch to say that Agora's administrators were completely altruisitic in their voluntary shutdown; but such protections of themselves and their clients suggests that conducting business in a professional manner was a priority.

## images

Agora Marketplace index page on July 7th, 2015:

![2015-07-07-index](vis/index-2015-07-07.jpg)
