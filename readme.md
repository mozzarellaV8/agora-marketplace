# Agora Marketplace Analysis

_student work in R: a proposal_

#### contents

- [the problem](agora-marketplace.md#what-is-the-problem-you-want-to-solve)
- [the client](agora-marketplace.md#who-is-your-client-and-why-do-they-care-about-this-problem)
- [the data](agora-marketplace.md#what-data-are-you-going-to-use-for-this)
- [the approach](agora-marketplace.md#in-brief-outline-your-approach-to-solving-this-problem)
- [deliverables](agora-marketplace.md#what-are-your-deliverables)
- [appendix](agora-marketplace.md#appendix)

#### What is the problem you want to solve?

The problem is Darknet Markets and the darknet in general gain attention for novel, sensational reasons. DNMs are still relatively young, and in the rush to prosecute dealers or persecute whistleblowers - maybe we haven't had a chance to look at why these black markets exist in the first place.

My hope is to find trends that might adapt into general features of darknet markets - giving consideration to the market creators', buyers', and sellers' perspectives. How are darknet markets - aside from the types of product on offer -  different from clearnet eCommerce institutions? Could DNM products find a place on clearnet eComm sites in the long tail business model?


#### Who is your client and why do they care about this problem?

_What will your client DO or DECIDE based on your analysis that they wouldn’t have otherwise?_ 

My client is Amazon.com. 

Their currently-in-development Medical Marijuana, Research Chemical, and Counterfeit Outerwear departments are lacking in street market values and potential consumer behaviors due to the previously illegal nature of the products to be offered. 

They want to develop a rough model of the type of person who is interested in such things, so they can get a headstart tranining their predictors and classifiers for a seamless transition when President ToBe Determined signs into law a bill broadening the definition of 'legal'. 

The client will decide, based on this analysis, the necessary scale for their newly legal departments. They'll also begin to implement these products into their affinity network: i.e. "Customers Who Bought This Item Also Bought...". We will find out if Customer X who favors Purple Kush and Moon Rocks might one day want to try 5-Meo-DMT, or if she likes to wear Nike Free Runs. 


#### What data are you going to use for this?

_and how are you planning on getting it?_

[Black Market Archives](https://www.gwern.net/Black-market%20archives), particularly 13 months of the daily listings on Agora Marketplace. These were provided to Gwern by Grams - a main DNM search engine. 

[Coinbase BTC rates](https://www.coinbase.com/charts?locale=en)

[Investing.com BTC rates](http://www.investing.com/currencies/btc-usd-historical-data)

To make the monetary values more relatable, I also have Bitcoin to USD exchange rate data covering the time period of the market listings data. This data is chosen from Coinbase and Investing.com - one site being dedicated specifically to Bitcoin, and the other looking at Bitcoin as one part of the larger field of economics.


#### In brief, outline your approach to solving this problem.

- Market listing prices

The deck has been put together from 247 daily market listings, and now needs to be shuffled again. 

Originally there were 4,371,382 observations of 8 variables. To that I've added variables for price in USD, daily BTC-USD exhange rate, and logarithms of both BTC and USD list prices. 

Currently the plan is subset this master list into many chunks - hoping to find meaningful trends that might develop into features. The first subsets are of relatable dollar values and breaking down listings by countries shipped from. Will be moving into quantiles and deciles of list prices (along with logarithms) until the mass of data begins to show itself.

- Market forum posts

The aim for this data is a decently working topic model of all the forum posts. So far, I've run the basic text mining preprocessing steps with term-frequency weighting, and have changed parameters a bit each time just to get a feel for the data. 

There are 10,666 text documents total comprising the corpus - covering the time period from December 4th, 2013 until April 20th, 2014. The ultimate goal is to hopefully uncover a 'vocabulary' that might show insight beyond drug popularity and how markets are built. 


#### What are your deliverables?

There will be a GitHub repository with code and cleansed, whole, and subsetted datasets. The aim is for the research to be reproducible. 

Pending mentor advisement, I'd like to be able to deliver code in chunks that others might be able to use as starting points for other analyses. I'd like to also have the data subsetted cleanly into meaningful ranges - again so that others might be able to use them as a starting point. 

I'm interested in something that combines elements of a slide deck and expository paper. As a longtime follower of Tufte, I have a slight bias against large numbers of slides with quick bits of information that lose meaning when taken out of context. Though it may take more time and organization, my ideal would be a 11x17 inch document of 10-20 pages - that could function as PDF, printed booklet, or projection in a conference room. 


#### appendix

**Research**
- [Black Market Risks](http://www.gwern.net/Black-market%20survival) - in-depth study by Gwern, with valuable information on black market creation, lifetimes, and reasons for success/failure. 2013.
- [Recommendation Networks and the Long Tail of Electronic Commerce](http://pages.stern.nyu.edu/~goestrei/LongTail.pdf) - Niche products of Amazon, network affiliations. 2011. 
- [The Stealth of Nations](http://www.amazon.com/Stealth-Nations-Global-Informal-Economy/dp/0307279987) - "Journalist Robert Neuwirth joins globe-trotting Nigerians who sell Chinese cell phones and laid-off San Franciscans who use Twitter to market street food and learns that the people who work in informal economies are entrepreneurs who provide essential services and crucial employment." - why do black markets exist? 2011. 
- [Wealth Happens](https://hbr.org/2002/04/wealth-happens) - Harvard Business Review article on the Pareto Distribution. 2002.
- [Tim O'Reilly: Piracy is Progressive Taxation](http://www.openp2p.com/lpt/a/3015) - an alternative financial model that reconsiders the role of piracy. 2002.


**R - Vis**
- [Tamara Munzer](http://www.cs.ubc.ca/~tmm/vadbook/) Task Taxonomy and Abstraction; Visual Analysis & Design
- [Vialab - Chris Collins](http://vialab.science.uoit.ca/) - text visualization and analysis


**Additional Data Sources**
- [CPI - consumer price index](http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/) - US Dept of Labor Bureau of Labor
- [TMHQ - Trans High Market Quotations](http://www.hightimes.com/read/pot-prices-april-2016-thmq) - monthly, user-submitted, consumer price index for an oz of marijuana. Covers mainly US cities/states.
- [Honeypots - Amazon Web Services](http://datadrivensecurity.info/blog/pages/dds-dataset-collection.html), via datadrivensecurity.com







