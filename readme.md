# Agora Marketplace Analysis

_student work in R_

- [the Problem](#the-problem)
- [the Data](#the-data)
- [cleansing the scrapes](scrape-cleanse/readme.md)

## the Problem

_Who is your client and why do they care about this problem? In other words, what will your client DO or DECIDE based on your analysis that they wouldn’t have otherwise?_

My client is Amazon.com. 

Their currently-in-development Medical Marijuana, Research Chemical, and Counterfeit Outerwear departments are lacking in street market values and potential consumer behaviors due to the previously illegal nature of the products to be offered. 

They want to develop a rough model of the type of person who is interested in such things, so they can get a headstart tranining their recommender systems for a seamless transition when President ToBe Determined signs into law a bill broadening the definition of 'legal'. 

The client will decide, based on this analysis, the necessary scale for their newly legal departments, and they'll also decide if it is worth aggregating these darknet vendors into their network. By conducting a market basket analysis of vendor listings, Amazon will discover which vendors cluster into 'hit' products and which might reside in the 'long tail' of their ecommerce network. 

## the Data

Agora was a referral-based darknet market that rose to prominence after the demise of Silk Road 2 in 2013. 

The data was acquired via gwern's [black market archives](http://www.gwern.net/Black-market%20archives#grams); specifically from the torrent/magnet link. Agora was chosen as a market to analyze because of it's immense popularity (high usage). Because of the scale of gwern's data harvest, it can't be posted here. But to get an idea of what the data looks like, please do refer to the [scrape-cleanse](wrangling/readme.md) directory.

