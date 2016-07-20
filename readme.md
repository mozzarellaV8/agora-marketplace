# Agora Marketplace Analysis

_student work in R_


## the Data

Agora was a referral-based darknet market that rose to prominence after the demise of Silk Road 2 in 2013. 

The data was acquired via gwern's [black market archives](http://www.gwern.net/Black-market%20archives#grams); specifically the `Grams` (darknet market search engine) crawls. The data itself is comprised of daily listings of goods/services on offer from vendors, and ranges from the dates 06-29-2014 until 07-12-2015. Some data is missing; likely due to markets going offline while taking measures to avoid law enforcement or hackers. 

Here is a glimpse of the data:

![agora-data](pics/agora-data.png)

- 4,371,382 observations of 9 variables
- hash, Date, btc, usd, rate, ship_from, vendor\_name, name, description


- **Hash**: hash value of listing
- **Date**: date of listing
- **btc** : list price of product/service in Bitcoin
- **usd** : list price of product/service in US Dollar
- **rate**: exchange rate of Bitcoin to Dollar for that particular Date. 
- **ship_from**: where product/service claims to originate from
- **name**: 'headline' of the product/service listing
- **description**: further information of the product/service

## Agora and Darknet Markets

Agora was distinct from other darknet markets such as Silk Road and Evolution in that in word and action they appeared to have their client's best interests in mind. While suffering from recurring downtime issues, they made efforts to change servers and remain as anonymous as possible in the face of threats from LE, hackers, and DDOS attacks. And in contrast to markets such as Evolution, which ended in a large exit scam where the admins absconded with millions of dollars worth of bitcoin still in escrow - Agora voluntarily shut down their services once a [Tor vulnerability was published](https://www.usenix.org/system/files/conference/usenixsecurity15/sec15-paper-kwon.pdf) in July 2015. Rather than continue to operate in the face of a potential risk of deanonymization, the administrators of Agora chose to allow vedors and buyers to cash out before they closed down. 


