# Market Sizes

The initial questions had been asked, **How large is this market?** and if left unchecked, **How large could it be?**. Using Poisson regression, a simple answer was obtained. In working to validate the model, it was observed that listing counts clustered into "tiers" over time.

Given domain information, it could be inferred that count data were affected by activity in other markets of similar scale - namely, other markets shutting down. 

At the start of 2014, Agora was one of 3 major darknet markets for vendors to choose from<sup>1</sup>. The other markets were Silk Road 2 and Evolution. Within the timeframe the data covers, both SR2 and Evolution shut down. 

- [QuasiPoisson Regression](#quasipoisson-regression)
- [External Market Influences](#external-market-influences)
- [Endnotes](#endnotes)

# QuasiPoisson Regression

Facing overdispersion issues in my initial Poisson model on the population of counts, I received a few suggestions from my mentor. Given the choice between Generalized Boosted Modeling and Piecewise Regression, I opted for Piecewise. While interested in learning `gbm`, given domain knowledge of the variable clusters I fear the model could be overfit using `gbm`. Additionally, I recalled that competing darknet market Silk Road 2 shut down in early October 2014. This domain information could correlate to the overdispersion, and observed clustering of count data.

With a piecewise regression, the goal will be to examine how shutdowns of other markets affects listing counts on Agora Market.

Looking at the counts with domain information, 3 "tiers" were observed:

- T1: 0-25,000
- T2: 40,000-75,000
- T3: 75,000 and above

In T1 we have 3 major markets in effect: Agora, Evolution, and Silk Road 2. T2 arises after SR2's shutdown, leaving just Agora and Evolution. T3 is a tier only observed recently after the shutdowns of SR2 and Evolution. It could be inferred that these spikes in listing counts were the result of vendors coming to Agora in the iterim of a shutdown. 

# External Market Influences

Looking at Silk Road 2 and Evolution, and how their shutdowns affected the number of listings on Agora. 

Imagine a global market or network suddenly shutting down. For example, what if Amazon.com suddenly ceased operations over the course of a week? What would traffic on Alibaba/Taobao, eBay, or Rakuten look like? Or, what would Twitter or Instagram traffic look like if Facebook suddenly shut down?<sup>2</sup>

As an abstraction, this is essentially the sort of condition that darknet market Agora faced when rival markets Silk Road 2 and Evolution shut down. How did this affect the size of the market?


## before the Silk Road 2 shutdown

Given the overdispersion issues in the quasi/poisson regression models on weekly count population, I decided to investigate what appeared to be a divide in values from 2014 to 2015. Recalling that Silk Road 2 shut down in early October 2014, I started here.

Why should this be significant? At any given time, several darknet markets may exist. But in practical terms, there generally have been only 2-4 major markets (high usage rate) at play simultaneously. Markets have a tendency to gain traction as usage goes up - trust is established, word of mouth and reliable feedback spread.

It can be seen during the interval of SR2's shutdown (~2014-09-20 through 2014-10-10) that the number of product listings on Agora more than doubles, when it previously never had. The interval of the SR2 shutdown is set wide to account for spread of this knowledge - from LE filing to media reports <sup>3</sup><sup>,</sup><sup>4</sup>.

## after SR2





# Endnotes

<sup>1</sup> The case is actually that there were _many_ more than 3 darknet markets at any given time. But most were not large enough to warrant attention. For this analysis, consideration of 3 markets defined as major would suffice. Support for this can be found by searching [deepdotweb](http://deepdotweb.com) or consulting the appropriate [reddit](http://reddit.com/r/darknetmarkets) forums.

<sup>2</sup>_While not a perfect comparison with the situation faced by Agora, the point I'm trying to make is that if a major resource suddenly disappeared, what effect would that have?_

<sup>3</sup>United States of America vs. Blake Benthall a/k/a "Defcon", [justice.gov](https://www.justice.gov/sites/default/files/usao-sdny/legacy/2015/03/25/Benthall,%20Blake%20Complaint.pdf)

<sup>4</sup>deepdotweb search: [Agora](https://www.deepdotweb.com/?s=Agora)




