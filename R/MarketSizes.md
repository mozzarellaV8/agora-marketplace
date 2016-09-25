# Market Sizes

The initial questions had been asked, `How large is this market?` and if left unchecked, `How large could it be?`. Using Poisson regression, a simple answer was obtained. In working to validate the model, it was observed that listing counts clustered into "tiers" over time.

Given domain information, it could be inferred these counts were affected by activity in other markets of similar scale - namely, their shutting down. 

At the start of 2014, Agora was one of 3 major darknet markets for vendors to choose from. The other markets were Silk Road 2 and Evolution. Within the timeframe the data covers, both SR2 and Evolution shut down. 


# Poisson Regression

Given the choice between Generalized Boosted Modeling and Piecewise Regression, I've opted for Piecewise. While interested in learning `gbm`, given domain knowledge of the variable clusters I fear the model could be overfit using `gbm`. 

With a piecewise regression, the goal will be to examine how shutdowns of other markets affects listing counts on Agora Market.

Looking at the counts with domain information, 3 "tiers" were observed:

- T1: 0-25,000
- T2: 40,000-75,000
- T3: 75,000 and above

In T1 we have 3 major markets in effect: Agora, Evolution, and Silk Road 2. T2 arises after SR2's shutdown, leaving just Agora and Evolution. T3 is a tier only observed recently after the shutdowns of SR2 and Evolution. It could be inferred that these spikes in listing counts were the result of vendors coming to Agora in the iterim of a shutdown. 