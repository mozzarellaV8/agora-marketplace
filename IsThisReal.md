
# Is This Real?

Using data extracted from one year's worth of listings on anonymous marketplace Agora, we'll seek to answer the following:

- Is this real?
- What am I buying?? 
- How much does it cost, and how much of it for sale?
- Should I smoke this?


# idea runoff

- how do you get people to care? phrase the question in the form a business problem. 
- if you start a business, consider if the name can eventually become a verb.
- predicting cannabis quality on anonymous marketplaces
- 'Should I Smoke This? A Cannabis Analysis on Anonymous Marketplaces'
- I like how with 'counterfeits' are straightforward as a category - not scamming or selling false hope. 
- How large will an anonymous market grow before cannabis is legalized across more than 1/3 of US states? 
- on clearnet markets, people tend to write reviews to complain. on an anonymous market, people go on to verify a transaction actually legitimate. 


# Should I Smoke This?

Predicting the quality of cannabis on anonymous marketplaces using logistic regression.

	log(odds)=β0+β1∗x1+...+βn∗xn

	log(odds)=ln[p/(1−p)]


1. dependent
	* quality

2. independent
	* feedback (none, 0, 1, 2, 3, 4, 5)
	* location
	* price
	* vendors
	* number of listings (check independence for repeat listings)
	* payment method: finalize early (client assumes risk) vs. escrow (vendor assumes risk)
	* bitcoin price index - up or down? (5 day sample?)
	* bitcoin price index - volatile  / stable

3. data
	* training set: 	Agora Marketplace 2014 listings
	* test set: 		Agora Marketplace 2015 listings
	* validation set: 	Silk Road 2, Evolution, Alphabay, et. al

4. issues
	* dichotomizing a dependent variable has numerous issues.
	* real/placeholer/scam listings

# Independent Variables

#### feedback - 

Feedback on Agora is implemented on a 0-5 out of 5 scale, with comments. Beyond the provided scale, there's also the option to not leave feedback at all - making 7 possibilities total. Of these 7, the distribution of values is divided roughly in half between 5/5 ('great') and no feedback at all ('none').

There's pressure in listings from some vendors for clients to leave 5/5 feedback - one vendor says leaving anything else as feedback will result in blacklisting said client. But - the vendor also says to contact if there are any issues before leaving a negative feedback. (3dames). 


#### Vendor

Some have been on multiple anonymous markets; some are new; some are probably scams.




	
