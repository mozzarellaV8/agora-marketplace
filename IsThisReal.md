
# Is This Real?

Using data extracted from one year's worth of listings on anonymous marketplace Agora, we'll seek to answer the following:

- Is this real?
- What am I buying?? 
- How much does it cost, and how much of it for sale?
- Should I smoke this?


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
	* validation set: 	Silk Road 2, Evolution, Alphabay, &c.

4. issues
	* dichotomizing a dependent variable has numerous issues.
	* real/placeholer/scam listings



# Independent Variables

### feedback

Feedback on Agora is implemented on a 0-5 out of 5 scale, with comments. Beyond the provided scale, there's also the option to not leave feedback at all - making 7 possibilities total. Of these 7, the distribution of values is divided roughly in half between 5/5 ('great') and no feedback at all ('none').

There's pressure in listings from some vendors for clients to leave 5/5 feedback - one vendor says leaving anything else as feedback will result in blacklisting said client. But - the vendor also says to contact if there are any issues before leaving a negative feedback. (3dames). 

### Vendor

Some have been on multiple anonymous markets; some are new; some are probably scams.

Vendors each have a profile page, which can contain barebones information or detailed shipping info, ordering guidelines, and privacy policies. 

### Location

The source of the product, as well as the destinations. 

### Price

How much the product costs. How does this compare with 'street' prices for the same product - and general availability of the product?

### Number of Listings

This count can be be made by treating listings as factors.

### Payment Method

Can be found in the listing or in the vendor page. The two options are Finalize Early (FE) or have the market hold funds in escrow until the transaction is complete (product delivered). 

Generally, a buyer will assume more risk with FE, because they are releasing funds to a vendor before receiving product. With a large enough transaction, a vendor could simply disappear with the funds. 

There is some risk for vendors to use the escrow system. This risk is generally high when the price of Bitcoin is volatile - a deal may be finalized at a set price, but by the time product has been shipped and funds released from escrow - the value of Bitcoin may have gone down or up considerably.

# Dependent Variable - Quality

How will this be measured? 

From the lecture we saw an example which built a logistic model to predict the quality of health care given a set of independent variables. In this example, the result was dichotomized to be '1 = poor care' and '0 = good care'. While simple and easy to interpret, this may oversimplify a complex situation...how does this model account for log odds that are calculated to be 0.49 or 0.52? 


# idea runoff

- how do you get people to care? phrase the question in the form a business problem. 
- if you start a business, consider if the name can eventually become a verb.
- predicting cannabis quality on anonymous marketplaces
- 'Should I Smoke This? A Cannabis Analysis on Anonymous Marketplaces'
- I like how with 'counterfeits' are straightforward as a category - not scamming or selling false hope. 
- How large will an anonymous market grow before cannabis is legalized across more than 1/3 of US states? 
- on clearnet markets, people tend to write reviews to complain. on an anonymous market, people go on to verify a transaction actually legitimate. 




	
