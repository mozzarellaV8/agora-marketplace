Agora Marketplace Analysis
Placeholder Listings

# the Waitlist

Often on Agora there will be products listed at exoribitant prices.

While on the surface they may resemble scams, it's been observed (cit. Econ, other paper) that these prices are here for vendors to keep their listings active while waiting for their supply to be restocked. The prices are set high to 
discourage transactions, but keep their listings active to 'advertise' for the near-future and maintain their market presence.

In some ways, this can be seen as a 'waitlist' for certain products. As opposed to buying a rare or one-of-a-kind item, to maintain a placeholder listing suggests at the very least a perceived demand for the product listed and renewable supply. 

This pursuit arose from trying to determine a price cutoff for what is a placeholder and what is an actual listing - the area becomes grey at the price points between $1000 and xxTBD. At the lower bound of 1000 USD there can be legitimate bulk listings found for MDMA, usually pressed pills in quantities of 50, 100, or 250. 

What becomes problematic is another item such as a kilogram of cocaine has a legitimate street value of $40,000 - and there are listings for such.
within that range, many products of lesser street value can be exist as placeholers, and thus live within the bounds of two legitimate listings.

So - a strategy of single price cutoff point - while convenient - will ultimately rule out legitimate listings or include spurious ones.

## Discoveries while Discretizing Continuous Values

Originally I'd been taking steps towards a market basket analysis of this data, but given the limited range of products on offer I realized it'd be difficult to find novel combinations within itemsets that could counter existing perceptions. But there were discoveries along the way in trying to discretize prices in preparation for the analysis:

Choosing a dollar value upper bound to subset the market data from has been interesting. On my first trial I'd chosen all listings < 3500 BTC just from offhand scrolling throught the dataframe and getting a feel for what's real and what's for sale. 

The case of AusKing gave me pause to write this. Listen on/around 2014-11-08 - 2014-11-12, and more. Here we have a listing for **MDMA PILLS (FISH) - 10,000 pills**. No mention of milligram counts, but listing at/around $95,000 USD. Why did this catch my eye? Because it seemed 'legitimate'; many vendors will put an exorbitant price on their product listings when they're out of stock, so people won't order. While this is generally easy to spot and avoid - it also made AusKing's listing stand out because the price seemed 'reasonable'.

$95,000/10,000 pills works out to $9.50/pill - not bad, half price of street value. But then what do you do with 10,000 MDMA pills? A bit much for personal use. Selling at street price would be nice, but might also take far too long without a distribution network. All told, selling all 10k pills at street value nets $100k - a $5000 return on investment, but far more work than it's worth unless you're already a kingpin in your region. 

# drafting a set of rules

extract: 

product, reasonable price, list price

- MDMA 250x
- MDMA 100x
- MDMA 50x

- Cocaine 1kg

- RCs

- Cannabis 15 grams
- Cannabis 1 oz


