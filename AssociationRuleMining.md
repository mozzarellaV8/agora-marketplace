# Association Rule Mining



Let's say you have a set of items that could be on their own, or in groups. (and also pairs)

Sometimes you go to the store just to get _the one thing_ missing for dinner that night; other times you need to get _all sorts of things_ to make dinner for the whole week, and feed the cat. Regardless, a **transacation** takes place once you've purchased your _one_ or _several things_ and left the store. 

So the store pays it's taxes and keeps records of all these transactions - daily, weekly, monthtly, seasonally, annually. Of course the store also needs these transaction records to balance it's budget, do payroll, and so on so forth. Another thing the store could do is study these transactions to learn about novel combinations - sets of items - that customers prefer might prefer, or not even be aware of. They could even study these transactions over different intervals of interest - say, before a national eating holiday or impending natural disaster. This is, in essense, a simple way to think about the motivations and process behind Market Basket Analysis and Association Rule Mining.


# List of Items; List of Itemsets; Frequent Itemsets; Association Rule Mining


Transaction ID |  Bread | Milk | Diapers | Butter | Beer | arules paper | Thing for Dinner | Banana
-------------- | -------:------:---------:--------:------:--------------:------------------:--------
	1	   	   |	1		0		0		 1 		 1			1				0				0
	2	       |	0		0		1		 0 		 1			1				0  				1	
	3	       |	0		0		1		 0		 1			1				0				1
	4	       |	0		0		1		 0		 1			1				0				0
	5	       |	0		0		1		 0		 1			1				0				0
	6	       |	1		1		1		 0		 0			0				1				1
	7	       |	0		0		1		 0		 1			1				0				0
	8	       |	0		0		1		 0		 1			1				0				0



# Measures of Quality and Interest

There are many enlightening, confusing, and oversimpflified ways of explaining the three popular measures of quality for deciding to keep an association rule. 

Support

Confidence

Lift


