# Association Rule Mining

Let's say you have a set of items that could be on their own, or in groups. (and also pairs)

Sometimes you go to the store just to get _the one thing_ missing for dinner that night; other times you need to get _all sorts of things_ to make dinner for the whole week, and feed the cat. Regardless, a **transacation** takes place once you've purchased your _one_ or _several things_ and left the store. 

So the store pays it's taxes and keeps records of all these transactions - daily, weekly, monthtly, seasonally, annually. Of course the store also needs these transaction records to balance it's budget, do payroll, and so on so forth. 

Another thing the store could do is study these transactions to learn about novel combinations - sets of items - that customers prefer might prefer, or not even be aware of. They could even study these transactions over different intervals of interest - say, before a national eating holiday or impending natural disaster. This is, in essense, a simple way to think about the motivations and process behind Market Basket Analysis and Association Rule Mining.

# Itemset Example

From this simple table of transactions - resembling a market basket - we can see relationships between itemsets. These relationships are what form _frequent itemsets_ and **Association Rules**.

-  TID    Items
-   1  |  {Bread, Milk}
-   2  |  {Bread, Diapers, Beer, arules paper, Eggs}
-   3  |  {Thing for Dinner, Diapers, Beer, arules paper}
-   4  |  {Butter, Catnip, Beer, arules paper}
-   5  |  {Diapers, Beer, arules paper, Milk}
-   6  |  {Bread, Milk, Semicolon, Gluten Starch}
-   7  |  {Nail Polish, Diapers, Beer, arules paper}

We can form a rule with an _antecedent_ (left-hand side) and _consequent_ (right-hand side), expressed as:

		{Diapers, Beer} ⟹ {arules paper}

The rule suggests a strong relationship between the pairing of Diapers and Beer and papers on Association Rules. By taking measures of quality, we can test the strength of this rule - or dismiss it as spurious. 

