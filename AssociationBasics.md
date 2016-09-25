# Association Rule Mining - Basics

_(please skip if familiar - i'm practicing my 'explaining basics' skills.)_

Let's say you have a set of items that could be on their own, or in groups. (and also pairs)

Sometimes you go to the store just to get _the one thing_ missing for dinner that night; other times you need to get _all sorts of things_ to make dinner for the whole week, and feed the cat. Regardless, a **transacation** takes place once you've purchased your _one_ or _several things_ and left the store. 

So the store pays it's taxes and keeps records of all these transactions - daily, weekly, monthly, quarterly, annually. The store also needs these transaction records to balance it's budget, do payroll, and so on so forth.

Another thing the store could do is study these transactions to learn about novel combinations - sets of items - that customers prefer might prefer, or not even be aware of. They could even study these transactions over different intervals of interest - say, before a National Eating Holiday or impending natural disaster. This is, in essense, a simple way to think about the motivations and process behind Market Basket Analysis and Association Rule Mining.

## Itemset Example

From this simple table of transactions - resembling a market basket - we can see relationships between itemsets. These relationships are what form _frequent itemsets_ and **Association Rules**.

-  TID    Items
-   1  |  {Bread, Milk}
-   2  |  {Bread, Diapers, Beer, arules paper, Eggs}
-   3  |  {Thing for Dinner, Diapers, Beer, arules paper}
-   4  |  {Butter, Catnip, Beer, arules paper}
-   5  |  {Diapers, ANOVA paper, Beer, Milk}
-   6  |  {Bread, Diapers, Semicolon, Gluten Starch, Beer}
-   7  |  {Nail Polish, Diapers, Beer, arules paper}

We can form a **rule** with an _antecedent_ (left-hand side) and _consequent_ (right-hand side) from observing which items appear frequently together. A rule for this example can be expressed as:

		{_Diapers, Beer_} => {_arules paper_}

The rule suggests a relationship between the pairing of Diapers and Beer, and papers on Association Rules. By taking measures of quality, we can test the strength of this rule - or dismiss it as spurious. 

## Measures of Quality

The nice thing about association rules is that three main measures of quality for the rules also express themselves as probabilities. One set of items implies another set of items.

Support(_{Diapers, Beer} => {arules paper}_) would be a measure of the total number of times _{Diapers, Beer}_ and _{arules paper}_ appeared in transactions together, divided by the total number of transactions in the population. From the above list, this transactions occurs 3 times out of a total 7 possible - for a support of 3/7 = 0.43.

Confidence gives an estimate for the conditional probability of an _{arules paper}_  occuring in an itemset that also containes _{Diapers, Beer}_. It also expresses the strength of inference made by a rule - the higher the value (as close to 1 as possible), the most likely an _{arules paper}_ is to appear in transactions with _{Diapers, Beer}_. Above we can again see 3 instances of this, out of a total 5 transactions that contain at least {_Diapers, Beer_}.

- support({Diapers, Beer} => {arules paper})          = 3/7 = 0.43
- confidence({_Diapers, Beer_} => {_arules paper_})   = 3/5 = 0.60
- lift({_Diapers, Beer_} => {_arules paper_})         = (3)(7)/(5)(5) = 21/25 = 0.84

Another way to say it:

Given a set of transactions where `N` is a number of transactions and `x` and `y` are particular itemsets - calulations for 3 common measures of rule quality:

- Support:    Nx / N
- Confidence: Nxy / Nx
- Lift:       Nxy * N / Nx * Ny