# Association Rule Mining - Basics

_(please [skip](AgAssociationRules-01-prep.md) if familiar - i'm practicing my explaining skills.)_

Let's say you have a set of items that could be on their own, or in groups. (and also pairs)

Sometimes you go to the store just to get **_the one thing_** missing for dinner that night; other times you need to get **_all sorts of things_** to make dinner for the whole week, and feed the cat. Regardless, a **transacation** takes place once you've purchased your **_one_** or **_several things_** and left the store. 

So the store pays it's taxes and keeps records of all these transactions - daily, weekly, monthly, quarterly, annually. The store also needs these transaction records to balance it's budget, do payroll, and so on so forth.

Another thing the store could do is study these transactions to learn about novel combinations - sets of items - that customers prefer might prefer, or not even be aware of. They could even study these transactions over different intervals of interest - say, before a national eating holiday or impending natural disaster. This is, in essense, a simple way to think about the motivations and process behind **Market Basket Analysis** and **Association Rule Mining**.

## Itemset Example

From this simple table of transactions - resembling a market basket - we can see relationships between itemsets. These relationships are what form **_frequent itemsets_** and **_Association Rules_**.

```
TID    Items
1  |  {Bread, Milk}
2  |  {Bread, Diapers, Beer, arules paper, Eggs}
3  |  {Thing for Dinner, Diapers, Beer, arules paper}
4  |  {Butter, Catnip, Beer, arules paper}
5  |  {Diapers, ANOVA paper, Beer, Milk}
6  |  {Bread, Diapers, Semicolon, Gluten Starch, Beer}
7  |  {Nail Polish, Diapers, Beer, arules paper}
```

We can form a **rule** with an **_antecedent_** (left-hand side) and **_consequent_** (right-hand side) from observing which items appear frequently together. A rule for this example can be expressed as:

		{Diapers, Beer_ => {arules paper}

The rule suggests a relationship between the pairing of Diapers and Beer, and papers on Association Rules. By looking at measures of quality, we can test the strength of this rule - or dismiss it altogher. 

## Measures of Quality

The nice thing about association rules is that three main measures of quality for the rules also express themselves as probabilities. One set of items implies another set or single item.

`Support` for **(_{Diapers, Beer} => {arules paper}_)** would be a measure of the total number of times **_{Diapers, Beer}_** and **_{arules paper}_** appeared in transactions together, divided by the total number of transactions in the population. From the above list, this transactions occurs 3 times out of a total 7 possible - for a support of 3/7 = 0.43.

`Confidence` gives an estimate for the conditional probability of an **_{arules paper}_** occuring in an itemset that also containes **_{Diapers, Beer}_**. It also expresses the strength of inference made by a rule - the higher the value, the more likely an **_{arules paper}_** is to appear in transactions with **_{Diapers, Beer}_**. Above we can again see 3 instances of this, out of a total 5 transactions that contain at least **{_Diapers, Beer_}**.

- support**({Diapers, Beer} => {arules paper})**         	= 3/7 = **0.43**
- confidence**({_Diapers, Beer_} => {_arules paper_})**  	= 3/5 = **0.60**
- lift**({_Diapers, Beer_} => {_arules paper_})**        	= (3)(7)/(5)(5) = 21/25 = **0.84**

Another way to say it:

Given a set of transactions where `N` is a number of transactions and `x` and `y` are particular itemsets - calulations for 3 common measures of rule quality:

- Support:    **N<sub>x</sub> / N**
- Confidence: **N<sub>xy</sub> / N<sub>x</sub>**
- Lift:       **N<sub>xy</sub> * N / N<sub>x</sub> * N<sub>y</sub>**

## References

- Hahsler, Michael, Bettina Grün, and Kurt Hornik. "Arules - A Computational Environment for Mining Association Rules and Frequent Item Sets." Journal of Statistical Software J. Stat. Soft. 14.15 (2005): n. pag. [Web](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0ahUKEwjG1oDFxqrPAhUE64MKHW0yA30QFggmMAA&url=https%3A%2F%2Fwww.jstatsoft.org%2Farticle%2Fview%2Fv014i15%2Fv14i15.pdf&usg=AFQjCNG3aCjcy7O3mvHx2byove-2DTLTRw&sig2=fhgTMeA9DKm16v575FdbWg).

-  Tan, Pang-Ning; Michael, Steinbach; Kumar, Vipin (2005). "Chapter 6. Association Analysis: Basic Concepts and Algorithms" (PDF). Introduction to Data Mining. Addison-Wesley. ISBN 0-321-32136-7. [Sept 2016](https://www-users.cs.umn.edu/~kumar/dmbook/ch6.pdf)


- Borne, Dr. Kirk. "Association Rule Mining – Not Your Typical Data Science Algorithm | MapR." Association Rule Mining – Not Your Typical Data Science Algorithm | MapR. N.p., 2014. Web. 25 [Sept. 2016](https://www.mapr.com/blog/association-rule-mining-not-your-typical-data-science-algorithm).