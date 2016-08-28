# {Agora Operational Lifetime} ∈ {Bitcoin Price History}

To put exchange rate trends in context - Agora lives in a subset of Bitcoin's lifetime. Bitcoin list prices will change (fact-check) as the Bitcoin-USD exchange rate changes. Does this have an effect on vendor or client behaviour on anonymous marketplaces? Would a client forego their order in the midst of a strong rally? 

![annotated plot image]()

The Bitcoin price index from its birth through August 2016.

_full disclosure: I'm not an economist_

The union of Agora's operational lifetime and it's place in Bitcoin's history provides a uniquely volatile overlap. 


``` {r}
# Bitcoin Lifetime with trends ------------------------------------------------

# plot entire Bitcoin lifetime
par(mar = c(6, 6, 6, 6), family = "FranklinGothicSSK")
plot(bpi$Date, bpi$Price, col = "#00000075",
     main = "Bitcoin Price Index (USD) 2010-07-18 :: 2016-08-02")
abline(a = 526.9241, b = 0, lty = 2, col = "#FF000075")
rug(bpi$Date, ticksize = 0.025, lwd = 0.1, col = "#000000")
```