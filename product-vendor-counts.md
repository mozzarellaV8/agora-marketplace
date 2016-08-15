# Products and Vendors

_a look by the numbers_

So each page in the product and vendor category corresponds to a single specific product or vendor. Given this, we can use the page number counts to stand in for product/vendor counts.

Reading in total page counts for products and vendors by date as `pv`, we have --

``` r 
sum(pv$p)
# [1] 2467200
sum(pv$vendor)
# [1] 19245
```

-- a total of 2467200 products listed and 19425 vendors active from January 1st 2014 until July 7th 2015. But this number is a bit misleading as it's a raw count, and doesn't take into account pages that were bad crawls(no information) or repeat listings over days (although repeat listings could be used a weight to determine market tenacity, or some such.)

