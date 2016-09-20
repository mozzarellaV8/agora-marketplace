# Poisson Regression - Weekly Listings

One of issues with darknet markets is that they're in the dark. Answering some basic questions might shed some light. **How large are these markets?** and, if left unchecked, **How large could they be?**

What follows are basic attempts at models to answer these questions.

- ["How much is there?"](#how-much-is-there)
- ["How much could there be?"](#how-much-could-there-be)

I decided to try a Poisson Regression on weekly count data, after having a feeling that monthly count data would be far too general and and daily count data far too specific. 

A weekly interval seemed a good compromise. 

# How much is there?

``` {r}
library(data.table)
library(sandwich)
library(ggplot2)
library(dplyr)
library(vcd)
library(zoo)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)
```

This data represents the entirety of Agora raw data available. Altogether there are 2322961 observations of 18 variables. To make my life a bit simpler, I decided to split the gathering the count data into two `for()` loops by year. 

``` {R}
# convert to numeric
agora$day <- as.numeric(agora$day)
agora$month <- as.numeric(agora$month)

# loop over months and count rows - 2014 ------------------

`lambda` <- data.frame()

for (i in 1:12) {
  
  w1 <- nrow(subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 7))
  
  w2 <- nrow(subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 14 & agora$day > 7))
  
  w3 <- nrow(subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 21 & agora$day > 14))
  
  w4 <- nrow(subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 31 & agora$day > 21))
  
  wLog <- cbind(w = i, w1 = w1, w2 = w2, w3 = w3, w4 = w4)
  w14 <- rbind(w14, wLog)
}
```

After getting weekly count data with `nrow()`, some quick stacking and cleansing:

``` {R}
# stack counts + sort chronologically
w14 <- stack(w14, select = c(w1, w2, w3, w4))
w14$month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
w2$m.w <- paste("2014", w14$month, w14$ind, sep = "-")
colnames(w14) <- c("count", "week", "month", "mw")
w14 <- w14[c(4, 1, 2, 3)]
w14 <- w14[order(w14$mw, decreasing = F), ]
rownames(w14) <- NULL
```

I repeated this process for 2015 data, and then bound `w14` and `w15` together:

``` {R}
weekly <- rbind(w14, w15)
weekly$mw <- gsub("\\bw1\\b", "07", weekly$mw)
weekly$mw <- gsub("\\bw2\\b", "14", weekly$mw)
weekly$mw <- gsub("\\bw3\\b", "21", weekly$mw)
weekly$mw <- gsub("\\bw4\\b", "28", weekly$mw)
```

# How much could there be?



